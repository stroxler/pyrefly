/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Drives the lazy-import domain over a set of modules: module bodies in import order,
//! memoized function summaries for calls, and a scan of every function body for the
//! effects that force a module's imports to run eagerly.

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

use pyrefly_static_analysis::Binding;
use pyrefly_static_analysis::Convergence;
use pyrefly_static_analysis::Engine;
use pyrefly_static_analysis::Entry;
use pyrefly_static_analysis::ExportProvider;
use pyrefly_static_analysis::ExportTarget;
use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::Name;
use pyrefly_static_analysis::Presence;
use pyrefly_static_analysis::Program;
use pyrefly_static_analysis::RaiseCause;
use pyrefly_static_analysis::RegionId;
use pyrefly_static_analysis::RegionResult;
use pyrefly_static_analysis::SymbolId;
use pyrefly_static_analysis::TextRange;
use pyrefly_static_analysis::analyze_python;

use crate::Diagnostic;
use crate::Kind;
use crate::ModuleReport;
use crate::domain::Event;
use crate::domain::Finding;
use crate::domain::LifeguardDomain;
use crate::domain::Mode;
use crate::domain::builtin_binding;
use crate::value::FuncRef;
use crate::value::Origin;
use crate::value::Value;

/// Calls nested deeper than this are reported as unknown.
const MAX_CALL_DEPTH: usize = 32;
/// Re-analyses of a recursive cycle before its summary is reported as unknown.
const MAX_CYCLE_ITERATIONS: usize = 8;

/// The effect of one function activation, as seen by its caller.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Summary {
    /// Import-time effects, each described by its root location and cause.
    pub unsafe_reasons: BTreeSet<String>,
    /// Operations that could not be analyzed.
    pub unknown_reasons: BTreeSet<String>,
    /// Parameter origins (of this or an enclosing activation) whose objects are mutated.
    pub mutated: BTreeSet<Origin>,
    /// Parameter origins that are iterated or awaited.
    pub consumed: BTreeSet<Origin>,
    /// Cells of the caller's symbols that the activation rebinds or deletes, with their
    /// bindings at exit.
    pub writes: Vec<(SymbolId, Binding<Value>)>,
    pub returned: Value,
}

impl Summary {
    fn unknown(reason: &str) -> Self {
        Self {
            unknown_reasons: BTreeSet::from([reason.to_owned()]),
            returned: Value::unknown(),
            ..Self::default()
        }
    }
}

/// A function activation: the body, the module whose import runs it, and the cells
/// (closure cells, globals and builtins) it sees at invocation time.
#[derive(Clone)]
pub struct Activation {
    pub func: FuncRef,
    pub importer: ModuleName,
    pub cells: Vec<(SymbolId, Binding<Value>)>,
}

type Cells = Vec<(SymbolId, Binding<Value>)>;
type SummaryTable = BTreeMap<(FuncRef, ModuleName), Vec<(Cells, Slot)>>;

/// Bookkeeping for a summary under construction.
struct Pending {
    /// The lowest call depth of an in-progress summary that this one depends on.
    lowlink: usize,
    /// True if this summary depends on a call cut off by the depth limit, which makes it
    /// specific to the current call stack.
    truncated: bool,
}

enum Slot {
    /// Being computed at call depth `depth`; recursive calls see `approx`.
    InProgress {
        approx: Summary,
        depth: usize,
        used: bool,
    },
    Done(Summary),
}

struct Module {
    program: Program,
    /// The module globals that some function of the module reads: the globals a call
    /// into the module may observe, directly or through further calls.
    function_globals: Vec<SymbolId>,
}

impl Module {
    fn new(program: Program) -> Self {
        let mut function_globals: Vec<SymbolId> = (1..program.regions.len())
            .flat_map(|r| program.free_symbols(RegionId(r as u32)).iter().copied())
            .filter(|s| program.owner(*s) == Some(Program::MODULE))
            .collect();
        function_globals.sort();
        function_globals.dedup();
        Self {
            program,
            function_globals,
        }
    }
}

struct ModuleOutcome {
    /// Global bindings at the end of the module body; `None` while the body is running.
    globals: Option<BTreeMap<SymbolId, Binding<Value>>>,
    diagnostics: Vec<Diagnostic>,
}

pub struct Analyzer<'a> {
    modules: BTreeMap<ModuleName, Module>,
    exports: &'a dyn ExportProvider,
    outcomes: RefCell<BTreeMap<ModuleName, ModuleOutcome>>,
    /// Summaries by function and importer, then by the cells of the activation.
    summaries: RefCell<SummaryTable>,
    /// The summaries under construction, innermost last.
    pending: RefCell<Vec<Pending>>,
}

impl<'a> Analyzer<'a> {
    pub fn new(modules: &[(ModuleName, String, bool)], exports: &'a dyn ExportProvider) -> Self {
        Self {
            modules: modules
                .iter()
                .map(|(name, source, is_init)| {
                    (*name, Module::new(analyze_python(source, *name, *is_init)))
                })
                .collect(),
            exports,
            outcomes: RefCell::new(BTreeMap::new()),
            summaries: RefCell::new(BTreeMap::new()),
            pending: RefCell::new(Vec::new()),
        }
    }

    pub fn run(&self) -> Vec<ModuleReport> {
        let mut names: Vec<ModuleName> = self.modules.keys().copied().collect();
        names.sort_by(|a, b| a.as_str().cmp(b.as_str()));
        names
            .into_iter()
            .map(|name| {
                self.ensure_module(name);
                let mut diagnostics = self.outcomes.borrow()[&name].diagnostics.clone();
                diagnostics.sort();
                diagnostics.dedup();
                ModuleReport { name, diagnostics }
            })
            .collect()
    }

    pub fn program(&self, module: ModuleName) -> &Program {
        &self.modules[&module].program
    }

    pub fn function_globals(&self, module: ModuleName) -> &[SymbolId] {
        &self.modules[&module].function_globals
    }

    pub fn contains(&self, module: ModuleName) -> bool {
        self.modules.contains_key(&module)
    }

    /// The global values of a project module after its body ran, or `None` if the module
    /// is still running (an import cycle).
    pub fn exit_globals(&self, module: ModuleName) -> Option<BTreeMap<SymbolId, Binding<Value>>> {
        self.ensure_module(module);
        self.outcomes.borrow()[&module].globals.clone()
    }

    /// The value of `module.attr` for a project module: a global, else a submodule.
    pub fn module_attr(&self, module: ModuleName, attr: &Name) -> Value {
        let program = self.program(module);
        if let Some(symbol) = program.global(attr.as_str())
            && let Some(globals) = self.exit_globals(module)
        {
            let binding = &globals[&symbol];
            return match binding.presence {
                Presence::Bound => binding.value.clone(),
                Presence::Maybe => binding.value.join(&Value::unknown()),
                Presence::Unbound => Value::unknown(),
            };
        }
        let submodule = module.append(attr);
        if self.contains(submodule) {
            Value::of(Origin::Module(submodule))
        } else {
            Value::unknown()
        }
    }

    /// The value bound by `from module import name`.
    pub fn imported_name(&self, module: ModuleName, name: &Name) -> Value {
        let project_or_external = |m: ModuleName| {
            Value::of(if self.contains(m) {
                Origin::Module(m)
            } else {
                Origin::Imported(m)
            })
        };
        match self.exports.resolve(module, name) {
            ExportTarget::Definition(m, n) if self.contains(m) => self.module_attr(m, &n),
            ExportTarget::Definition(m, n) => project_or_external(m.append(&n)),
            ExportTarget::Module(m) => project_or_external(m),
            ExportTarget::Unknown => Value::unknown(),
        }
    }

    fn ensure_module(&self, module: ModuleName) {
        if self.outcomes.borrow().contains_key(&module) {
            return;
        }
        self.outcomes.borrow_mut().insert(
            module,
            ModuleOutcome {
                globals: None,
                diagnostics: Vec::new(),
            },
        );
        let program = self.program(module);
        let domain = LifeguardDomain::new(self, program, module, Mode::Execute);
        let builtins: Cells = (0..program.symbols.len())
            .filter_map(|i| {
                let s = SymbolId(i as u32);
                builtin_binding(program, s).map(|b| (s, b))
            })
            .collect();
        let mut entry = Entry::new(());
        entry.outer.extend(builtins.iter().cloned());
        let result = Engine::new(program, &domain).analyze(Program::MODULE, entry);
        // A caller that catches the module's exception can still see its globals, so the
        // normal and raising exits are joined. A module with neither exit reachable has
        // unknown globals.
        let unknown = Binding {
            presence: Presence::Maybe,
            value: Value::unknown(),
        };
        let globals: BTreeMap<SymbolId, Binding<Value>> = program
            .module_symbols()
            .map(|s| {
                let ends = [&result.exit, &result.raised];
                let bindings: Vec<&Binding<Value>> = ends
                    .into_iter()
                    .flatten()
                    .map(|f| f.binding(program, s).unwrap_or(&unknown))
                    .collect();
                let binding = bindings
                    .iter()
                    .copied()
                    .cloned()
                    .reduce(|a, b| Binding {
                        presence: if a.presence == b.presence {
                            a.presence
                        } else {
                            Presence::Maybe
                        },
                        value: a.value.join(&b.value),
                    })
                    .unwrap_or_else(|| unknown.clone());
                (s, binding)
            })
            .collect();

        let diagnostic = |range: TextRange, kind: Kind, message: String| {
            let (line, column) = program.line_col(range);
            Diagnostic {
                line: line.get(),
                column: column.get(),
                kind,
                message,
            }
        };
        let mut diagnostics = Vec::new();
        for Finding { start, end, event } in &result.diagnostics {
            let range = TextRange::new((*start).into(), (*end).into());
            diagnostics.push(match event {
                Event::Report { kind, message } => diagnostic(range, *kind, message.clone()),
                // Callers substitute parameters, so none should reach a module body; if
                // one does, the mutation is reported rather than dropped.
                Event::MutatesParam(_) | Event::ConsumesParam(_) => diagnostic(
                    range,
                    Kind::Unsupported,
                    "uses a parameter value that escaped its function".to_owned(),
                ),
            });
        }
        for (range, message) in incompleteness(program, &result) {
            diagnostics.push(diagnostic(range, Kind::Unsupported, message));
        }
        for range in explicit_raises(&result) {
            let message = "import-time code raises an exception".to_owned();
            diagnostics.push(diagnostic(range, Kind::UnhandledRaise, message));
        }
        if let Some(outcome) = self.outcomes.borrow_mut().get_mut(&module) {
            outcome.globals = Some(globals.clone());
        }

        // Function bodies count toward the module only when import-time code calls
        // them, except for the effects that force eager imports wherever they occur.
        let scan = LifeguardDomain::new(self, program, module, Mode::Scan);
        let engine = Engine::new(program, &scan);
        for index in 1..program.regions.len() {
            let region = RegionId(index as u32);
            let mut entry = Entry::new(());
            for s in program.free_symbols(region) {
                let binding = builtin_binding(program, *s).or_else(|| globals.get(s).cloned());
                if let Some(binding) = binding {
                    entry.outer.insert(*s, binding);
                }
            }
            let result = engine.analyze(region, entry);
            for Finding { start, end, event } in result.diagnostics {
                if let Event::Report { kind, message } = event
                    && kind.forces_eager_imports()
                {
                    let range = TextRange::new(start.into(), end.into());
                    diagnostics.push(diagnostic(range, kind, message));
                }
            }
        }
        if let Some(outcome) = self.outcomes.borrow_mut().get_mut(&module) {
            outcome.diagnostics = diagnostics;
        }
    }

    /// The summary of one activation. Recursive cycles are solved by iterating from an
    /// empty summary at the head of each strongly connected component; summaries inside
    /// a cycle are only cached once the head has converged.
    pub fn summarize(&self, activation: Activation) -> Summary {
        let key = (activation.func, activation.importer);
        if let Some(found) = self.find(&activation, |slot| match slot {
            Slot::Done(summary) => summary.clone(),
            Slot::InProgress {
                approx,
                depth,
                used,
            } => {
                *used = true;
                let mut pending = self.pending.borrow_mut();
                let top = pending
                    .last_mut()
                    .expect("an in-progress summary has a caller on the stack");
                top.lowlink = top.lowlink.min(*depth);
                approx.clone()
            }
        }) {
            return found;
        }
        let depth = self.pending.borrow().len();
        if depth >= MAX_CALL_DEPTH {
            if let Some(top) = self.pending.borrow_mut().last_mut() {
                top.truncated = true;
            }
            return Summary::unknown("call depth limit reached");
        }
        self.summaries.borrow_mut().entry(key).or_default().push((
            activation.cells.clone(),
            Slot::InProgress {
                approx: Summary::default(),
                depth,
                used: false,
            },
        ));
        self.pending.borrow_mut().push(Pending {
            lowlink: depth,
            truncated: false,
        });
        let mut iterations = 0;
        loop {
            let mut summary = self.activate(&activation);
            let (lowlink, truncated) = {
                let pending = self.pending.borrow();
                let top = pending.last().expect("pushed above");
                (top.lowlink, top.truncated)
            };
            if lowlink < depth || truncated {
                // An enclosing activation heads this cycle and will recompute us, or the
                // result depends on where the depth limit fell: do not cache it.
                if let Some(slots) = self.summaries.borrow_mut().get_mut(&key) {
                    slots.retain(|(c, _)| *c != activation.cells);
                }
                let mut pending = self.pending.borrow_mut();
                pending.pop();
                if let Some(parent) = pending.last_mut() {
                    parent.lowlink = parent.lowlink.min(lowlink);
                    parent.truncated |= truncated;
                }
                return summary;
            }
            let finished = self
                .find(&activation, |slot| {
                    let Slot::InProgress { approx, used, .. } = slot else {
                        unreachable!("only this activation finishes its own slot")
                    };
                    if *used {
                        summary
                            .unknown_reasons
                            .insert("recursive call cycle; termination is not analyzed".to_owned());
                    }
                    let converged = !*used || summary == *approx;
                    if converged || iterations == MAX_CYCLE_ITERATIONS {
                        if !converged {
                            summary
                                .unknown_reasons
                                .insert("recursive summary did not converge".to_owned());
                        }
                        *slot = Slot::Done(summary.clone());
                        true
                    } else {
                        *approx = summary.clone();
                        *used = false;
                        false
                    }
                })
                .expect("the slot was inserted above");
            if finished {
                self.pending.borrow_mut().pop();
                return summary;
            }
            iterations += 1;
        }
    }

    /// Apply `f` to the slot of `activation`, if there is one.
    fn find<T>(&self, activation: &Activation, f: impl FnOnce(&mut Slot) -> T) -> Option<T> {
        let mut summaries = self.summaries.borrow_mut();
        let slots = summaries.get_mut(&(activation.func, activation.importer))?;
        let (_, slot) = slots.iter_mut().find(|(c, _)| *c == activation.cells)?;
        Some(f(slot))
    }

    /// Run the callee's region with symbolic parameters and the key's cells.
    fn activate(&self, activation: &Activation) -> Summary {
        let Activation {
            func,
            importer,
            cells,
        } = activation;
        let program = self.program(func.module);
        let region = program.region(func.region);
        let domain = LifeguardDomain::new(self, program, *importer, Mode::Execute);
        let mut entry = Entry::new(());
        entry.args = (0..region.params.len())
            .map(|i| Value::of(Origin::Param(*func, i)))
            .collect();
        entry.outer.extend(cells.iter().cloned());
        let result = Engine::new(program, &domain).analyze(func.region, entry);

        let location = |start: u32| {
            let range = TextRange::empty(start.into());
            format!("{}:{}", func.module, program.line_col(range).0)
        };
        let mut summary = Summary::default();
        for Finding { start, event, .. } in &result.diagnostics {
            match event {
                Event::Report { kind, message } => {
                    // Calls already carry the root location of the effect they report.
                    let reason = match kind {
                        Kind::UnsafeCall | Kind::UnknownCall => message.clone(),
                        _ => format!("{}: {message}", location(*start)),
                    };
                    if kind.is_unknown() {
                        summary.unknown_reasons.insert(reason);
                    } else {
                        summary.unsafe_reasons.insert(reason);
                    }
                }
                Event::MutatesParam(param) => {
                    summary.mutated.insert(param.clone());
                }
                Event::ConsumesParam(param) => {
                    summary.consumed.insert(param.clone());
                }
            }
        }
        for (range, message) in incompleteness(program, &result) {
            let start = u32::from(range.start());
            summary
                .unknown_reasons
                .insert(format!("{}: {message}", location(start)));
        }
        for range in explicit_raises(&result) {
            let start = u32::from(range.start());
            summary
                .unsafe_reasons
                .insert(format!("{}: raises an exception", location(start)));
        }
        if let Some(exit) = &result.exit {
            let entry: BTreeMap<&SymbolId, &Binding<Value>> =
                cells.iter().map(|(s, b)| (s, b)).collect();
            summary.writes = exit
                .outer()
                .iter()
                .filter(|(s, v)| entry.get(s) != Some(v))
                .map(|(s, v)| (*s, v.clone()))
                .collect();
        }
        summary.returned = result.returned.unwrap_or_default();
        summary
    }
}

/// Reachable gaps and non-convergence, which make a result incomplete.
fn incompleteness(
    program: &Program,
    result: &RegionResult<LifeguardDomain<'_>>,
) -> Vec<(TextRange, String)> {
    let mut out: Vec<(TextRange, String)> = result
        .gaps
        .iter()
        .map(|(gap, range)| (*range, format!("{gap:?} is not modeled")))
        .collect();
    if result.convergence != Convergence::Converged {
        let range = program.region(result.region).range;
        out.push((range, "the analysis did not converge".to_owned()));
    }
    out
}

/// The reachable `raise` statements of a region. A failing `assert` is not reported,
/// like any other operation that may raise, and a re-raise out of a `try` is covered by
/// the gap that `try` already reports.
fn explicit_raises(result: &RegionResult<LifeguardDomain<'_>>) -> Vec<TextRange> {
    result
        .raise_sites
        .iter()
        .filter(|site| site.cause == RaiseCause::Explicit)
        .map(|site| site.range)
        .collect()
}
