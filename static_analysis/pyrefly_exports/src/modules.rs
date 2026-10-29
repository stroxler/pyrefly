/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Classify module-valued exports using the shared frontend and frame semantics.
//! This is declaration provenance, not a claim that arbitrary module effects are pure.

use std::collections::BTreeSet;
use std::collections::HashMap;

use pyrefly_static_analysis::Access;
use pyrefly_static_analysis::Cx;
use pyrefly_static_analysis::Domain;
use pyrefly_static_analysis::Engine;
use pyrefly_static_analysis::Entry;
use pyrefly_static_analysis::ImportItem;
use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::Name;
use pyrefly_static_analysis::OpId;
use pyrefly_static_analysis::OpKind;
use pyrefly_static_analysis::Program;
use pyrefly_static_analysis::SymbolId;
use pyrefly_static_analysis::analyze_python;

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Module(ModuleName),
    Other,
    Unknown,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct State {
    candidates: BTreeSet<SymbolId>,
    effectful: bool,
}

struct Modules;

impl Domain for Modules {
    type Value = Value;
    type State = State;
    type Diagnostic = ();

    fn top(&self) -> Value {
        Value::Unknown
    }

    fn join(&self, a: &Value, b: &Value) -> Value {
        if a == b { a.clone() } else { Value::Unknown }
    }

    fn join_state(&self, a: &State, b: &State) -> State {
        State {
            candidates: a.candidates.union(&b.candidates).copied().collect(),
            effectful: a.effectful || b.effectful,
        }
    }

    fn transfer(&self, _: OpId, kind: &OpKind, cx: &mut Cx<'_, Self>) {
        match kind {
            OpKind::Import { module, item, dst } => {
                let value = match item {
                    ImportItem::Root => {
                        Value::Module(ModuleName::from_name(&module.first_component()))
                    }
                    ImportItem::Module => Value::Module(*module),
                    ImportItem::Name(_) => Value::Unknown,
                };
                cx.set_reg(*dst, value);
            }
            OpKind::Literal { dst, .. }
            | OpKind::Build { dst, .. }
            | OpKind::MakeFunction { dst, .. } => cx.set_reg(*dst, Value::Other),
            OpKind::Name {
                symbol,
                access: Access::Store(reg),
            } => {
                let is_module = matches!(cx.reg(*reg), Value::Module(_));
                let module_scope = cx.program().region(Program::MODULE).scope;
                if is_module && cx.program().symbol(*symbol).scope == module_scope {
                    cx.state_mut().candidates.insert(*symbol);
                }
            }
            OpKind::Name { .. } | OpKind::Param { .. } => {}
            // Unknown execution can alter the namespace. Do not turn its surviving
            // import-shaped values into a proof of module-valued identity.
            _ => cx.state_mut().effectful = true,
        }
    }
}

/// Known modules, or an explicitly ambiguous module candidate. Other declaration
/// kinds are absent and are resolved by Pyrefly's export table instead.
pub(crate) fn classify(
    source: &str,
    module: ModuleName,
    is_init: bool,
) -> HashMap<Name, Option<ModuleName>> {
    let program = analyze_python(source, module, is_init);
    let result =
        Engine::new(&program, &Modules).analyze(Program::MODULE, Entry::new(State::default()));
    let Some(frame) = result.exit else {
        return HashMap::new();
    };
    let incomplete = result.convergence != pyrefly_static_analysis::Convergence::Converged
        || !result.gaps.is_empty()
        || frame.state.effectful;
    program
        .module_symbols()
        .filter_map(|symbol| {
            if !frame.state.candidates.contains(&symbol) {
                return None;
            }
            let value = frame.symbol(&program, symbol);
            match value {
                Some(Value::Other) => None,
                Some(Value::Module(module)) if !incomplete => {
                    Some((program.symbol(symbol).name.clone(), Some(*module)))
                }
                _ => Some((program.symbol(symbol).name.clone(), None)),
            }
        })
        .collect()
}
