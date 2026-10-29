/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A stateful consumer of the shared operation CFG. No Python syntax is walked here.

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use pyrefly_static_analysis::Access;
use pyrefly_static_analysis::Arg;
use pyrefly_static_analysis::BinaryOp;
use pyrefly_static_analysis::Binding;
use pyrefly_static_analysis::BuildKind;
use pyrefly_static_analysis::Cx;
use pyrefly_static_analysis::Domain;
use pyrefly_static_analysis::Element;
use pyrefly_static_analysis::Engine;
use pyrefly_static_analysis::Entry;
use pyrefly_static_analysis::ExportProvider;
use pyrefly_static_analysis::ExportTarget;
use pyrefly_static_analysis::ImportItem;
use pyrefly_static_analysis::Limits;
use pyrefly_static_analysis::Literal;
use pyrefly_static_analysis::OpId;
use pyrefly_static_analysis::OpKind;
use pyrefly_static_analysis::Operator;
use pyrefly_static_analysis::ParamKind;
use pyrefly_static_analysis::Program;
use pyrefly_static_analysis::Reg;
use pyrefly_static_analysis::RegionId;
use pyrefly_static_analysis::SymbolId;
use pyrefly_static_analysis::SymbolOrigin;
use pyrefly_static_analysis::Trigger;
use pyrefly_static_analysis::UnaryOp;

use crate::types::Collective;
use crate::types::Compute;
use crate::types::Grad;
use crate::types::Local;
use crate::types::Mesh;
use crate::types::Placement;
use crate::types::Tensor;
use crate::types::compute;
use crate::types::transition;

/// Abstract values stay finite: unequal path values join to Ambiguous. Imported
/// identity flows through the shared frame, so aliases and shadowing are semantic.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Unknown,
    Unbound,
    Ambiguous,
    None,
    Bool(bool),
    /// A trusted Boolean input of unknown truth value, with no truthiness effects.
    Predicate,
    Int(i64),
    Str(String),
    Sequence(Vec<Value>),
    Placement(Placement),
    Tensor(Tensor),
    Mesh(Mesh),
    MeshContext(Mesh),
    Module(String),
    Qualified(String),
    Function(RegionId),
    Metadata,
    MetadataMethod {
        name: String,
        rank: usize,
    },
}

impl Value {
    fn validate_tensors(&self, global: bool) -> Result<(), String> {
        match self {
            Self::Tensor(tensor) => tensor.validate(global),
            Self::Sequence(values) => values
                .iter()
                .try_for_each(|value| value.validate_tensors(global)),
            _ => Ok(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ambient {
    Known(Option<Mesh>),
    Ambiguous,
}

/// Unknown effects permanently invalidate tensor facts for this activation,
/// including aliases and outer bindings. A context never clears this flag.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State {
    pub ambient: Ambient,
    pub contexts: Option<Vec<(OpId, Ambient)>>,
    pub invalidated: bool,
}

impl State {
    pub fn new(mesh: Option<Mesh>) -> Self {
        Self {
            ambient: Ambient::Known(mesh),
            contexts: Some(Vec::new()),
            invalidated: false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diagnostic {
    pub op: Option<OpId>,
    pub code: &'static str,
    pub message: String,
}

/// Entry values and per-operation partial intent are trusted assumptions, not
/// facts inferred from an annotation call. The expected return is checked exactly.
#[derive(Clone, Debug, Default)]
pub struct Contract {
    pub arguments: BTreeMap<String, Value>,
    pub outer: BTreeMap<SymbolId, Value>,
    pub mesh: Option<Mesh>,
    pub returns: Option<Tensor>,
    pub partial_outputs: BTreeMap<OpId, BTreeSet<String>>,
    pub global: bool,
    pub limits: Limits,
}

#[derive(Clone, Debug)]
pub struct Report {
    pub returned: Option<Value>,
    pub diagnostics: Vec<Diagnostic>,
    pub incomplete: bool,
}

impl Report {
    /// This certifies only the supported contract rules under the supplied
    /// assumptions; it is not a Python or distributed-program soundness claim.
    pub fn validated(&self) -> bool {
        !self.incomplete && self.diagnostics.is_empty()
    }
}

pub struct Spmd<'a> {
    pub exports: &'a dyn ExportProvider,
    pub partial_outputs: &'a BTreeMap<OpId, BTreeSet<String>>,
    pub global: bool,
}

impl Spmd<'_> {
    fn fail(
        &self,
        op: OpId,
        cx: &mut Cx<'_, Self>,
        code: &'static str,
        message: impl Into<String>,
    ) {
        cx.report(Diagnostic {
            op: Some(op),
            code,
            message: message.into(),
        });
    }

    fn unsupported(&self, op: OpId, cx: &mut Cx<'_, Self>, message: impl Into<String>) {
        cx.state_mut().invalidated = true;
        self.fail(op, cx, "unsupported", message);
    }

    fn ambient(&self, cx: &Cx<'_, Self>) -> Result<Option<Mesh>, String> {
        if cx.state().invalidated {
            return Err("tensor facts were invalidated by an unsupported effect".into());
        }
        match &cx.state().ambient {
            Ambient::Known(mesh) => Ok(mesh.clone()),
            Ambient::Ambiguous => Err("active mesh is ambiguous across control-flow paths".into()),
        }
    }

    fn compute(
        &self,
        op: OpId,
        kind: Compute,
        values: &[Value],
        cx: &Cx<'_, Self>,
    ) -> Result<Value, String> {
        let ambient = self.ambient(cx)?;
        let mut tensors = Vec::new();
        let template = values.iter().find_map(|v| {
            if let Value::Tensor(t) = v {
                Some(t)
            } else {
                None
            }
        });
        for value in values {
            match value {
                Value::Tensor(tensor) => tensors.push(tensor.clone()),
                Value::Int(_) if matches!(kind, Compute::Add | Compute::Multiply) => {
                    let template = template.ok_or("scalar-only operations are unsupported")?;
                    tensors.push(Tensor::new(
                        template.rank(),
                        Grad::No,
                        template
                            .local()
                            .keys()
                            .map(|axis| (axis.clone(), Placement::Local(Local::R))),
                    )?);
                }
                Value::Ambiguous => {
                    return Err("tensor contract is ambiguous across branches".into());
                }
                _ => return Err("operand has no verified tensor contract".into()),
            }
        }
        let empty = BTreeSet::new();
        let partial = self.partial_outputs.get(&op).unwrap_or(&empty);
        if !partial.is_empty() && !matches!(kind, Compute::Matmul | Compute::Linear) {
            return Err(
                "explicit Partial output intent is supported only for matmul/linear".into(),
            );
        }
        compute(kind, &tensors, ambient.as_ref(), partial, self.global).map(Value::Tensor)
    }

    fn call(
        &self,
        op: OpId,
        function: Value,
        args: &[Arg],
        cx: &mut Cx<'_, Self>,
    ) -> Result<Value, String> {
        let mut positional = Vec::new();
        let mut keywords = BTreeMap::new();
        for arg in args {
            match arg {
                Arg::Positional(reg) => positional.push(cx.reg(*reg).clone()),
                Arg::Keyword(name, reg) => {
                    if keywords
                        .insert(name.as_str(), cx.reg(*reg).clone())
                        .is_some()
                    {
                        return Err("duplicate call keyword".into());
                    }
                }
                _ => {
                    self.unsupported(op, cx, "starred call arguments are unsupported");
                    return Ok(Value::Unknown);
                }
            }
        }
        if self.partial_outputs.contains_key(&op)
            && !matches!(&function, Value::Qualified(name) if matches!(name.as_str(),
                "torch.matmul" | "torch.mm" | "torch.nn.functional.linear"))
        {
            return Err(
                "Partial output contract does not identify a supported matmul/linear".into(),
            );
        }
        if let Value::MetadataMethod { name, rank } = &function {
            let valid = keywords.is_empty()
                && match name.as_str() {
                    "size" | "stride" => {
                        positional.is_empty()
                            || (positional.len() == 1
                                && matches!(&positional[0], Value::Int(dim)
                        if *dim >= -(i64::try_from(*rank).unwrap_or(i64::MAX)) && *dim < *rank as i64))
                    }
                    _ => positional.is_empty(),
                };
            if !valid {
                return Err("unsupported tensor metadata query signature".into());
            }
            return Ok(if matches!(name.as_str(), "dim" | "ndimension") {
                Value::Int(*rank as i64)
            } else {
                Value::Metadata
            });
        }
        let Value::Qualified(name) = function else {
            self.unsupported(op, cx, "callee has no supported qualified identity");
            return Ok(Value::Unknown);
        };
        let compute = match name.as_str() {
            "torch.matmul" | "torch.mm" => Some(Compute::Matmul),
            "torch.nn.functional.linear" => Some(Compute::Linear),
            "torch.relu" | "torch.nn.functional.relu" => Some(Compute::Relu),
            "torch.add" => Some(Compute::Add),
            "torch.mul" => Some(Compute::Multiply),
            _ => None,
        };
        if let Some(kind) = compute {
            // Only these exact signatures are summarized. In particular, `out=`,
            // inplace=True, and nontrivial linear bias must never look pure.
            let count = if kind == Compute::Relu { 1 } else { 2 };
            let bias_none = kind == Compute::Linear
                && positional.len() == 3
                && positional.last() == Some(&Value::None);
            if !keywords.is_empty() || !(positional.len() == count || bias_none) {
                self.unsupported(op, cx, format!("unsupported {name} call signature"));
                return Ok(Value::Unknown);
            }
            return self.compute(op, kind, &positional[..count], cx);
        }
        if matches!(name.as_str(), "spmd_types.S" | "spmd_types.Shard") {
            if keywords.is_empty()
                && positional.len() == 1
                && let Value::Int(dim) = positional[0]
            {
                return Ok(Value::Placement(Placement::Shard(dim)));
            }
            return Err("S(i) needs a literal integer dimension".into());
        }
        if name == "spmd_types.set_current_mesh" {
            if keywords.is_empty()
                && positional.len() == 1
                && let Value::Mesh(mesh) = &positional[0]
            {
                return Ok(Value::MeshContext(mesh.clone()));
            }
            self.unsupported(op, cx, "set_current_mesh requires a trusted Mesh argument");
            return Ok(Value::Unknown);
        }
        if name == "spmd_types.assert_type" {
            self.unsupported(op, cx, "assert_type cannot establish static facts; supply a trusted entry contract and use a checked exit contract");
            return Ok(Value::Unknown);
        }
        let (kind, rules) = match name.as_str() {
            "spmd_types.invariant_to_replicate" => (Collective::InvariantToReplicate, false),
            "spmd_types.convert" | "spmd_types.rules.convert" => {
                (Collective::Convert, name.contains(".rules."))
            }
            "spmd_types.reinterpret" | "spmd_types.rules.reinterpret" => {
                (Collective::Reinterpret, name.contains(".rules."))
            }
            "spmd_types.all_reduce" | "spmd_types.rules.all_reduce" => {
                (Collective::AllReduce, name.contains(".rules."))
            }
            "spmd_types.all_gather" | "spmd_types.rules.all_gather" => {
                (Collective::AllGather, name.contains(".rules."))
            }
            "spmd_types.reduce_scatter" | "spmd_types.rules.reduce_scatter" => {
                (Collective::ReduceScatter, name.contains(".rules."))
            }
            "spmd_types.all_to_all" | "spmd_types.rules.all_to_all" => {
                (Collective::AllToAll, name.contains(".rules."))
            }
            _ => {
                self.unsupported(op, cx, format!("unsupported call {name}"));
                return Ok(Value::Unknown);
            }
        };
        if positional.len() != 2 || keywords.keys().any(|key| !matches!(*key, "src" | "dst")) {
            self.unsupported(op, cx, format!("unsupported {name} call signature"));
            return Ok(Value::Unknown);
        }
        let ambient = self.ambient(cx)?;
        let Value::Tensor(input) = &positional[0] else {
            return Err("collective input has no verified tensor contract".into());
        };
        let Value::Str(axis) = &positional[1] else {
            return Err("collective axis must be an explicit named axis token".into());
        };
        if let Some(mesh) = &ambient
            && (!mesh.axes().contains(axis)
                || input.local().keys().any(|a| !mesh.axes().contains(a))
                || mesh.axes().iter().any(|a| !input.local().contains_key(a)))
        {
            return Err("collective operand and active mesh are incompatible or incomplete".into());
        }
        let default_src = match kind {
            Collective::InvariantToReplicate => Some(Local::I),
            Collective::AllReduce | Collective::ReduceScatter => Some(Local::P),
            Collective::AllGather | Collective::AllToAll => Some(Local::V),
            _ => None,
        };
        let default_dst = match kind {
            Collective::InvariantToReplicate => Some(Local::R),
            Collective::ReduceScatter | Collective::AllToAll => Some(Local::V),
            _ => None,
        };
        let placement = |key: &str, default: Option<Local>| -> Result<Placement, String> {
            match keywords.get(key) {
                Some(Value::Placement(p)) => Ok(*p),
                Some(_) => Err(format!("{key} is not a known SPMD placement")),
                None => default
                    .map(Placement::Local)
                    .ok_or_else(|| format!("missing collective {key}")),
            }
        };
        let mut src = placement("src", default_src)?;
        let dst = placement("dst", default_dst)?;
        if kind == Collective::InvariantToReplicate && !keywords.is_empty() {
            return Err("invariant_to_replicate does not accept src/dst overrides".into());
        }
        // The runtime reduction API interprets V as P even when src defaults to P;
        // the type-only rules API requires the explicit source claim instead.
        if !rules
            && matches!(kind, Collective::AllReduce | Collective::ReduceScatter)
            && src == Placement::Local(Local::P)
            && input.local().get(axis) == Some(&Local::V)
        {
            src = Placement::Local(Local::V);
        }
        transition(input, kind, axis, src, dst, self.global).map(Value::Tensor)
    }
}

impl Domain for Spmd<'_> {
    type Value = Value;
    type State = State;
    type Diagnostic = Diagnostic;

    fn top(&self) -> Value {
        Value::Unknown
    }
    fn unbound(&self) -> Value {
        Value::Unbound
    }

    fn join(&self, a: &Value, b: &Value) -> Value {
        if a == b {
            a.clone()
        } else if matches!(a, Value::Unknown) || matches!(b, Value::Unknown) {
            Value::Unknown
        } else {
            Value::Ambiguous
        }
    }

    fn join_state(&self, a: &State, b: &State) -> State {
        State {
            ambient: if a.ambient == b.ambient {
                a.ambient.clone()
            } else {
                Ambient::Ambiguous
            },
            contexts: if a.contexts == b.contexts {
                a.contexts.clone()
            } else {
                None
            },
            invalidated: a.invalidated || b.invalidated,
        }
    }

    fn transfer(&self, op: OpId, kind: &OpKind, cx: &mut Cx<'_, Self>) {
        match kind {
            OpKind::Name {
                symbol,
                access: Access::Load(dst),
            } => {
                if cx.program().symbol(*symbol).origin == SymbolOrigin::Builtin
                    && cx.program().symbol(*symbol).name.as_str() == "len"
                {
                    // Lexical shadowing changes the symbol, not just its spelling.
                    cx.set_reg(*dst, Value::Qualified("builtins.len".into()));
                } else if matches!(cx.load(*symbol), Value::Unknown | Value::Unbound) {
                    self.unsupported(op, cx, "name has no verified reaching value");
                }
            }
            OpKind::Name { .. } | OpKind::Param { .. } => {}
            OpKind::Literal { value, dst } => {
                let value = match value {
                    Literal::None => Value::None,
                    Literal::Bool(v) => Value::Bool(*v),
                    Literal::Int(Some(v)) => Value::Int(*v),
                    Literal::Str(v) => Value::Str(v.to_string()),
                    _ => Value::Unknown,
                };
                cx.set_reg(*dst, value);
            }
            OpKind::Import { module, item, dst } => {
                let module_text = module.to_string();
                let value = match item {
                    ImportItem::Root => Value::Module(
                        module_text
                            .split('.')
                            .next()
                            .expect("module name has a root")
                            .into(),
                    ),
                    ImportItem::Module => Value::Module(module_text),
                    ImportItem::Name(name) => match self.exports.resolve(*module, name) {
                        ExportTarget::Definition(module, name) => {
                            Value::Qualified(format!("{module}.{name}"))
                        }
                        ExportTarget::Module(module) => Value::Module(module.to_string()),
                        ExportTarget::Unknown => Value::Unknown,
                    },
                };
                cx.set_reg(*dst, normalize_qualified(value));
            }
            OpKind::Attribute {
                object,
                attr,
                access: Access::Load(dst),
            } => {
                let value = match cx.reg(*object).clone() {
                    Value::Module(module) | Value::Qualified(module)
                        if trusted_namespace(&module) =>
                    {
                        normalize_qualified(Value::Qualified(format!("{module}.{attr}")))
                    }
                    Value::Tensor(tensor) => match attr.as_str() {
                        "ndim" => Value::Int(tensor.rank() as i64),
                        "shape" | "dtype" | "device" => Value::Metadata,
                        "size" | "stride" | "dim" | "ndimension" | "storage_offset" => {
                            Value::MetadataMethod {
                                name: attr.to_string(),
                                rank: tensor.rank(),
                            }
                        }
                        _ => {
                            self.unsupported(
                                op,
                                cx,
                                format!("unsupported tensor attribute {attr}"),
                            );
                            Value::Unknown
                        }
                    },
                    _ => {
                        self.unsupported(
                            op,
                            cx,
                            "attribute access may invoke an unsupported protocol",
                        );
                        Value::Unknown
                    }
                };
                cx.set_reg(*dst, value);
            }
            OpKind::Call { func, args, dst } => {
                let function = cx.reg(*func).clone();
                let value = if function == Value::Qualified("builtins.len".into()) {
                    if args.len() == 1
                        && matches!(args[0], Arg::Positional(r) if matches!(cx.reg(r), Value::Tensor(t) if t.rank() > 0))
                    {
                        Ok(Value::Metadata)
                    } else {
                        self.unsupported(
                            op,
                            cx,
                            "len is supported only as a non-scalar tensor metadata query",
                        );
                        Ok(Value::Unknown)
                    }
                } else {
                    self.call(op, function, args, cx)
                };
                match value {
                    Ok(value) => cx.set_reg(*dst, value),
                    Err(message) => {
                        self.fail(op, cx, "contract", message);
                        cx.set_reg(*dst, Value::Unknown);
                    }
                }
            }
            OpKind::Binary {
                op: binary,
                left,
                right,
                dst,
            } => {
                let compute = match binary {
                    BinaryOp::Arith(Operator::Add) => Some(Compute::Add),
                    BinaryOp::Arith(Operator::Mult) => Some(Compute::Multiply),
                    BinaryOp::Arith(Operator::MatMult) => Some(Compute::Matmul),
                    _ => None,
                };
                if let Some(kind) = compute {
                    let values = [cx.reg(*left).clone(), cx.reg(*right).clone()];
                    match self.compute(op, kind, &values, cx) {
                        Ok(value) => cx.set_reg(*dst, value),
                        Err(message) => {
                            self.fail(op, cx, "contract", message);
                            cx.set_reg(*dst, Value::Unknown);
                        }
                    }
                } else {
                    self.unsupported(op, cx, "unsupported binary operator or in-place effect");
                }
            }
            OpKind::Unary {
                op: UnaryOp::USub,
                operand,
                dst,
            } => match cx.reg(*operand) {
                Value::Int(value) => match value.checked_neg() {
                    Some(value) => cx.set_reg(*dst, Value::Int(value)),
                    None => self.unsupported(op, cx, "integer negation overflows the descriptor"),
                },
                _ => self.unsupported(op, cx, "unsupported unary protocol"),
            },
            OpKind::Build {
                kind: BuildKind::Tuple | BuildKind::List | BuildKind::Set,
                elements,
                dst,
            } => {
                if elements.len() <= 32 && elements.iter().all(|e| matches!(e, Element::Value(_))) {
                    let values = elements
                        .iter()
                        .map(|element| match element {
                            Element::Value(reg) => cx.reg(*reg).clone(),
                            _ => unreachable!("collection elements were checked above"),
                        })
                        .collect();
                    cx.set_reg(*dst, Value::Sequence(values));
                } else {
                    self.unsupported(op, cx, "unsupported collection construction");
                }
            }
            OpKind::MakeFunction { region, dst, .. } => {
                cx.set_reg(*dst, Value::Function(*region));
            }
            OpKind::ContextEnter {
                manager,
                dst,
                context,
                is_async: false,
            } => {
                if let Value::MeshContext(mesh) = cx.reg(*manager).clone() {
                    let previous = cx.state().ambient.clone();
                    match &mut cx.state_mut().contexts {
                        Some(contexts) if contexts.len() < 32 => {
                            contexts.push((*context, previous))
                        }
                        _ => {
                            self.unsupported(
                                op,
                                cx,
                                "mesh context stack is ambiguous or exceeds its bound",
                            );
                        }
                    }
                    cx.state_mut().ambient = Ambient::Known(Some(mesh));
                    cx.set_reg(*dst, Value::None);
                } else {
                    cx.state_mut().ambient = Ambient::Ambiguous;
                    self.unsupported(
                        op,
                        cx,
                        "unsupported context manager can mutate tensor contracts",
                    );
                }
            }
            OpKind::ContextExit {
                context,
                is_async: false,
                ..
            } => {
                let restored = cx.state_mut().contexts.as_mut().and_then(Vec::pop);
                match restored {
                    Some((expected, ambient)) if expected == *context => {
                        cx.state_mut().ambient = ambient;
                    }
                    _ => {
                        cx.state_mut().ambient = Ambient::Ambiguous;
                        self.unsupported(op, cx, "mesh context restoration is ambiguous");
                    }
                }
            }
            OpKind::Gap { .. } => {
                cx.state_mut().invalidated = true;
            }
            _ => self.unsupported(op, cx, "operation has no supported SPMD effect summary"),
        }
    }

    fn assume(&self, cond: Reg, truth: bool, cx: &mut Cx<'_, Self>) -> bool {
        match cx.reg(cond) {
            Value::Bool(value) => *value == truth,
            Value::None => !truth,
            Value::Int(value) => (*value != 0) == truth,
            Value::Str(value) => !value.is_empty() == truth,
            Value::Predicate | Value::Metadata => true,
            _ => {
                // An unknown object's truthiness can invoke arbitrary Python code.
                cx.state_mut().invalidated = true;
                true
            }
        }
    }
}

fn trusted_namespace(name: &str) -> bool {
    ["torch", "spmd_types"]
        .iter()
        .any(|root| name == *root || name.starts_with(&format!("{root}.")))
}

fn normalize_qualified(value: Value) -> Value {
    if let Value::Qualified(name) = &value {
        let local = match name.as_str() {
            "spmd_types.R" => Some(Local::R),
            "spmd_types.I" => Some(Local::I),
            "spmd_types.V" => Some(Local::V),
            "spmd_types.P" => Some(Local::P),
            _ => None,
        };
        if let Some(local) = local {
            return Value::Placement(Placement::Local(local));
        }
    }
    value
}

/// Analyze a selected function under trusted entry/call contracts. Module execution
/// is analyzed first to obtain reaching imported identities, not scanned for imports.
/// Nested/deferred helpers are intentionally unsupported at calls in this subset.
pub fn check(
    program: &Program,
    region: RegionId,
    contract: &Contract,
    exports: &dyn ExportProvider,
) -> Report {
    let domain = Spmd {
        exports,
        partial_outputs: &contract.partial_outputs,
        global: contract.global,
    };
    let engine = Engine::new(program, &domain).with_limits(contract.limits);
    let module = engine.analyze(Program::MODULE, Entry::new(State::new(None)));
    let mut entry: Entry<Spmd<'_>> = Entry::new(State::new(contract.mesh.clone()));
    let mut diagnostics = module.diagnostics.clone();
    let mut incomplete =
        module.is_incomplete() || module.exit.as_ref().is_none_or(|f| f.state.invalidated);
    if region == Program::MODULE || region.index() >= program.regions.len() {
        diagnostics.push(Diagnostic {
            op: None,
            code: "entry",
            message: "select a valid function region".into(),
        });
        return Report {
            returned: None,
            diagnostics,
            incomplete: true,
        };
    }
    let function = program.region(region);
    if function.trigger != Trigger::OnCall
        || function
            .params
            .iter()
            .any(|param| matches!(param.kind, ParamKind::VarArgs | ParamKind::KwArgs))
        || function
            .parent
            .is_none_or(|(parent, _)| parent != Program::MODULE)
    {
        diagnostics.push(Diagnostic {
            op: None,
            code: "unsupported",
            message: "only plain module-level function activations are supported".into(),
        });
        incomplete = true;
    }
    for param in &function.params {
        entry.args.push(
            contract
                .arguments
                .get(param.name.as_str())
                .cloned()
                .unwrap_or(Value::Unknown),
        );
    }
    if contract
        .arguments
        .keys()
        .any(|name| !function.params.iter().any(|p| p.name.as_str() == name))
    {
        diagnostics.push(Diagnostic {
            op: None,
            code: "entry",
            message: "entry contract names a nonexistent parameter".into(),
        });
    }
    if let Some(frame) = &module.exit {
        for (index, _) in program.symbols.iter().enumerate() {
            let symbol = SymbolId(index as u32);
            if program.owner(symbol) == Some(Program::MODULE)
                && let Some(binding) = frame.binding(program, symbol)
            {
                entry.outer.insert(symbol, binding.clone());
            }
        }
    }
    for op in contract.partial_outputs.keys() {
        if program.ops.get(op.index()).is_none_or(|operation| {
            operation.region != region
                || !matches!(operation.kind, OpKind::Call { .. } | OpKind::Binary { .. })
        }) {
            diagnostics.push(Diagnostic {
                op: None,
                code: "entry",
                message:
                    "partial output contract must identify a call/binary op in the selected region"
                        .into(),
            });
        }
    }
    for (symbol, value) in &contract.outer {
        if symbol.index() >= program.symbols.len() {
            diagnostics.push(Diagnostic {
                op: None,
                code: "entry",
                message: "outer entry contract names a nonexistent symbol".into(),
            });
        } else {
            entry.outer.insert(*symbol, Binding::bound(value.clone()));
        }
    }
    for value in contract.arguments.values().chain(contract.outer.values()) {
        if let Err(message) = value.validate_tensors(contract.global) {
            diagnostics.push(Diagnostic {
                op: None,
                code: "entry",
                message,
            });
            incomplete = true;
        }
    }
    entry.state.invalidated = incomplete;
    let result = engine.analyze(region, entry);
    incomplete |= result.is_incomplete()
        || result.exit.as_ref().is_none_or(|frame| {
            frame.state.invalidated
                || frame.state.ambient == Ambient::Ambiguous
                || frame.state.contexts.is_none()
        })
        || matches!(
            result.returned,
            Some(Value::Unknown | Value::Ambiguous | Value::Unbound)
        );
    diagnostics.extend(result.diagnostics);
    if let Some(value) = &result.returned
        && let Err(message) = value.validate_tensors(contract.global)
    {
        diagnostics.push(Diagnostic {
            op: None,
            code: "exit",
            message,
        });
        incomplete = true;
    }
    if incomplete {
        diagnostics.push(Diagnostic {
            op: None,
            code: "incomplete",
            message: "reachable gaps, unknown effects/values, or an unconverged/absent normal exit prevent contract validation".into(),
        });
    }
    if let Some(expected) = &contract.returns {
        match &result.returned {
            Some(Value::Tensor(actual)) if actual.matches(expected) => {}
            value => diagnostics.push(Diagnostic {
                op: None,
                code: "exit",
                message: format!("return contract {value:?} does not match expected {expected:?}"),
            }),
        }
    }
    diagnostics.sort();
    diagnostics.dedup();
    Report {
        returned: result.returned,
        diagnostics,
        incomplete,
    }
}
