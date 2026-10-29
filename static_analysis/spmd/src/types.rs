/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Pure, finite SPMD descriptors. They describe contracts, not executing tensors.

use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// The backward contract is determined by the forward local contract.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Local {
    R,
    I,
    V,
    P,
}

impl Local {
    pub fn gradient(self) -> Self {
        match self {
            Self::R => Self::P,
            Self::P => Self::R,
            Self::I => Self::I,
            Self::V => Self::V,
        }
    }
}

/// Unknown gradient information never enables a no-gradient relaxation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Grad {
    Yes,
    No,
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Placement {
    Local(Local),
    Shard(i64),
}

impl Placement {
    pub fn local(self) -> Local {
        match self {
            Self::Local(local) => local,
            Self::Shard(_) => Local::V,
        }
    }
}

/// Named, independent axes supplied by a trusted integration. Names do not prove
/// the physical rank layout or orthogonality of actual process groups.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mesh {
    axes: BTreeSet<String>,
}

impl Mesh {
    pub fn new(axes: impl IntoIterator<Item = impl Into<String>>) -> Result<Self, String> {
        let mut result = BTreeSet::new();
        for axis in axes {
            let axis = axis.into();
            if axis.is_empty() || !result.insert(axis) {
                return Err("mesh axes must be nonempty and distinct".into());
            }
        }
        Ok(Self { axes: result })
    }

    pub fn axes(&self) -> &BTreeSet<String> {
        &self.axes
    }
}

/// An ordered list of axes per dimension, outermost first. Construction validates
/// that only V axes occur and that an axis occurs in at most one dimension.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tensor {
    rank: usize,
    grad: Grad,
    local: BTreeMap<String, Local>,
    partition: Vec<Vec<String>>,
}

impl Tensor {
    pub fn new(
        rank: usize,
        grad: Grad,
        axes: impl IntoIterator<Item = (impl Into<String>, Placement)>,
    ) -> Result<Self, String> {
        if rank > 32 {
            return Err("descriptor rank exceeds 32".into());
        }
        let mut result = Self {
            rank,
            grad,
            local: BTreeMap::new(),
            partition: vec![Vec::new(); rank],
        };
        for (axis, placement) in axes {
            let axis = axis.into();
            if axis.is_empty()
                || result
                    .local
                    .insert(axis.clone(), placement.local())
                    .is_some()
            {
                return Err("tensor axes must be nonempty and distinct".into());
            }
            if let Placement::Shard(dim) = placement {
                result.partition[dimension(dim, rank)?].push(axis);
            }
        }
        if result.partition.iter().any(|axes| axes.len() > 1) {
            return Err("multiple axes on one dimension require an ordered PartitionSpec".into());
        }
        Ok(result)
    }

    pub fn with_partition(mut self, partition: Vec<Vec<String>>) -> Result<Self, String> {
        self.partition = partition;
        self.validate(false)?;
        Ok(self)
    }

    /// Check representation invariants independently of an expected return contract.
    /// Local V may omit a shard dimension; global V must occur exactly once in the
    /// partition. Every producer checks this before publishing its result.
    pub fn validate(&self, global: bool) -> Result<(), String> {
        if self.rank > 32 || self.partition.len() != self.rank {
            return Err("PartitionSpec must match a tensor rank of at most 32".into());
        }
        if self.local.keys().any(String::is_empty) {
            return Err("tensor axes must be nonempty".into());
        }
        let mut seen = BTreeSet::new();
        for axis in self.partition.iter().flatten() {
            if self.local.get(axis) != Some(&Local::V) || !seen.insert(axis) {
                return Err("PartitionSpec axes must be V and occur at most once".into());
            }
        }
        if global {
            for (axis, local) in &self.local {
                if *local == Local::V && !seen.contains(axis) {
                    return Err(format!(
                        "global V axis {axis} is missing from PartitionSpec"
                    ));
                }
            }
        }
        Ok(())
    }

    pub fn rank(&self) -> usize {
        self.rank
    }

    pub fn grad(&self) -> Grad {
        self.grad
    }

    pub fn local(&self) -> &BTreeMap<String, Local> {
        &self.local
    }

    pub fn partition(&self) -> &[Vec<String>] {
        &self.partition
    }

    /// Exact forward and gradient contract, including explicit shard ordering.
    pub fn matches(&self, expected: &Self) -> bool {
        self == expected
    }

    fn shard(&self, axis: &str) -> Option<usize> {
        self.partition
            .iter()
            .position(|axes| axes.iter().any(|a| a == axis))
    }

    fn complete(&self, axes: &BTreeSet<String>) -> Result<(), String> {
        for axis in axes {
            if !self.local.contains_key(axis) {
                return Err(format!("tensor is missing active mesh axis {axis}"));
            }
        }
        Ok(())
    }
}

fn dimension(dim: i64, rank: usize) -> Result<usize, String> {
    let rank = i64::try_from(rank).map_err(|_| "tensor rank is too large")?;
    let canonical = if dim < 0 {
        rank.checked_add(dim)
    } else {
        Some(dim)
    };
    match canonical {
        Some(dim) if dim >= 0 && dim < rank => Ok(dim as usize),
        _ => Err(format!("shard dimension {dim} is invalid for rank {rank}")),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Compute {
    Add,
    Multiply,
    Relu,
    Matmul,
    Linear,
}

/// Check local propagation and, optionally, the restricted rank-two sharding rule.
/// A sharded contraction requires explicit `partial_axes` intent; this never
/// inserts a collective or guesses whether a local operation meant a global one.
pub fn compute(
    op: Compute,
    inputs: &[Tensor],
    ambient: Option<&Mesh>,
    partial_axes: &BTreeSet<String>,
    global: bool,
) -> Result<Tensor, String> {
    if !partial_axes.is_empty() && !matches!(op, Compute::Matmul | Compute::Linear) {
        return Err("explicit Partial output intent is supported only for matmul/linear".into());
    }
    let count = if op == Compute::Relu { 1 } else { 2 };
    if inputs.len() != count {
        return Err(format!("{op:?} requires {count} tensor operands"));
    }
    let mut axes = partial_axes.clone();
    for input in inputs {
        axes.extend(input.local.keys().cloned());
    }
    if let Some(mesh) = ambient {
        axes.extend(mesh.axes.iter().cloned());
        if inputs
            .iter()
            .any(|t| t.local.keys().any(|a| !mesh.axes.contains(a)))
        {
            return Err("foreign mesh presentation is unsupported".into());
        }
    }
    for input in inputs {
        input.complete(&axes)?;
    }
    let grad = if inputs.iter().any(|t| t.grad == Grad::Yes) {
        Grad::Yes
    } else if inputs.iter().all(|t| t.grad == Grad::No) {
        Grad::No
    } else {
        Grad::Unknown
    };
    let rank = inputs[0].rank;
    if inputs.iter().any(|t| t.rank != rank) {
        return Err("broadcasting and unequal tensor ranks are unsupported".into());
    }
    if matches!(op, Compute::Matmul | Compute::Linear) && rank != 2 {
        return Err("only rank-two matmul and linear are supported".into());
    }
    let mut output = Tensor {
        rank,
        grad,
        local: BTreeMap::new(),
        partition: vec![Vec::new(); rank],
    };
    for axis in &axes {
        let mut locals: Vec<_> = inputs.iter().map(|t| t.local[axis]).collect();
        // Preserve homogeneous I. Otherwise a known no-grad R/I adopts the
        // gradient-bearing flavor (or R when no operand has a gradient edge).
        if locals.contains(&Local::I) && locals.iter().any(|l| *l != Local::I) {
            let flavor = inputs
                .iter()
                .zip(&locals)
                .find_map(|(t, l)| {
                    (t.grad != Grad::No && matches!(l, Local::I | Local::R)).then_some(*l)
                })
                .unwrap_or(Local::R);
            for (input, local) in inputs.iter().zip(&mut locals) {
                if input.grad == Grad::No && matches!(local, Local::I | Local::R) {
                    *local = flavor;
                }
            }
        }
        let mut local = if locals.iter().all(|l| *l == Local::I) {
            Local::I
        } else if locals.contains(&Local::I) {
            return Err(format!(
                "gradient-bearing I cannot mix with other contracts on {axis}; an I->R entry may be missing"
            ));
        } else if locals.contains(&Local::P) {
            let valid = match op {
                Compute::Add => locals.iter().all(|l| *l == Local::P),
                Compute::Multiply | Compute::Matmul | Compute::Linear => {
                    locals.iter().filter(|l| **l == Local::P).count() == 1
                        && locals.iter().all(|l| matches!(l, Local::P | Local::R))
                }
                Compute::Relu => false,
            };
            if !valid {
                return Err(format!(
                    "{op:?} cannot consume these Partial contracts on {axis}"
                ));
            }
            Local::P
        } else if locals.contains(&Local::V) {
            Local::V
        } else {
            Local::R
        };
        if partial_axes.contains(axis) {
            if !matches!(local, Local::V | Local::P) {
                return Err(format!("explicit partial output requires V or P on {axis}"));
            }
            local = Local::P;
        }
        output.local.insert(axis.clone(), local);
    }
    if !global {
        output.validate(false)?;
        return Ok(output);
    }
    if matches!(op, Compute::Matmul | Compute::Linear) {
        let right_labels = if op == Compute::Linear {
            [1, 2]
        } else {
            [2, 1]
        };
        let labels = [[0, 2], right_labels]; // output labels are [0, 1]; 2 is K.
        for axis in &axes {
            let mut label = None;
            let mut order = None;
            for (input, labels) in inputs.iter().zip(labels) {
                if let Some(dim) = input.shard(axis) {
                    let next = labels[dim];
                    if label.is_some_and(|previous| previous != next) {
                        return Err(format!("mesh axis {axis} shards different matmul labels"));
                    }
                    label = Some(next);
                    let next_order = &input.partition[dim];
                    if order.is_some_and(|previous| previous != next_order) {
                        return Err(format!("shard order differs on matmul label for {axis}"));
                    }
                    order = Some(next_order);
                } else if input.local[axis] == Local::V {
                    return Err(format!(
                        "global matmul needs a shard dimension for V on {axis}"
                    ));
                }
            }
            if let Some(label) = label {
                for (input, labels) in inputs.iter().zip(labels) {
                    if labels.contains(&label) && input.shard(axis).is_none() {
                        return Err(format!("matmul label sharding is inconsistent on {axis}"));
                    }
                }
                if label == 2 {
                    if !partial_axes.contains(axis) {
                        return Err(format!(
                            "sharded contraction on {axis} requires an explicit Partial output contract"
                        ));
                    }
                } else {
                    if partial_axes.contains(axis) {
                        return Err(format!(
                            "free matmul label on {axis} cannot be declared Partial"
                        ));
                    }
                    let order = order.expect("a sharded label records its axis order");
                    if output.partition[label].is_empty() {
                        output.partition[label] = order.clone();
                    } else if &output.partition[label] != order {
                        return Err("output shard order is inconsistent".into());
                    }
                }
            } else if partial_axes.contains(axis)
                && inputs.iter().all(|t| t.local[axis] != Local::P)
            {
                return Err(format!(
                    "no sharded contraction justifies Partial on {axis}"
                ));
            }
        }
    } else {
        for axis in &axes {
            let shards: Vec<_> = inputs
                .iter()
                .filter(|t| t.local[axis] == Local::V)
                .map(|t| t.shard(axis))
                .collect();
            if shards.iter().any(|dim| dim.is_none()) {
                return Err(format!(
                    "global pointwise compute needs a shard dimension on {axis}"
                ));
            }
            if let Some(Some(dim)) = shards.first() {
                if shards.iter().any(|d| d != &Some(*dim)) {
                    return Err(format!("pointwise shard dimensions disagree on {axis}"));
                }
                let orders: Vec<_> = inputs
                    .iter()
                    .filter(|t| t.local[axis] == Local::V)
                    .map(|t| &t.partition[*dim])
                    .collect();
                if orders.iter().any(|order| *order != orders[0]) {
                    return Err(format!("pointwise shard ordering disagrees on {axis}"));
                }
                if output.partition[*dim].is_empty() {
                    output.partition[*dim] = orders[0].clone();
                } else if &output.partition[*dim] != orders[0] {
                    return Err(format!(
                        "coincident output shards have incompatible ordering on dimension {dim}"
                    ));
                }
            }
        }
    }
    output.validate(true)?;
    Ok(output)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Collective {
    Convert,
    InvariantToReplicate,
    Reinterpret,
    AllReduce,
    AllGather,
    ReduceScatter,
    AllToAll,
}

/// Validate a transition before changing any descriptor. Unlike a runtime
/// annotation setter, a missing source axis is an error, never an assumption.
pub fn transition(
    input: &Tensor,
    kind: Collective,
    axis: &str,
    src: Placement,
    dst: Placement,
    global: bool,
) -> Result<Tensor, String> {
    let source = src.local();
    let destination = dst.local();
    let allowed = match kind {
        Collective::InvariantToReplicate => source == Local::I && destination == Local::R,
        Collective::Convert => {
            (matches!(
                (source, destination),
                (Local::I, Local::R) | (Local::R, Local::P)
            ) || (matches!(source, Local::I | Local::R) && matches!(dst, Placement::Shard(_))))
                || source == destination
        }
        Collective::Reinterpret => {
            source == Local::V
                && destination == Local::P
                && matches!(src, Placement::Local(_))
                && matches!(dst, Placement::Local(_))
        }
        Collective::AllReduce => {
            matches!(source, Local::P | Local::V)
                && matches!(destination, Local::R | Local::I)
                && matches!(src, Placement::Local(_))
        }
        Collective::AllGather => source == Local::V && matches!(destination, Local::R | Local::I),
        Collective::ReduceScatter => {
            matches!(source, Local::P | Local::V)
                && destination == Local::V
                && matches!(src, Placement::Local(_))
        }
        Collective::AllToAll => {
            source == Local::V
                && destination == Local::V
                && (matches!(src, Placement::Shard(_)) == matches!(dst, Placement::Shard(_)))
        }
    };
    if !allowed {
        return Err(format!("{kind:?} does not admit {src:?} -> {dst:?}"));
    }
    let actual = input
        .local
        .get(axis)
        .ok_or_else(|| format!("tensor has no source contract on axis {axis}"))?;
    if *actual != source
        && !(input.grad == Grad::No
            && matches!(
                (*actual, source),
                (Local::I, Local::R) | (Local::R, Local::I)
            ))
    {
        return Err(format!(
            "collective src {source:?} does not match {actual:?} on {axis}"
        ));
    }
    let source_dim = match src {
        Placement::Shard(dim) => Some(dimension(dim, input.rank)?),
        Placement::Local(_) => None,
    };
    let destination_dim = match dst {
        Placement::Shard(dim) => Some(dimension(dim, input.rank)?),
        Placement::Local(_) => None,
    };
    if let Some(dim) = source_dim {
        if input.shard(axis) != Some(dim) {
            return Err(format!(
                "collective source shard dimension does not match on {axis}"
            ));
        }
    } else if global && source == Local::V && input.shard(axis).is_some() {
        return Err("global collective requires an explicit S(i) source".into());
    }
    if global
        && ((source == Local::V && source_dim.is_none())
            || (destination == Local::V && destination_dim.is_none()))
    {
        return Err("global collectives require S(i), not unqualified V".into());
    }
    if kind == Collective::Convert
        && source_dim != destination_dim
        && source == Local::V
        && destination == Local::V
    {
        return Err("convert cannot change a shard dimension without communication".into());
    }
    if let Some(dim) = input.shard(axis)
        && input.partition[dim].last().map(String::as_str) != Some(axis)
    {
        return Err("a single-axis collective cannot change an outer shard axis".into());
    }
    let mut result = input.clone();
    for axes in &mut result.partition {
        axes.retain(|a| a != axis);
    }
    result.local.insert(axis.into(), destination);
    if let Some(dim) = destination_dim {
        result.partition[dim].push(axis.into());
    }
    if kind == Collective::AllGather && src == Placement::Local(Local::V) {
        result.rank += 1;
        result.partition.insert(0, Vec::new());
    }
    if kind == Collective::ReduceScatter && dst == Placement::Local(Local::V) {
        if result.rank == 0 || !result.partition[0].is_empty() {
            return Err("unbind reduce_scatter requires an unsharded leading dimension".into());
        }
        result.rank -= 1;
        result.partition.remove(0);
    }
    result.validate(global)?;
    Ok(result)
}
