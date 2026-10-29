/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Strict JSON sidecars are trusted integration inputs, never executable code.

use std::collections::BTreeSet;

use pyrefly_static_analysis::OpId;
use pyrefly_static_analysis::OpKind;
use pyrefly_static_analysis::Program;
use pyrefly_static_analysis::RegionId;
use serde_json::Map;
use serde_json::Value as Json;

use crate::Contract;
use crate::Grad;
use crate::Local;
use crate::Mesh;
use crate::Placement;
use crate::Tensor;
use crate::Value;

fn object(value: &Json) -> Result<&Map<String, Json>, String> {
    value
        .as_object()
        .ok_or_else(|| "expected JSON object".into())
}

fn fields(value: &Map<String, Json>, allowed: &[&str]) -> Result<(), String> {
    if let Some(key) = value.keys().find(|key| !allowed.contains(&key.as_str())) {
        return Err(format!("unknown sidecar field {key}"));
    }
    Ok(())
}

fn strings(value: &Json) -> Result<Vec<String>, String> {
    value
        .as_array()
        .ok_or("expected axis name array")?
        .iter()
        .map(|value| {
            value
                .as_str()
                .map(String::from)
                .ok_or_else(|| "expected string axis name".into())
        })
        .collect()
}

fn placement(value: &str) -> Result<Placement, String> {
    let local = match value {
        "R" => Some(Local::R),
        "I" => Some(Local::I),
        "V" => Some(Local::V),
        "P" => Some(Local::P),
        _ => None,
    };
    if let Some(local) = local {
        return Ok(Placement::Local(local));
    }
    if let Some(dim) = value
        .strip_prefix("S(")
        .and_then(|value| value.strip_suffix(')'))
    {
        return dim
            .parse::<i64>()
            .map(Placement::Shard)
            .map_err(|_| "invalid S(i) dimension".into());
    }
    Err(format!("unknown placement {value}"))
}

fn tensor(value: &Json) -> Result<Tensor, String> {
    let value = object(value)?;
    fields(value, &["rank", "grad", "axes", "partition"])?;
    let rank = value
        .get("rank")
        .and_then(Json::as_u64)
        .ok_or("tensor needs unsigned rank")?;
    let rank = usize::try_from(rank).map_err(|_| "rank does not fit this platform")?;
    if rank > 32 {
        return Err("descriptor rank exceeds 32".into());
    }
    let grad = match value.get("grad").and_then(Json::as_str) {
        Some("yes") => Grad::Yes,
        Some("no") => Grad::No,
        Some("unknown") => Grad::Unknown,
        _ => return Err("tensor grad must be yes, no or unknown".into()),
    };
    let axes = object(value.get("axes").ok_or("tensor needs axes")?)?;
    let mut placements = Vec::new();
    for (axis, value) in axes {
        placements.push((
            axis.clone(),
            placement(value.as_str().ok_or("axis placement must be a string")?)?,
        ));
    }
    let mut result = Tensor::new(rank, grad, placements)?;
    if let Some(partition) = value.get("partition") {
        let mut dims = Vec::new();
        for dim in partition.as_array().ok_or("partition must be an array")? {
            dims.push(if dim.is_null() {
                Vec::new()
            } else if let Some(axis) = dim.as_str() {
                vec![axis.into()]
            } else {
                strings(dim)?
            });
        }
        result = result.with_partition(dims)?;
    }
    Ok(result)
}

fn input(value: &Json) -> Result<Value, String> {
    let value_object = object(value)?;
    if let Some(mesh) = value_object.get("mesh") {
        fields(value_object, &["mesh"])?;
        Ok(Value::Mesh(Mesh::new(strings(mesh)?)?))
    } else if value_object.get("predicate") == Some(&Json::Bool(true)) {
        fields(value_object, &["predicate"])?;
        Ok(Value::Predicate)
    } else {
        Ok(Value::Tensor(tensor(value)?))
    }
}

/// Resolve a partial-output contract only when exactly one compute operation
/// starts at the specified line in the selected region. Ambiguity is an error;
/// source text is never reparsed or pattern-matched for operation semantics.
pub fn parse(program: &Program, text: &str) -> Result<(RegionId, Contract), String> {
    let value: Json = serde_json::from_str(text).map_err(|error| error.to_string())?;
    let value = object(&value)?;
    fields(
        value,
        &[
            "function",
            "global",
            "mesh",
            "inputs",
            "return",
            "partial_outputs",
        ],
    )?;
    let name = value
        .get("function")
        .and_then(Json::as_str)
        .ok_or("sidecar needs function name")?;
    let candidates: Vec<_> = program
        .regions
        .iter()
        .enumerate()
        .filter(|(_, region)| {
            region.name.as_str() == name
                && region
                    .parent
                    .is_some_and(|(parent, _)| parent == Program::MODULE)
        })
        .collect();
    if candidates.len() != 1 {
        return Err("function must name exactly one module-level region".into());
    }
    let region = RegionId(candidates[0].0 as u32);
    let mut contract = Contract::default();
    if let Some(global) = value.get("global") {
        contract.global = global.as_bool().ok_or("global must be a boolean")?;
    }
    if let Some(mesh) = value.get("mesh") {
        contract.mesh = Some(Mesh::new(strings(mesh)?)?);
    }
    for (name, value) in object(value.get("inputs").ok_or("sidecar needs inputs")?)? {
        contract.arguments.insert(name.clone(), input(value)?);
    }
    if let Some(value) = value.get("return") {
        contract.returns = Some(tensor(value)?);
    }
    if let Some(partial) = value.get("partial_outputs") {
        for value in partial
            .as_array()
            .ok_or("partial_outputs must be an array")?
        {
            let value = object(value)?;
            fields(value, &["line", "axes"])?;
            let line = value
                .get("line")
                .and_then(Json::as_u64)
                .ok_or("partial output needs unsigned line")?;
            let axes: BTreeSet<_> = strings(value.get("axes").ok_or("partial output needs axes")?)?
                .into_iter()
                .collect();
            if axes.is_empty() {
                return Err("partial output must name at least one axis".into());
            }
            let ops: Vec<_> = program
                .ops
                .iter()
                .enumerate()
                .filter(|(_, op)| {
                    op.region == region
                        && program.line_col(op.range).0.get() as u64 == line
                        && matches!(op.kind, OpKind::Call { .. } | OpKind::Binary { .. })
                })
                .collect();
            if ops.len() != 1 {
                return Err(format!(
                    "line {line} must identify exactly one call or binary operation"
                ));
            }
            if contract
                .partial_outputs
                .insert(OpId(ops[0].0 as u32), axes)
                .is_some()
            {
                return Err(format!("duplicate partial output line {line}"));
            }
        }
    }
    Ok((region, contract))
}
