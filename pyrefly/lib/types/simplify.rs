/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::types::literal::Lit;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::types::Type;

/// Turn unions of unions into a flattened list for one union, and return the deduped list.
fn flatten_and_dedup(xs: Vec<Type>) -> Vec<Type> {
    fn flatten(xs: Vec<Type>, res: &mut Vec<Type>) {
        for x in xs {
            match x {
                Type::Union(xs) => flatten(xs, res),
                Type::Never(_) => {}
                _ => res.push(x),
            }
        }
    }
    let mut res = Vec::with_capacity(xs.len());
    flatten(xs, &mut res);

    res.sort();
    res.dedup();
    res
}

/// Given a list of types to union together,
/// - If there's 0 element in the list, return `Ok` with `Type::never()`.
/// - If there's 1 element in the list, return `Ok` with that element.
/// - Otherwise, return `Err` along with `xs`.
fn try_collapse(mut xs: Vec<Type>) -> Result<Type, Vec<Type>> {
    if xs.is_empty() {
        Ok(Type::never())
    } else if xs.len() == 1 {
        Ok(xs.pop().unwrap())
    } else {
        Err(xs)
    }
}

/// Union a set of types together, simplifying as much as you can.
pub fn unions(xs: Vec<Type>) -> Type {
    try_collapse(xs).unwrap_or_else(|xs| {
        let res = flatten_and_dedup(xs);
        // `res` is collapsable again if `flatten_and_dedup` drops `xs` to 0 or 1 elements
        try_collapse(res).unwrap_or_else(Type::Union)
    })
}

/// Like `unions`, but also simplify away things regarding literals if you can,
/// e.g. `Literal[True, False] ==> bool`.
pub fn unions_with_literals(mut xs: Vec<Type>, stdlib: &Stdlib) -> Type {
    replace_literal_true_false_with_bool(&mut xs, stdlib);
    unions(xs)
}

fn replace_literal_true_false_with_bool(types: &mut Vec<Type>, stdlib: &Stdlib) {
    let mut has_true = false;
    let mut has_false = false;

    for t in types.iter() {
        match t {
            Type::Literal(Lit::Bool(true)) => {
                has_true = true;
            }
            Type::Literal(Lit::Bool(false)) => {
                has_false = true;
            }
            _ => {}
        }
    }

    if has_true && has_false {
        types.retain(|t| !matches!(t, Type::Literal(Lit::Bool(_))));
        types.push(stdlib.bool().clone().to_type());
    }
}

fn flatten_unpacked_concrete_tuples(elts: Vec<Type>) -> Vec<Type> {
    let mut result = Vec::new();
    for elt in elts {
        match elt {
            Type::Unpack(box Type::Tuple(Tuple::Concrete(elts))) => {
                result.extend(elts);
            }
            _ => result.push(elt),
        }
    }
    result
}

// After a TypeVarTuple gets substituted with a tuple type, try to simplify the type
pub fn simplify_tuples(tuple: Tuple) -> Type {
    match tuple {
        Tuple::Concrete(elts) => {
            Type::Tuple(Tuple::Concrete(flatten_unpacked_concrete_tuples(elts)))
        }
        Tuple::Unpacked(box (prefix, middle, suffix)) if prefix.is_empty() && suffix.is_empty() => {
            middle
        }
        Tuple::Unpacked(box (prefix, middle, suffix)) => match middle {
            Type::Tuple(Tuple::Concrete(elts)) => {
                Type::Tuple(Tuple::Concrete(flatten_unpacked_concrete_tuples(
                    prefix
                        .into_iter()
                        .chain(elts)
                        .chain(suffix)
                        .collect::<Vec<_>>(),
                )))
            }
            Type::Tuple(Tuple::Unpacked(box (m_prefix, m_middle, m_suffix))) => {
                let mut new_prefix = flatten_unpacked_concrete_tuples(prefix);
                new_prefix.extend(flatten_unpacked_concrete_tuples(m_prefix));
                let mut new_suffix = flatten_unpacked_concrete_tuples(m_suffix);
                new_suffix.extend(flatten_unpacked_concrete_tuples(suffix));
                Type::Tuple(Tuple::Unpacked(Box::new((
                    new_prefix, m_middle, new_suffix,
                ))))
            }
            _ => Type::Tuple(Tuple::Unpacked(Box::new((
                flatten_unpacked_concrete_tuples(prefix),
                middle,
                flatten_unpacked_concrete_tuples(suffix),
            )))),
        },
        _ => Type::Tuple(tuple),
    }
}

#[cfg(test)]
mod tests {
    use crate::types::simplify::unions;
    use crate::types::types::NeverStyle;
    use crate::types::types::Type;

    #[test]
    fn test_flatten_nevers() {
        let xs = vec![
            Type::Never(NeverStyle::Never),
            Type::Never(NeverStyle::NoReturn),
        ];
        let res = unions(xs);
        assert_eq!(res, Type::never());
    }
}
