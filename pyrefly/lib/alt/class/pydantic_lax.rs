/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use pyrefly_python::module_name::ModuleName;
use pyrefly_types::class::ClassType;
use pyrefly_types::keywords::ConverterMap;
use pyrefly_types::types::TArgs;
use pyrefly_types::types::Union;
use starlark_map::ordered_map::OrderedMap;

use crate::alt::answers::LookupAnswer;
use crate::alt::answers_solver::AnswersSolver;
use crate::types::class::Class;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    fn types_to_union(&self, types: &[&ClassType]) -> Type {
        let expanded_types: Vec<Type> = types.iter().map(|cls| (*cls).clone().to_type()).collect();
        self.unions(expanded_types)
    }

    fn class_types_to_union(&self, types: Vec<ClassType>) -> Type {
        self.unions(types.into_iter().map(|c| c.to_type()).collect())
    }

    fn expand_types(&self, types: &[Type]) -> Vec<Type> {
        types
            .iter()
            .map(|t| self.expand_type_for_lax_mode(t))
            .collect()
    }

    fn get_atomic_lax_conversion(&self, ty: &Type) -> Option<Type> {
        match ty {
            Type::ClassType(cls) if cls == self.stdlib.bool() => Some(self.types_to_union(&[
                self.stdlib.bool(),
                self.stdlib.int(),
                self.stdlib.float(),
                self.stdlib.str(),
                self.stdlib.decimal(),
            ])),
            Type::ClassType(cls) if cls == self.stdlib.int() => Some(self.types_to_union(&[
                self.stdlib.int(),
                self.stdlib.bool(),
                self.stdlib.float(),
                self.stdlib.str(),
                self.stdlib.bytes(),
                self.stdlib.decimal(),
            ])),
            Type::ClassType(cls) if cls == self.stdlib.float() => Some(self.types_to_union(&[
                self.stdlib.float(),
                self.stdlib.int(),
                self.stdlib.bool(),
                self.stdlib.str(),
                self.stdlib.bytes(),
                self.stdlib.decimal(),
            ])),
            Type::ClassType(cls) if cls == self.stdlib.bytes() => Some(self.types_to_union(&[
                self.stdlib.bytes(),
                self.stdlib.bytearray(),
                self.stdlib.str(),
            ])),
            Type::ClassType(cls) if cls == self.stdlib.str() => Some(self.types_to_union(&[
                self.stdlib.str(),
                self.stdlib.bytes(),
                self.stdlib.bytearray(),
            ])),
            Type::ClassType(cls) if cls == self.stdlib.date() => Some(self.types_to_union(&[
                self.stdlib.date(),
                self.stdlib.datetime(),
                self.stdlib.int(),
                self.stdlib.float(),
                self.stdlib.str(),
                self.stdlib.bytes(),
                self.stdlib.decimal(),
            ])),
            Type::ClassType(cls) if cls == self.stdlib.datetime() => Some(self.types_to_union(&[
                self.stdlib.datetime(),
                self.stdlib.date(),
                self.stdlib.int(),
                self.stdlib.float(),
                self.stdlib.str(),
                self.stdlib.bytes(),
                self.stdlib.decimal(),
            ])),
            _ => None,
        }
    }

    fn get_container_lax_conversion(
        &self,
        class_obj: &Class,
        expanded_targs: &[Type],
    ) -> Option<Type> {
        if class_obj.has_toplevel_qname(ModuleName::collections().as_str(), "deque") {
            let elem_ty = expanded_targs
                .first()
                .cloned()
                .unwrap_or_else(Type::any_implicit);
            return Some(self.class_types_to_union(vec![
                self.stdlib.deque(elem_ty.clone()),
                self.stdlib.frozenset(elem_ty.clone()),
                self.stdlib.list(elem_ty.clone()),
                self.stdlib.set(elem_ty.clone()),
                self.stdlib.tuple(elem_ty),
            ]));
        }
        if class_obj == self.stdlib.dict_object() {
            let key_ty = expanded_targs
                .first()
                .cloned()
                .unwrap_or_else(Type::any_implicit);
            let val_ty = expanded_targs
                .get(1)
                .cloned()
                .unwrap_or_else(Type::any_implicit);
            return Some(self.class_types_to_union(vec![
                self.stdlib.dict(key_ty.clone(), val_ty.clone()),
                self.stdlib.mapping(key_ty, val_ty),
            ]));
        }
        None
    }

    fn expand_type_for_lax_mode(&self, ty: &Type) -> Type {
        match ty {
            // Container types: recursively expand all type arguments
            Type::ClassType(cls) if !cls.targs().as_slice().is_empty() => {
                let class_obj = cls.class_object();
                let targs = cls.targs().as_slice();
                let expanded_targs = self.expand_types(targs);

                // Check for container type conversions
                if let Some(converted) =
                    self.get_container_lax_conversion(class_obj, &expanded_targs)
                {
                    return converted;
                }

                let tparams = self.get_class_tparams(class_obj);
                Type::ClassType(ClassType::new(
                    class_obj.dupe(),
                    TArgs::new(tparams, expanded_targs),
                ))
            }
            Type::Union(box Union { members, .. }) => {
                let expanded_members = self.expand_types(members);
                self.unions(expanded_members)
            }
            // Known atomic types with conversion tables, or Any for everything else
            _ => self
                .get_atomic_lax_conversion(ty)
                .unwrap_or_else(Type::any_explicit),
        }
    }

    pub fn build_pydantic_lax_conversion_table(&self, field_types: &[Type]) -> ConverterMap {
        let mut table = OrderedMap::new();

        for field_ty in field_types {
            if table.contains_key(field_ty) {
                continue;
            }

            let expanded = self.expand_type_for_lax_mode(field_ty);
            table.insert(field_ty.clone(), expanded);
        }

        ConverterMap::from_map(table)
    }
}
