/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;
use std::sync::LazyLock;

use regex::Regex;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprDict;
use ruff_python_ast::ExprList;
use ruff_python_ast::ExprTuple;
use ruff_python_ast::Identifier;
use ruff_python_ast::Keyword;
use ruff_python_ast::StmtClassDef;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::alt::class::class_metadata::BaseClass;
use crate::binding::binding::AnnotationTarget;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingClass;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingClassSynthesizedFields;
use crate::binding::binding::ClassBinding;
use crate::binding::binding::ClassFieldInitialValue;
use crate::binding::binding::ExprOrBinding;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClass;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyClassSynthesizedFields;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::bindings::LegacyTParamBuilder;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::InstanceAttribute;
use crate::binding::scope::MethodThatSetsAttr;
use crate::binding::scope::Scope;
use crate::binding::scope::ScopeKind;
use crate::error::kind::ErrorKind;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::class::ClassFieldProperties;
use crate::types::class::ClassIndex;
use crate::types::special_form::SpecialForm;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;

enum IllegalIdentifierHandling {
    Error,
    Allow,
    Rename,
}

#[derive(Eq, PartialEq)]
enum SynthesizedClassKind {
    Enum,
    TypedDict,
    NamedTuple,
    NewType,
}

impl<'a> BindingsBuilder<'a> {
    fn class_index(&mut self) -> ClassIndex {
        let res = ClassIndex(self.class_count);
        self.class_count += 1;
        res
    }

    pub fn class_def(&mut self, mut x: StmtClassDef) {
        if self.module_info.name() == ModuleName::typing() && x.name.as_str() == "Any" {
            // We special case the definition of `Any`, because it isn't a `SpecialForm`,
            // but an ordinary `class`.
            self.bind_definition(
                &x.name,
                Binding::Type(Type::type_form(Type::any_explicit())),
                FlowStyle::None,
            );
            return;
        }

        let body = mem::take(&mut x.body);
        let decorators = self.ensure_and_bind_decorators(mem::take(&mut x.decorator_list));

        self.scopes.push(Scope::annotation(x.range));

        let class_index = self.class_index();
        let class_key = KeyClass(ShortIdentifier::new(&x.name));
        // @nocommit(stroxler): clowntown
        //
        // This one is actually basically okay: the stack discipline I want is more or less
        // that we *should* be creating this idx at the beginning of processing.
        //
        // It's only marked as clowntown because most inserts do the opposite and generate the
        // key at the very end ... in reality we want *everything* to work like this, but
        // ideally through some kind of abstraction that makes it easier.
        let definition_key = self.table.classes.0.insert(class_key);

        x.type_params.iter_mut().for_each(|x| {
            self.type_params(x);
        });

        let mut legacy = Some(LegacyTParamBuilder::new(x.type_params.is_some()));
        let bases = x.bases().map(|base| {
            let mut base = base.clone();
            // Forward refs are fine *inside* of a base expression in the type arguments,
            // but outermost class cannot be a forward ref.
            match &base {
                Expr::StringLiteral(v) => {
                    self.error(
                        base.range(),
                        format!(
                            "Cannot use string annotation `{}` as a base class",
                            v.value.to_str()
                        ),
                        ErrorKind::InvalidInheritance,
                    );
                }
                _ => {}
            }
            self.ensure_type(&mut base, &mut legacy);
            base
        });

        let mut keywords = Vec::new();
        if let Some(args) = &mut x.arguments {
            args.keywords.iter_mut().for_each(|keyword| {
                if let Some(name) = &keyword.arg {
                    self.ensure_expr(&mut keyword.value);
                    keywords.push((name.id.clone(), keyword.value.clone()));
                } else {
                    self.error(
                        keyword.range(),
                        format!(
                            "The use of unpacking in class header of `{}` is not supported",
                            x.name
                        ),
                        ErrorKind::InvalidInheritance,
                    )
                }
            });
        }

        self.table.insert(
            KeyClassMetadata(class_index),
            BindingClassMetadata {
                def: definition_key,
                bases: bases.clone().into_boxed_slice(),
                keywords: keywords.into_boxed_slice(),
                decorators: decorators.clone().into_boxed_slice(),
                is_new_type: false,
                special_base: None,
            },
        );
        self.table.insert(
            KeyClassSynthesizedFields(class_index),
            BindingClassSynthesizedFields(definition_key),
        );

        let legacy_tparam_builder = legacy.unwrap();
        legacy_tparam_builder.add_name_definitions(self);

        self.scopes
            .push(Scope::class_body(x.range, class_index, x.name.clone()));
        self.init_static_scope(&body, false);
        self.stmts(body);

        let last_scope = self.scopes.pop();
        self.scopes.pop(); // annotation scope
        let mut fields_defined_in_this_class = SmallMap::with_capacity(last_scope.stat.0.len());
        for (name, info) in last_scope.flow.info.iter_hashed() {
            let is_function_without_return_annotation =
                if let FlowStyle::FunctionDef(_, has_return_annotation) = info.style {
                    !has_return_annotation
                } else {
                    false
                };
            // A name with flow in the last_scope, but whose static is in a parent scope, is a reference to something that isn't a class field.
            // Can occur when we narrow a parent scopes variable, thus producing a fresh flow for it, but no static.
            if let Some(stat_info) = last_scope.stat.0.get_hashed(name) {
                let initial_value = info.as_initial_value();
                let value = match &initial_value {
                    ClassFieldInitialValue::Class(Some(e)) => ExprOrBinding::Expr(e.clone()),
                    _ => ExprOrBinding::Binding(Binding::Forward(info.key)),
                };
                let binding = BindingClassField {
                    class: definition_key,
                    name: name.into_key().clone(),
                    value,
                    annotation: stat_info.annot,
                    range: stat_info.loc,
                    initial_value,
                    is_function_without_return_annotation,
                };
                fields_defined_in_this_class.insert_hashed(
                    name.cloned(),
                    ClassFieldProperties::new(stat_info.annot.is_some(), stat_info.loc),
                );
                self.table
                    .insert(KeyClassField(class_index, name.into_key().clone()), binding);
            }
        }
        if let ScopeKind::ClassBody(body) = last_scope.kind {
            for (
                name,
                MethodThatSetsAttr {
                    method_name,
                    recognized_attribute_defining_method,
                },
                InstanceAttribute(value, annotation, range),
            ) in body.method_defined_attributes()
            {
                if !fields_defined_in_this_class.contains_key_hashed(name.as_ref()) {
                    if !recognized_attribute_defining_method {
                        self.error(
                        range,
                        format!("Attribute `{}` is implicitly defined by assignment in method `{method_name}`, which is not a constructor", &name),
                        ErrorKind::ImplicitlyDefinedAttribute)
                    }
                    fields_defined_in_this_class.insert_hashed(
                        name.clone(),
                        ClassFieldProperties::new(annotation.is_some(), range),
                    );
                    self.table.insert(
                        KeyClassField(class_index, name.key().clone()),
                        BindingClassField {
                            class: definition_key,
                            name: name.into_key(),
                            value,
                            annotation,
                            range,
                            initial_value: ClassFieldInitialValue::Instance(Some(
                                method_name.clone(),
                            )),
                            is_function_without_return_annotation: false,
                        },
                    );
                } else if annotation.is_some() {
                    self.error(range, format!("Cannot annotate attribute `{}`, which is already annotated in the class body", &name), ErrorKind::InvalidAnnotation);
                }
            }
        } else {
            unreachable!("Expected class body scope, got {:?}", last_scope.kind);
        }

        let legacy_tparams = legacy_tparam_builder.lookup_keys();

        self.bind_definition(
            &x.name,
            Binding::ClassDef(definition_key, decorators.into_boxed_slice()),
            FlowStyle::None,
        );
        fields_defined_in_this_class.reserve(0); // Attempt to shrink to capacity
        self.table.insert_idx(
            definition_key,
            BindingClass::ClassDef(ClassBinding {
                index: class_index,
                def: x,
                fields: fields_defined_in_this_class,
                bases: bases.into_boxed_slice(),
                legacy_tparams: legacy_tparams.into_boxed_slice(),
            }),
        );
    }

    fn extract_string_literals(
        &mut self,
        items: &[Expr],
    ) -> Vec<(String, TextRange, Option<Expr>)> {
        items
            .iter()
            .filter_map(|item| match item {
                Expr::StringLiteral(x) => Some((x.value.to_string(), x.range(), None)),
                _ => {
                    self.error(
                        item.range(),
                        "Expected a string literal".to_owned(),
                        ErrorKind::InvalidLiteral,
                    );
                    None
                }
            })
            .collect()
    }

    fn decompose_key_value_pairs(
        &mut self,
        items: &[Expr],
    ) -> Vec<(String, TextRange, Option<Expr>)> {
        items
            .iter()
            .filter_map(|item| match item {
                Expr::Tuple(ExprTuple { elts, .. }) => match elts.as_slice() {
                    [Expr::StringLiteral(k), v] => {
                        Some((k.value.to_string(), k.range(), Some(v.clone())))
                    }
                    [k, _] => {
                        self.error(
                            k.range(),
                            "Expected first item to be a string literal".to_owned(),
                            ErrorKind::InvalidArgument,
                        );
                        None
                    }
                    _ => {
                        self.error(
                            item.range(),
                            "Expected a pair".to_owned(),
                            ErrorKind::InvalidArgument,
                        );
                        None
                    }
                },
                _ => {
                    self.error(
                        item.range(),
                        "Expected a tuple".to_owned(),
                        ErrorKind::InvalidArgument,
                    );
                    None
                }
            })
            .collect()
    }

    fn synthesize_class_def(
        &mut self,
        class_name: Identifier,
        base: Option<Expr>,
        keywords: Box<[(Name, Expr)]>,
        // name, position, annotation, value
        member_definitions: Vec<(String, TextRange, Option<Expr>, Option<Expr>)>,
        illegal_identifier_handling: IllegalIdentifierHandling,
        force_class_initialization: bool,
        class_kind: SynthesizedClassKind,
        special_base: Option<Box<BaseClass>>,
    ) {
        let class_index = self.class_index();
        let class_key = KeyClass(ShortIdentifier::new(&class_name));
        // @nocommit(stroxler): clowntown
        //
        // Same situation as above... this isn't really clowny, it's actually approximately
        // the pattern i want to *go* to, except that the way it's done here is likely
        // too manual.
        let definition_key = self.table.classes.0.insert(class_key.clone());
        self.table.insert(
            KeyClassMetadata(class_index),
            BindingClassMetadata {
                def: definition_key,
                bases: base.into_iter().collect::<Vec<_>>().into_boxed_slice(),
                keywords,
                decorators: Box::new([]),
                is_new_type: class_kind == SynthesizedClassKind::NewType,
                special_base,
            },
        );
        self.table.insert(
            KeyClassSynthesizedFields(class_index),
            BindingClassSynthesizedFields(definition_key),
        );
        let mut fields = SmallMap::new();
        for (idx, (member_name, range, member_annotation, member_value)) in
            member_definitions.into_iter().enumerate()
        {
            let mut member_name = member_name;
            if !is_valid_identifier(member_name.as_str()) {
                match illegal_identifier_handling {
                    IllegalIdentifierHandling::Allow => {}
                    IllegalIdentifierHandling::Error => {
                        self.error(
                            range,
                            format!("`{member_name}` is not a valid identifier"),
                            ErrorKind::BadClassDefinition,
                        );
                        continue;
                    }
                    IllegalIdentifierHandling::Rename => member_name = format!("_{idx}"),
                }
            }
            if class_kind == SynthesizedClassKind::NamedTuple && member_name.starts_with("_") {
                match illegal_identifier_handling {
                    IllegalIdentifierHandling::Allow => {}
                    IllegalIdentifierHandling::Error => {
                        self.error(
                            range,
                            format!(
                                "NamedTuple field name may not start with an underscore: `{member_name}`"
                            ),
                            ErrorKind::BadClassDefinition,
                        );
                        continue;
                    }
                    IllegalIdentifierHandling::Rename => member_name = format!("_{idx}"),
                }
            }
            let member_name = Name::new(member_name);
            if fields.contains_key(&member_name) {
                self.error(
                    range,
                    format!("Duplicate field `{member_name}`"),
                    ErrorKind::BadClassDefinition,
                );
                continue;
            }
            // Synthesized fields for named tuples are always considered annotated
            fields.insert(
                member_name.clone(),
                ClassFieldProperties::new(
                    member_annotation.is_some() || class_kind == SynthesizedClassKind::NamedTuple,
                    range,
                ),
            );
            let initial_value = if force_class_initialization || member_value.is_some() {
                ClassFieldInitialValue::Class(member_value.clone())
            } else {
                ClassFieldInitialValue::Instance(None)
            };
            let value = match member_value {
                Some(value) => ExprOrBinding::Expr(value),
                None => ExprOrBinding::Binding(Binding::Type(Type::any_implicit())),
            };
            let annotation_binding = if let Some(annotation) = member_annotation {
                let ann_key = KeyAnnotation::Annotation(ShortIdentifier::new(&Identifier::new(
                    member_name.clone(),
                    range,
                )));
                let ann_val = if let Some(special) = SpecialForm::new(&member_name, &annotation) {
                    BindingAnnotation::Type(
                        AnnotationTarget::ClassMember(member_name.clone()),
                        special.to_type(),
                    )
                } else {
                    BindingAnnotation::AnnotateExpr(
                        AnnotationTarget::ClassMember(member_name.clone()),
                        annotation,
                        None,
                    )
                };
                Some(self.table.insert(ann_key, ann_val))
            } else {
                None
            };
            self.table.insert(
                KeyClassField(class_index, member_name.clone()),
                BindingClassField {
                    class: definition_key,
                    name: member_name,
                    value,
                    annotation: annotation_binding,
                    range,
                    initial_value,
                    is_function_without_return_annotation: false,
                },
            );
        }
        self.bind_definition(
            &class_name,
            Binding::ClassDef(definition_key, Box::new([])),
            FlowStyle::None,
        );
        self.table.insert(
            class_key,
            BindingClass::FunctionalClassDef(class_index, class_name, fields),
        );
    }

    pub fn synthesize_enum_def(&mut self, class_name: Identifier, base: Expr, members: &[Expr]) {
        let member_definitions: Vec<(String, TextRange, Option<Expr>, Option<Expr>)> =
            match members {
                // Enum('Color', 'RED, GREEN, BLUE')
                // Enum('Color', 'RED GREEN BLUE')
                [Expr::StringLiteral(x)] => {
                    let s = x.value.to_str();
                    if s.contains(',') {
                        s.split(',')
                            .map(str::trim)
                            .map(|s| (s.to_owned(), x.range(), None))
                            .collect()
                    } else {
                        s.split_whitespace()
                            .map(|s| (s.to_owned(), x.range(), None))
                            .collect()
                    }
                }
                // Enum('Color', 'RED', 'GREEN', 'BLUE')
                [Expr::StringLiteral(_), ..] => self.extract_string_literals(members),
                // Enum('Color', ['RED', 'GREEN', 'BLUE'])
                [Expr::List(ExprList { elts, .. })]
                    if matches!(elts.as_slice(), [Expr::StringLiteral(_), ..]) =>
                {
                    self.extract_string_literals(elts)
                }
                // Enum('Color', ('RED', 'GREEN', 'BLUE'))
                [Expr::Tuple(ExprTuple { elts, .. })]
                    if matches!(elts.as_slice(), [Expr::StringLiteral(_), ..]) =>
                {
                    self.extract_string_literals(elts)
                }
                // Enum('Color', [('RED', 1), ('GREEN', 2), ('BLUE', 3)])
                [Expr::List(ExprList { elts, .. })]
                    if matches!(elts.as_slice(), [Expr::Tuple(_), ..]) =>
                {
                    self.decompose_key_value_pairs(elts)
                }
                // Enum('Color', (('RED', 1), ('GREEN', 2), ('BLUE', 3)))
                [Expr::Tuple(ExprTuple { elts, .. })]
                    if matches!(elts.as_slice(), [Expr::Tuple(_), ..]) =>
                {
                    self.decompose_key_value_pairs(elts)
                }
                // Enum('Color', {'RED': 1, 'GREEN': 2, 'BLUE': 3})
                [Expr::Dict(ExprDict { items, .. })] => items
                    .iter()
                    .filter_map(|item| match (&item.key, &item.value) {
                        (Some(Expr::StringLiteral(k)), v) => {
                            Some((k.value.to_string(), k.range(), Some(v.clone())))
                        }
                        (Some(k), _) => {
                            self.error(
                                k.range(),
                                "Expected first item to be a string literal".to_owned(),
                                ErrorKind::InvalidArgument,
                            );
                            None
                        }
                        _ => {
                            self.error(
                                item.range(),
                                "Expected a key-value pair".to_owned(),
                                ErrorKind::InvalidArgument,
                            );
                            None
                        }
                    })
                    .collect(),
                _ => {
                    self.error(
                        class_name.range,
                        "Expected valid functional enum definition".to_owned(),
                        ErrorKind::InvalidArgument,
                    );
                    Vec::new()
                }
            }
            .into_iter()
            .map(|(name, range, value)| (name, range, None, value))
            .collect();
        self.synthesize_class_def(
            class_name,
            Some(base),
            Box::new([]),
            member_definitions,
            IllegalIdentifierHandling::Error,
            true,
            SynthesizedClassKind::Enum,
            None,
        );
    }

    // This functional form supports renaming illegal identifiers and specifying defaults
    // but cannot specify the type of each element
    pub fn synthesize_collections_named_tuple_def(
        &mut self,
        class_name: Identifier,
        members: &[Expr],
        keywords: &mut [Keyword],
    ) {
        let member_definitions: Vec<(String, TextRange, Option<Expr>)> = match members {
            // namedtuple('Point', 'x y')
            // namedtuple('Point', 'x, y')
            [Expr::StringLiteral(x)] => {
                let s = x.value.to_str();
                if s.contains(',') {
                    s.split(',')
                        .map(str::trim)
                        .map(|s| (s.to_owned(), x.range(), None))
                        .collect()
                } else {
                    s.split_whitespace()
                        .map(|s| (s.to_owned(), x.range(), None))
                        .collect()
                }
            }
            // namedtuple('Point', ['x', 'y'])
            [Expr::List(ExprList { elts, .. })]
                if matches!(elts.as_slice(), [Expr::StringLiteral(_), ..]) =>
            {
                self.extract_string_literals(elts)
            }
            // namedtuple('Point', ('x', 'y'))
            [Expr::Tuple(ExprTuple { elts, .. })]
                if matches!(elts.as_slice(), [Expr::StringLiteral(_), ..]) =>
            {
                self.extract_string_literals(elts)
            }
            _ => {
                self.error(
                    class_name.range,
                    "Expected valid functional named tuple definition".to_owned(),
                    ErrorKind::InvalidArgument,
                );
                Vec::new()
            }
        };
        let n_members = member_definitions.len();
        let mut illegal_identifier_handling = IllegalIdentifierHandling::Error;
        let mut defaults: Vec<Option<Expr>> = vec![None; n_members];
        for kw in keywords {
            self.ensure_expr(&mut kw.value);
            if let Some(name) = &kw.arg
                && name.id == "rename"
                && let Expr::BooleanLiteral(lit) = &kw.value
            {
                if lit.value {
                    illegal_identifier_handling = IllegalIdentifierHandling::Rename;
                }
            } else if let Some(name) = &kw.arg
                && name.id == "defaults"
                && let Expr::Tuple(ExprTuple { elts, .. }) = &kw.value
            {
                let n_defaults = elts.len();
                if n_defaults > n_members {
                    self.error(
                        kw.value.range(),
                        format!(
                            "Too many defaults values: expected up to {}, got {}",
                            n_members, n_defaults
                        ),
                        ErrorKind::InvalidArgument,
                    );
                    let n_to_drop = n_defaults - n_members;
                    defaults = elts[n_to_drop..].map(|x| Some(x.clone()));
                } else {
                    defaults.splice(n_members - n_defaults.., elts.map(|x| Some(x.clone())));
                }
            } else {
                self.error(
                    kw.value.range(),
                    "Unrecognized argument for typed dictionary definition".to_owned(),
                    ErrorKind::InvalidArgument,
                );
            }
        }
        let member_definitions_with_defaults: Vec<(String, TextRange, Option<Expr>, Option<Expr>)> =
            member_definitions
                .into_iter()
                .zip(defaults)
                .map(|((name, range, annotation), default)| (name, range, annotation, default))
                .collect();
        let range = class_name.range();
        self.synthesize_class_def(
            class_name,
            None,
            Box::new([]),
            member_definitions_with_defaults,
            illegal_identifier_handling,
            false,
            SynthesizedClassKind::NamedTuple,
            Some(Box::new(BaseClass::CollectionsNamedTuple(range))),
        );
    }

    // This functional form allows specifying types for each element, but not default values
    pub fn synthesize_typing_named_tuple_def(
        &mut self,
        class_name: Identifier,
        base: Expr,
        members: &[Expr],
    ) {
        let member_definitions: Vec<(String, TextRange, Option<Expr>, Option<Expr>)> =
            match members {
                // NamedTuple('Point', [('x', int), ('y', int)])
                [Expr::List(ExprList { elts, .. })]
                    if matches!(elts.as_slice(), [Expr::Tuple(_), ..]) =>
                {
                    self.decompose_key_value_pairs(elts)
                }
                // NamedTuple('Point', (('x', int), ('y', int)))
                [Expr::Tuple(ExprTuple { elts, .. })]
                    if matches!(elts.as_slice(), [Expr::Tuple(_), ..]) =>
                {
                    self.decompose_key_value_pairs(elts)
                }
                _ => {
                    self.error(
                        class_name.range,
                        "Expected valid functional named tuple definition".to_owned(),
                        ErrorKind::InvalidArgument,
                    );
                    Vec::new()
                }
            }
            .into_iter()
            .map(|(name, range, annotation)| {
                if let Some(mut ann) = annotation {
                    self.ensure_type(&mut ann, &mut None);
                    (name, range, Some(ann), None)
                } else {
                    (name, range, None, None)
                }
            })
            .collect();
        self.synthesize_class_def(
            class_name,
            Some(base),
            Box::new([]),
            member_definitions,
            IllegalIdentifierHandling::Error,
            false,
            SynthesizedClassKind::NamedTuple,
            None,
        );
    }

    // Synthesize a class definition for NewType
    pub fn synthesize_typing_new_type(&mut self, class_name: Identifier, base: Expr) {
        self.synthesize_class_def(
            class_name,
            Some(base),
            Box::new([]),
            Vec::new(),
            IllegalIdentifierHandling::Error,
            false,
            SynthesizedClassKind::NewType,
            None,
        );
    }

    pub fn synthesize_typed_dict_def(
        &mut self,
        class_name: Identifier,
        base: Expr,
        args: &mut [Expr],
        keywords: &mut [Keyword],
    ) {
        let mut base_class_keywords: Box<[(Name, Expr)]> = Box::new([]);
        for kw in keywords {
            self.ensure_expr(&mut kw.value);
            if let Some(name) = &kw.arg
                && name.id == "total"
                && matches!(kw.value, Expr::BooleanLiteral(_))
            {
                base_class_keywords = Box::new([(name.id.clone(), kw.value.clone())])
            } else {
                self.error(
                    kw.value.range(),
                    "Unrecognized argument for typed dictionary definition".to_owned(),
                    ErrorKind::InvalidArgument,
                );
            }
        }
        let member_definitions: Vec<(String, TextRange, Option<Expr>, Option<Expr>)> = match args {
            // Movie = TypedDict('Movie', {'name': str, 'year': int})
            [Expr::Dict(ExprDict { items, .. })] => items
                .iter_mut()
                .filter_map(|item| {
                    if let Some(key) = &mut item.key {
                        self.ensure_expr(key);
                    }
                    self.ensure_type(&mut item.value.clone(), &mut None);
                    match (&item.key, &item.value) {
                        (Some(Expr::StringLiteral(k)), v) => {
                            Some((k.value.to_string(), k.range(), Some(v.clone()), None))
                        }
                        (Some(k), _) => {
                            self.error(
                                k.range(),
                                "Expected first item to be a string literal".to_owned(),
                                ErrorKind::InvalidArgument,
                            );
                            None
                        }
                        _ => {
                            self.error(
                                item.range(),
                                "Expected a key-value pair".to_owned(),
                                ErrorKind::InvalidArgument,
                            );
                            None
                        }
                    }
                })
                .collect(),
            _ => {
                self.error(
                    class_name.range,
                    "Expected valid functional typed dictionary definition".to_owned(),
                    ErrorKind::InvalidArgument,
                );
                Vec::new()
            }
        };
        self.synthesize_class_def(
            class_name,
            Some(base),
            base_class_keywords,
            member_definitions,
            IllegalIdentifierHandling::Allow,
            false,
            SynthesizedClassKind::TypedDict,
            None,
        );
    }
}

fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "False"
            | "None"
            | "True"
            | "and"
            | "as"
            | "assert"
            | "async"
            | "await"
            | "break"
            | "class"
            | "continue"
            | "def"
            | "del"
            | "elif"
            | "else"
            | "except"
            | "finally"
            | "for"
            | "from"
            | "global"
            | "if"
            | "import"
            | "in"
            | "is"
            | "lambda"
            | "nonlocal"
            | "not"
            | "or"
            | "pass"
            | "raise"
            | "return"
            | "try"
            | "while"
            | "with"
            | "yield",
    )
}

fn is_valid_identifier(name: &str) -> bool {
    static IDENTIFIER_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap());
    !is_keyword(name) && IDENTIFIER_REGEX.is_match(name)
}
