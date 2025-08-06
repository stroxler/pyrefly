/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::env::current_dir;

use num_traits::ToPrimitive;
use pyrefly_python::docstring::Docstring;
use pyrefly_python::module_name::ModuleName;
use pyrefly_util::visit::Visit;
use ruff_python_ast::Alias;
use ruff_python_ast::Decorator;
use ruff_python_ast::ExceptHandler;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_python_ast::Identifier;
use ruff_python_ast::Parameter;
use ruff_python_ast::ParameterWithDefault;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtClassDef;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use starlark_map::small_set::SmallSet;

use crate::module::module_info::ModuleInfo;
use crate::report::glean::facts::*;
use crate::report::glean::schema::*;
use crate::state::handle::Handle;
use crate::state::state::Transaction;

fn hash(x: &[u8]) -> String {
    // Glean uses blake3
    blake3::hash(x).to_string()
}

fn range_without_decorators(range: TextRange, decorators: &[Decorator]) -> TextRange {
    let decorators_range = decorators
        .first()
        .map(|first| first.range().cover(decorators.last().unwrap().range()));

    decorators_range.map_or(range, |x| range.add_start(x.len() + TextSize::from(1)))
}

fn to_span(range: TextRange) -> src::ByteSpan {
    src::ByteSpan {
        start: range.start().to_u32().into(),
        length: range.len().to_u32().into(),
    }
}

fn file_fact(module_info: &ModuleInfo) -> src::File {
    let file_path = module_info.path().as_path();
    let relative_path = file_path
        .strip_prefix(current_dir().unwrap_or_default())
        .unwrap_or(file_path)
        .to_str()
        .unwrap();

    src::File::new(relative_path.to_owned())
}

fn gather_nonlocal_variables(
    body: &[Stmt],
    globals: &mut SmallSet<Name>,
    nonlocals: &mut SmallSet<Name>,
) {
    for stmt in body {
        match stmt {
            Stmt::Global(stmt_global) => {
                globals.extend(stmt_global.names.iter().map(|name| name.id.clone()))
            }
            Stmt::Nonlocal(stmt_nonlocal) => {
                nonlocals.extend(stmt_nonlocal.names.iter().map(|name| name.id.clone()))
            }
            _ => {}
        }
    }
}
enum ScopeType {
    Global,
    Nonlocal,
    Local,
}
struct DeclarationInfo {
    declaration: python::Declaration,
    decl_span: src::ByteSpan,
    definition: Option<python::Definition>,
    def_span: Option<src::ByteSpan>,
    top_level_decl: Option<python::Declaration>,
    docstring_range: Option<TextRange>,
}

struct Facts {
    file: src::File,
    module: python::Module,
    modules: Vec<python::Module>,
    decl_locations: Vec<python::DeclarationLocation>,
    def_locations: Vec<python::DefinitionLocation>,
    import_star_locations: Vec<python::ImportStarLocation>,
    file_calls: Vec<python::FileCall>,
    callee_to_callers: Vec<python::CalleeToCaller>,
    containing_top_level_declarations: Vec<python::ContainingTopLevelDeclaration>,
    xrefs_via_name: Vec<python::XRefViaName>,
    xrefs_by_target: HashMap<python::Name, Vec<src::ByteSpan>>,
    declaration_docstrings: Vec<python::DeclarationDocstring>,
}

struct GleanState<'a> {
    transaction: &'a Transaction<'a>,
    handle: &'a Handle,
    module: ModuleInfo,
    module_name: ModuleName,
    facts: Facts,
}

impl Facts {
    fn new(file: src::File, module: python::Module) -> Facts {
        Facts {
            file,
            module,
            modules: vec![],
            decl_locations: vec![],
            def_locations: vec![],
            import_star_locations: vec![],
            file_calls: vec![],
            callee_to_callers: vec![],
            containing_top_level_declarations: vec![],
            xrefs_via_name: vec![],
            xrefs_by_target: HashMap::new(),
            declaration_docstrings: vec![],
        }
    }
}

impl GleanState<'_> {
    fn new<'a>(transaction: &'a Transaction<'a>, handle: &'a Handle) -> GleanState<'a> {
        let module_info = &transaction.get_module_info(handle).unwrap();
        GleanState {
            transaction,
            handle,
            module: module_info.clone(),
            module_name: module_info.name(),
            facts: Facts::new(
                file_fact(module_info),
                python::Module::new(python::Name::new(module_info.name().to_string())),
            ),
        }
    }

    fn module_fact(&self) -> python::Module {
        self.facts.module.clone()
    }

    fn file_fact(&self) -> src::File {
        self.facts.file.clone()
    }

    fn digest_fact(&self) -> digest::FileDigest {
        let digest = digest::Digest {
            hash: hash(self.module.contents().as_bytes()),
            size: self.module.contents().len() as u64,
        };
        digest::FileDigest::new(self.file_fact(), digest)
    }

    fn module_facts(&mut self, range: TextRange, top_level_decl: &python::Declaration) {
        let module_docstring_range = self.transaction.get_module_docstring_range(self.handle);
        let components = self.module_name.components();
        let mut module = None;

        for component in components.into_iter() {
            let name = module.map_or(ModuleName::from_name(&component), |x: ModuleName| {
                x.append(&component)
            });
            self.facts
                .modules
                .push(python::Module::new(python::Name::new(name.to_string())));
            module = Some(name);
        }

        let mod_decl_info = DeclarationInfo {
            declaration: python::Declaration::module(self.module_fact()),
            decl_span: to_span(range),
            definition: Some(python::Definition::module(python::ModuleDefinition::new(
                self.module_fact(),
            ))),
            def_span: Some(to_span(range)),
            top_level_decl: None,
            docstring_range: module_docstring_range,
        };

        self.declaration_facts(mod_decl_info, top_level_decl);
    }

    fn file_lines_fact(&self) -> src::FileLines {
        let lined_buffer = self.module.lined_buffer();
        let lens: Vec<u64> = lined_buffer
            .lines()
            .map(|x| x.len().to_u64().unwrap() + 1)
            .collect();
        let ends_in_new_line = lens.len() < lined_buffer.line_count();
        src::FileLines::new(
            self.facts.file.clone(),
            lens,
            ends_in_new_line,
            !lined_buffer.is_ascii() || lined_buffer.contents().contains('\t'),
        )
    }

    fn declaration_facts(
        &mut self,
        decl_info: DeclarationInfo,
        default_top_level_decl: &python::Declaration,
    ) {
        self.facts.containing_top_level_declarations.push(
            python::ContainingTopLevelDeclaration::new(
                decl_info.declaration.clone(),
                decl_info
                    .top_level_decl
                    .unwrap_or(default_top_level_decl.clone()),
            ),
        );

        self.facts
            .decl_locations
            .push(python::DeclarationLocation::new(
                decl_info.declaration.clone(),
                self.facts.file.clone(),
                decl_info.decl_span,
            ));
        if let Some(def_info) = decl_info.definition {
            self.facts
                .def_locations
                .push(python::DefinitionLocation::new(
                    def_info,
                    self.facts.file.clone(),
                    decl_info.def_span.unwrap(),
                ));
        }

        if let Some(docstring_range) = decl_info.docstring_range {
            self.facts
                .declaration_docstrings
                .push(python::DeclarationDocstring::new(
                    decl_info.declaration.clone(),
                    to_span(docstring_range),
                    Docstring::clean(self.module.code_at(docstring_range)),
                ));
        }
    }

    fn make_fq_name(&self, name: &Name, module_name: Option<&str>) -> python::Name {
        // TODO(@rubmary) create fully qualified name
        let fq_name = if let Some(module) = module_name {
            module.to_owned() + "." + name
        } else {
            name.to_string()
        };
        python::Name::new(fq_name)
    }

    fn make_fq_name_for_declaration(
        &self,
        name: &Name,
        container: &python::DeclarationContainer,
        scope_type: ScopeType,
    ) -> python::Name {
        let container_name = match container {
            python::DeclarationContainer::module(module) => &module.key.name,
            python::DeclarationContainer::cls(cls) => &cls.key.name,
            python::DeclarationContainer::func(func) => &func.key.name,
        };
        let container_str = container_name.key.as_str();
        let scope = match scope_type {
            ScopeType::Global => self.module_name.to_string(),
            ScopeType::Nonlocal => {
                let mut parts: Vec<&str> = container_str.split(".").collect();
                parts.pop();
                parts.join(".")
            }
            ScopeType::Local => {
                if let python::DeclarationContainer::func(_) = container {
                    container_str.to_owned() + ".<locals>"
                } else {
                    container_str.to_owned()
                }
            }
        };

        python::Name::new(scope.to_owned() + "." + name)
    }

    fn make_decorators(&self, decorators: &[Decorator]) -> Vec<String> {
        let lined_buffer = self.module.lined_buffer();
        decorators
            .iter()
            .map(|x| lined_buffer.code_at(x.range()).to_owned())
            .collect()
    }

    fn class_facts(
        &mut self,
        cls: &StmtClassDef,
        cls_declaration: python::ClassDeclaration,
        container: python::DeclarationContainer,
    ) -> DeclarationInfo {
        let bases = if let Some(arguments) = &cls.arguments {
            arguments
                .args
                .iter()
                .filter_map(|expr| expr.as_name_expr())
                .map(|expr_name| {
                    python::ClassDeclaration::new(self.make_fq_name(&expr_name.id, None), None)
                })
                .collect()
        } else {
            vec![]
        };

        let declaration = python::Declaration::cls(cls_declaration.clone());

        let cls_docstring_range = Docstring::range_from_stmts(&cls.body);

        let cls_definition = python::ClassDefinition::new(
            cls_declaration.clone(),
            Some(bases),
            None,
            Some(self.make_decorators(&cls.decorator_list)),
            Some(container),
        );

        DeclarationInfo {
            declaration,
            decl_span: to_span(range_without_decorators(cls.range, &cls.decorator_list)),
            definition: Some(python::Definition::cls(cls_definition)),
            def_span: Some(to_span(cls.range)),
            top_level_decl: None,
            docstring_range: cls_docstring_range,
        }
    }

    fn make_xref(&self, expr: &Expr) -> Option<python::XRefViaName> {
        let none_name = Name::new("None");
        let xref_info = match expr {
            Expr::Attribute(attr) => Some((attr.attr.id(), attr.attr.range(), Some(attr.ctx))),
            Expr::Name(name) => Some((name.id(), name.range, Some(name.ctx))),
            Expr::NoneLiteral(none) => Some((&none_name, none.range(), None)),
            _ => None,
        };

        xref_info
            .filter(|(_, _, ctx)| ctx.is_none_or(|x| x.is_load()))
            .map(|(name, range, _)| python::XRefViaName {
                target: self.make_fq_name(name, None),
                source: to_span(range),
            })
    }

    fn add_xref(&mut self, xref: python::XRefViaName) {
        if let Some(spans) = self.facts.xrefs_by_target.get_mut(&xref.target) {
            spans.push(xref.source.clone());
        } else {
            self.facts
                .xrefs_by_target
                .insert(xref.target.clone(), vec![xref.source.clone()]);
        }
        self.facts.xrefs_via_name.push(xref);
    }

    fn xrefs_for_type_info(&self, expr: &Expr, xrefs: &mut Vec<python::XRefViaName>) {
        if let Some(xref) = self.make_xref(expr) {
            xrefs.push(xref)
        }

        expr.recurse(&mut |x| self.xrefs_for_type_info(x, xrefs));
    }

    fn type_info(&self, annotation: Option<&Expr>) -> Option<python::TypeInfo> {
        annotation.map(|type_annotation| {
            let lined_buffer = self.module.lined_buffer();
            let mut xrefs = vec![];
            type_annotation.visit(&mut |expr| self.xrefs_for_type_info(expr, &mut xrefs));
            python::TypeInfo {
                displayType: python::Type::new(
                    lined_buffer.code_at(type_annotation.range()).to_owned(),
                ),
                xrefs,
            }
        })
    }

    fn variable_info(
        &self,
        name: python::Name,
        range: TextRange,
        container: Option<&python::DeclarationContainer>,
        type_info: Option<python::TypeInfo>,
        top_level_decl: Option<python::Declaration>,
    ) -> DeclarationInfo {
        let variable_declaration = python::VariableDeclaration::new(name);
        let variable_definition = python::VariableDefinition::new(
            variable_declaration.clone(),
            type_info,
            container.cloned(),
        );

        DeclarationInfo {
            declaration: python::Declaration::variable(variable_declaration),
            decl_span: to_span(range),
            definition: Some(python::Definition::variable(variable_definition)),
            def_span: Some(to_span(range)),
            top_level_decl,
            docstring_range: None,
        }
    }

    fn parameter_info(
        &self,
        param: &Parameter,
        value: Option<String>,
        container: &python::DeclarationContainer,
        top_level_declaration: Option<&python::Declaration>,
        decl_infos: &mut Vec<DeclarationInfo>,
    ) -> python::Parameter {
        let type_info: Option<python::TypeInfo> = self.type_info(param.annotation());

        decl_infos.push(self.variable_info(
            self.make_fq_name_for_declaration(param.name.id(), container, ScopeType::Local),
            param.range(),
            None,
            type_info.clone(),
            top_level_declaration.cloned(),
        ));
        python::Parameter {
            name: python::Name::new(param.name().to_string()),
            typeInfo: type_info,
            value,
        }
    }

    fn parameter_with_default_info(
        &self,
        parameter_with_default: &ParameterWithDefault,
        top_level_declaration: Option<&python::Declaration>,
        container: &python::DeclarationContainer,
        decl_infos: &mut Vec<DeclarationInfo>,
    ) -> python::Parameter {
        let lined_buffer = self.module.lined_buffer();
        let value = parameter_with_default
            .default
            .as_ref()
            .map(|x| lined_buffer.code_at(x.range()).to_owned());
        self.parameter_info(
            &parameter_with_default.parameter,
            value,
            container,
            top_level_declaration,
            decl_infos,
        )
    }

    fn function_facts(
        &mut self,
        func: &StmtFunctionDef,
        func_declaration: python::FunctionDeclaration,
        container: &python::DeclarationContainer,
        params_top_level_decl: Option<&python::Declaration>,
    ) -> Vec<DeclarationInfo> {
        let declaration = python::Declaration::func(func_declaration.clone());
        let params = &func.parameters;

        let mut decl_infos = vec![];
        let args = params
            .args
            .iter()
            .map(|x| {
                self.parameter_with_default_info(
                    x,
                    params_top_level_decl,
                    container,
                    &mut decl_infos,
                )
            })
            .collect();

        let pos_only_args = params
            .posonlyargs
            .iter()
            .map(|x| {
                self.parameter_with_default_info(
                    x,
                    params_top_level_decl,
                    container,
                    &mut decl_infos,
                )
            })
            .collect();

        let kwonly_args = params
            .kwonlyargs
            .iter()
            .map(|x| {
                self.parameter_with_default_info(
                    x,
                    params_top_level_decl,
                    container,
                    &mut decl_infos,
                )
            })
            .collect();

        let star_arg = params.vararg.as_ref().map(|x| {
            self.parameter_info(
                x.as_ref(),
                None,
                container,
                params_top_level_decl,
                &mut decl_infos,
            )
        });

        let star_kwarg = params.kwarg.as_ref().map(|x| {
            self.parameter_info(
                x.as_ref(),
                None,
                container,
                params_top_level_decl,
                &mut decl_infos,
            )
        });

        let func_definition = python::FunctionDefinition::new(
            func_declaration,
            func.is_async,
            self.type_info(func.returns.as_ref().map(|x| x.as_ref())),
            args,
            Some(pos_only_args),
            Some(kwonly_args),
            star_arg,
            star_kwarg,
            Some(self.make_decorators(&func.decorator_list)),
            Some(container.clone()),
        );

        decl_infos.push(DeclarationInfo {
            declaration,
            decl_span: to_span(range_without_decorators(func.range, &func.decorator_list)),
            definition: Some(python::Definition::func(func_definition)),
            def_span: Some(to_span(func.range)),
            top_level_decl: None,
            docstring_range: Docstring::range_from_stmts(&func.body),
        });

        decl_infos
    }

    fn variable_facts(
        &mut self,
        expr: &Expr,
        range: TextRange,
        annotation: Option<&Expr>,
        container: &python::DeclarationContainer,
        globals: &SmallSet<Name>,
        nonlocals: &SmallSet<Name>,
        def_infos: &mut Vec<DeclarationInfo>,
    ) {
        if let Some(name) = expr.as_name_expr() {
            let scope_type = if globals.contains(&name.id) {
                ScopeType::Global
            } else if nonlocals.contains(&name.id) {
                ScopeType::Nonlocal
            } else {
                ScopeType::Local
            };
            def_infos.push(self.variable_info(
                self.make_fq_name_for_declaration(&name.id, container, scope_type),
                range,
                Some(container),
                self.type_info(annotation),
                None,
            ));
        }
        expr.recurse(&mut |expr| {
            self.variable_facts(
                expr, range, annotation, container, globals, nonlocals, def_infos,
            )
        });
    }

    fn import_facts(
        &mut self,
        imports: &Vec<Alias>,
        range: TextRange,
        from_module_id: Option<&Identifier>,
        level: u32,
    ) -> Vec<DeclarationInfo> {
        let from_module_name = from_module_id.map(|x| x.id());
        let from_module = if level > 0 {
            self.module_name
                .new_maybe_relative(self.module.path().is_init(), level, from_module_name)
                .map(|x| x.to_string())
        } else {
            from_module_name.map(|x| x.to_string())
        };

        let from_module_fact = python::Name::new(from_module.clone().unwrap_or_default());
        if let Some(module) = from_module_id {
            self.add_xref(python::XRefViaName {
                target: from_module_fact.clone(),
                source: to_span(module.range()),
            });
        }

        let mut decl_infos = vec![];
        for import in imports {
            let from_name = &import.name.id;
            let star_import = "*";

            if *from_name.as_str() == *star_import {
                let import_star = python::ImportStarStatement::new(
                    from_module_fact.clone(),
                    self.facts.module.clone(),
                );
                self.facts
                    .import_star_locations
                    .push(python::ImportStarLocation::new(
                        import_star,
                        self.facts.file.clone(),
                        to_span(range),
                    ));
            } else {
                let as_name = import.asname.as_ref().map_or(from_name, |x| &x.id);

                let from_name_fact = self.make_fq_name(from_name, from_module.as_deref());
                self.add_xref(python::XRefViaName {
                    target: from_name_fact.clone(),
                    source: to_span(import.name.range()),
                });

                let import_fact = python::ImportStatement::new(
                    from_name_fact,
                    self.make_fq_name(as_name, Some(&self.module_name.to_string())),
                );

                decl_infos.push(DeclarationInfo {
                    declaration: python::Declaration::imp(import_fact),
                    decl_span: to_span(import.name.range),
                    definition: None,
                    def_span: None,
                    top_level_decl: None,
                    docstring_range: None,
                });
            }
        }

        decl_infos
    }

    fn arg_string_lit(&self, argument: &Expr) -> Option<python::Argument> {
        argument.as_string_literal_expr().map(|str_lit| {
            python::Argument::lit(python::StringLiteral::new(str_lit.value.to_string()))
        })
    }

    fn file_call_facts(&mut self, call: &ExprCall) {
        let callee_span = to_span(call.range());
        let mut call_args: Vec<python::CallArgument> = call
            .arguments
            .args
            .iter()
            .map(|arg| python::CallArgument {
                label: None,
                span: to_span(arg.range()),
                argument: self.arg_string_lit(arg),
            })
            .collect();

        let keyword_args = call
            .arguments
            .keywords
            .iter()
            .map(|keyword| python::CallArgument {
                label: keyword
                    .arg
                    .as_ref()
                    .map(|id| self.make_fq_name(id.id(), None)),
                span: to_span(keyword.range()),
                argument: self.arg_string_lit(&keyword.value),
            });

        call_args.extend(keyword_args);

        self.facts.file_calls.push(python::FileCall::new(
            self.facts.file.clone(),
            callee_span,
            call_args,
        ));
    }

    fn callee_to_caller_facts(&mut self, call: &ExprCall, caller: &python::FunctionDeclaration) {
        let caller_fact = caller.key.name.clone();
        let callee_name = match call.func.as_ref() {
            Expr::Attribute(attr) => Some(attr.attr.id()),
            Expr::Name(expr_name) => Some(expr_name.id()),
            _ => None,
        };
        if let Some(name) = callee_name {
            let callee_fact = self.make_fq_name(name, None);
            self.facts
                .callee_to_callers
                .push(python::CalleeToCaller::new(callee_fact, caller_fact));
        }
    }

    fn generate_facts_from_exprs(&mut self, expr: &Expr, container: &python::DeclarationContainer) {
        if let Some(call) = expr.as_call_expr() {
            self.file_call_facts(call);
            if let python::DeclarationContainer::func(caller) = container {
                self.callee_to_caller_facts(call, caller);
            }
        };
        if let Some(xref) = self.make_xref(expr) {
            self.add_xref(xref);
        }
        expr.recurse(&mut |s| self.generate_facts_from_exprs(s, container));
    }

    fn visit_exprs(&mut self, node: &impl Visit<Expr>, container: &python::DeclarationContainer) {
        node.visit(&mut |expr| self.generate_facts_from_exprs(expr, container));
    }

    fn generate_facts(
        &mut self,
        stmt: &Stmt,
        container: &python::DeclarationContainer,
        top_level_decl: &python::Declaration,
        globals: &SmallSet<Name>,
        nonlocals: &SmallSet<Name>,
    ) {
        let mut new_container = None;
        let mut new_top_level_decl = None;
        let mut child_globals = SmallSet::new();
        let mut child_nonlocals = SmallSet::new();
        let mut is_new_local_scope = false;
        let mut decl_infos = vec![];
        match stmt {
            Stmt::ClassDef(cls) => {
                let cls_fq_name =
                    self.make_fq_name_for_declaration(&cls.name.id, container, ScopeType::Local);
                let cls_declaration = python::ClassDeclaration::new(cls_fq_name, None);
                let decl_info = self.class_facts(cls, cls_declaration.clone(), container.clone());
                self.visit_exprs(&cls.decorator_list, container);
                self.visit_exprs(&cls.type_params, container);
                self.visit_exprs(&cls.arguments, container);
                if let python::Declaration::module(_) = top_level_decl {
                    new_top_level_decl = Some(decl_info.declaration.clone());
                }

                new_container = Some(python::DeclarationContainer::cls(cls_declaration));
                decl_infos.push(decl_info);
                is_new_local_scope = true;
                gather_nonlocal_variables(&cls.body, &mut child_globals, &mut child_nonlocals);
            }
            Stmt::FunctionDef(func) => {
                let func_fq_name =
                    self.make_fq_name_for_declaration(&func.name.id, container, ScopeType::Local);
                let func_declaration = python::FunctionDeclaration::new(func_fq_name);
                if let python::Declaration::module(_) = top_level_decl {
                    new_top_level_decl = Some(python::Declaration::func(func_declaration.clone()));
                }
                let mut func_decl_infos = self.function_facts(
                    func,
                    func_declaration.clone(),
                    container,
                    new_top_level_decl.as_ref(),
                );
                self.visit_exprs(&func.decorator_list, container);
                self.visit_exprs(&func.type_params, container);
                self.visit_exprs(&func.parameters, container);
                self.visit_exprs(&func.returns, container);
                new_container = Some(python::DeclarationContainer::func(func_declaration));
                decl_infos.append(&mut func_decl_infos);
                is_new_local_scope = true;
                gather_nonlocal_variables(&func.body, &mut child_globals, &mut child_nonlocals);
            }
            Stmt::Assign(assign) => {
                assign.targets.visit(&mut |target| {
                    self.variable_facts(
                        target,
                        assign.range(),
                        None,
                        container,
                        globals,
                        nonlocals,
                        &mut decl_infos,
                    )
                });
                self.visit_exprs(&assign.value, container);
            }
            Stmt::AnnAssign(assign) => {
                self.variable_facts(
                    &assign.target,
                    assign.range(),
                    Some(&assign.annotation),
                    container,
                    globals,
                    nonlocals,
                    &mut decl_infos,
                );
                self.visit_exprs(&assign.annotation, container);
                self.visit_exprs(&assign.value, container);
            }
            Stmt::AugAssign(assign) => {
                self.variable_facts(
                    &assign.target,
                    assign.range(),
                    None,
                    container,
                    globals,
                    nonlocals,
                    &mut decl_infos,
                );
                self.visit_exprs(&assign.value, container);
            }
            Stmt::Import(import) => {
                let mut imp_decl_infos = self.import_facts(&import.names, import.range, None, 0);
                decl_infos.append(&mut imp_decl_infos);
            }
            Stmt::ImportFrom(import) => {
                let mut imp_decl_infos = self.import_facts(
                    &import.names,
                    import.range,
                    import.module.as_ref(),
                    import.level,
                );
                decl_infos.append(&mut imp_decl_infos);
            }
            Stmt::For(stmt_for) => {
                stmt_for.target.visit(&mut |target| {
                    self.variable_facts(
                        target,
                        target.range(),
                        None,
                        container,
                        globals,
                        nonlocals,
                        &mut decl_infos,
                    )
                });
                self.visit_exprs(&stmt_for.iter, container);
            }
            Stmt::While(stmt_while) => self.visit_exprs(&stmt_while.test, container),
            Stmt::If(stmt_if) => {
                self.visit_exprs(&stmt_if.test, container);
                for x in &stmt_if.elif_else_clauses {
                    self.visit_exprs(&x.test, container);
                }
            }
            Stmt::With(stmt_with) => {
                for item in &stmt_with.items {
                    self.visit_exprs(&item.context_expr, container);
                    item.optional_vars.visit(&mut |target| {
                        self.variable_facts(
                            target,
                            target.range(),
                            None,
                            container,
                            globals,
                            nonlocals,
                            &mut decl_infos,
                        )
                    });
                }
            }
            Stmt::Match(stmt_match) => {
                self.visit_exprs(&stmt_match.subject, container);
                for x in &stmt_match.cases {
                    self.visit_exprs(&x.guard, container);
                    self.visit_exprs(&x.pattern, container);
                }
            }
            Stmt::Try(stmt_try) => {
                stmt_try.handlers.iter().for_each(|x| match x {
                    ExceptHandler::ExceptHandler(x) => self.visit_exprs(&x.type_, container),
                });
            }
            _ => self.visit_exprs(stmt, container),
        }
        for decl_info in decl_infos {
            self.declaration_facts(decl_info, top_level_decl);
        }

        let mut new_scope_globals = globals;
        let mut new_scope_locals = nonlocals;
        if is_new_local_scope {
            new_scope_globals = &child_globals;
            new_scope_locals = &child_nonlocals;
        }
        stmt.recurse(&mut |x| {
            self.generate_facts(
                x,
                new_container.as_ref().unwrap_or(container),
                new_top_level_decl.as_ref().unwrap_or(top_level_decl),
                new_scope_globals,
                new_scope_locals,
            )
        });
    }
}

impl Glean {
    pub fn new(transaction: &Transaction, handle: &Handle) -> Self {
        let ast = &*transaction.get_ast(handle).unwrap();
        let mut glean_state = GleanState::new(transaction, handle);

        let file_language_fact =
            src::FileLanguage::new(glean_state.file_fact(), src::Language::Python);
        let digest_fact = glean_state.digest_fact();
        let file_lines = glean_state.file_lines_fact();

        let container = python::DeclarationContainer::module(glean_state.module_fact());
        let top_level_decl = python::Declaration::module(glean_state.module_fact());
        glean_state.module_facts(ast.range, &top_level_decl);
        ast.body.visit(&mut |stmt: &Stmt| {
            glean_state.generate_facts(
                stmt,
                &container,
                &top_level_decl,
                &SmallSet::new(),
                &SmallSet::new(),
            )
        });

        let file_fact = glean_state.file_fact();
        let facts = glean_state.facts;

        let xrefs_via_name_by_file_fact =
            python::XRefsViaNameByFile::new(file_fact.clone(), facts.xrefs_via_name.to_owned());

        let xrefs_by_target: Vec<python::XRefsViaNameByTarget> = facts
            .xrefs_by_target
            .into_iter()
            .map(|(target, spans)| {
                python::XRefsViaNameByTarget::new(
                    target.to_owned(),
                    file_fact.clone(),
                    spans.to_owned(),
                )
            })
            .collect();

        let entries = vec![
            GleanEntry::SchemaId {
                schema_id: builtin::SCHEMA_ID.to_owned(),
            },
            GleanEntry::Predicate {
                predicate: python::Name::GLEAN_name(),
                facts: vec![json(python::Name::new("".to_owned()))],
            },
            GleanEntry::Predicate {
                predicate: python::Module::GLEAN_name(),
                facts: facts.modules.into_iter().map(json).collect(),
            },
            GleanEntry::Predicate {
                predicate: src::FileLanguage::GLEAN_name(),
                facts: vec![json(file_language_fact)],
            },
            GleanEntry::Predicate {
                predicate: src::FileLines::GLEAN_name(),
                facts: vec![json(file_lines)],
            },
            GleanEntry::Predicate {
                predicate: digest::FileDigest::GLEAN_name(),
                facts: vec![json(digest_fact)],
            },
            GleanEntry::Predicate {
                predicate: python::DeclarationLocation::GLEAN_name(),
                facts: facts.decl_locations.into_iter().map(json).collect(),
            },
            GleanEntry::Predicate {
                predicate: python::DefinitionLocation::GLEAN_name(),
                facts: facts.def_locations.into_iter().map(json).collect(),
            },
            GleanEntry::Predicate {
                predicate: python::ImportStarLocation::GLEAN_name(),
                facts: facts.import_star_locations.into_iter().map(json).collect(),
            },
            GleanEntry::Predicate {
                predicate: python::FileCall::GLEAN_name(),
                facts: facts.file_calls.into_iter().map(json).collect(),
            },
            GleanEntry::Predicate {
                predicate: python::CalleeToCaller::GLEAN_name(),
                facts: facts.callee_to_callers.into_iter().map(json).collect(),
            },
            GleanEntry::Predicate {
                predicate: python::ContainingTopLevelDeclaration::GLEAN_name(),
                facts: facts
                    .containing_top_level_declarations
                    .into_iter()
                    .map(json)
                    .collect(),
            },
            GleanEntry::Predicate {
                predicate: python::XRefsViaNameByFile::GLEAN_name(),
                facts: vec![json(xrefs_via_name_by_file_fact)],
            },
            GleanEntry::Predicate {
                predicate: python::XRefsViaNameByTarget::GLEAN_name(),
                facts: xrefs_by_target.into_iter().map(json).collect(),
            },
            GleanEntry::Predicate {
                predicate: python::DeclarationDocstring::GLEAN_name(),
                facts: facts.declaration_docstrings.into_iter().map(json).collect(),
            },
        ];
        // TODO(@rubmary) Add SName and NameToSName predicates

        Glean { entries }
    }
}
