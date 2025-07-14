/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_util::visit::Visit;
use ruff_python_ast::Alias;
use ruff_python_ast::ExceptHandler;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_python_ast::Identifier;
use ruff_python_ast::ModModule;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtClassDef;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::alt::answers::Answers;
use crate::binding::bindings::Bindings;
use crate::module::module_info::ModuleInfo;
use crate::report::glean::facts::*;
use crate::report::glean::schema::*;

fn hash(x: &[u8]) -> String {
    // Glean uses blake3
    blake3::hash(x).to_string()
}

struct Facts {
    file: src::File,
    module: python::Module,
    module_name: String,
    decl_locations: Vec<python::DeclarationLocation>,
    def_locations: Vec<python::DefinitionLocation>,
    import_star_locations: Vec<python::ImportStarLocation>,
    file_calls: Vec<python::FileCall>,
    callee_to_callers: Vec<python::CalleeToCaller>,
}

fn to_span(range: TextRange) -> src::ByteSpan {
    src::ByteSpan {
        start: range.start().to_u32().into(),
        length: range.len().to_u32().into(),
    }
}

impl Facts {
    fn new(file: src::File, module: python::Module, module_name: String) -> Facts {
        Facts {
            file,
            module,
            module_name,
            decl_locations: vec![],
            def_locations: vec![],
            import_star_locations: vec![],
            file_calls: vec![],
            callee_to_callers: vec![],
        }
    }

    fn decl_location_fact(
        &self,
        declaration: python::Declaration,
        range: TextRange,
    ) -> python::DeclarationLocation {
        python::DeclarationLocation::new(declaration.to_owned(), self.file.clone(), to_span(range))
    }

    fn def_location_fact(
        &self,
        definition: python::Definition,
        range: TextRange,
    ) -> python::DefinitionLocation {
        python::DefinitionLocation::new(definition.to_owned(), self.file.clone(), to_span(range))
    }

    fn make_fq_name(&self, name: &Name, module_name: Option<&str>) -> String {
        // TODO(@rubmary) create fully qualified name
        if let Some(module) = module_name {
            module.to_owned() + "." + name
        } else {
            name.to_string()
        }
    }

    fn module_facts(&mut self, module: &python::Module, range: TextRange) {
        self.decl_locations
            .push(self.decl_location_fact(python::Declaration::module(module.clone()), range));

        self.def_locations.push(self.def_location_fact(
            python::Definition::module(python::ModuleDefinition::new(module.clone())),
            range,
        ))
    }

    fn class_facts(&mut self, cls: &StmtClassDef) {
        let fqname = python::Name::new(self.make_fq_name(&cls.name.id, None));
        let cls_declaration = python::ClassDeclaration::new(fqname, None);

        let bases = if let Some(arguments) = &cls.arguments {
            arguments
                .args
                .iter()
                .filter_map(|expr| expr.as_name_expr())
                .map(|expr_name| {
                    python::ClassDeclaration::new(
                        python::Name::new(self.make_fq_name(&expr_name.id, None)),
                        None,
                    )
                })
                .collect()
        } else {
            vec![]
        };

        let cls_definition = python::ClassDefinition::new(
            cls_declaration.clone(),
            Some(bases),
            None,
            //TODO(@rubmary) Generate decorators and container for classes
            None,
            None,
        );

        self.decl_locations
            .push(self.decl_location_fact(python::Declaration::cls(cls_declaration), cls.range));
        self.def_locations
            .push(self.def_location_fact(python::Definition::cls(cls_definition), cls.range));
    }

    fn function_facts(&mut self, func: &StmtFunctionDef) {
        let fqname = python::Name::new(self.make_fq_name(&func.name.id, None));
        let func_declaration = python::FunctionDeclaration::new(fqname);

        let func_definition = python::FunctionDefinition::new(
            func_declaration.clone(),
            func.is_async,
            // TODO(@rubmary) generate additional fields
            None,
            vec![],
            None,
            None,
            None,
            None,
            None,
            None,
        );

        self.decl_locations
            .push(self.decl_location_fact(python::Declaration::func(func_declaration), func.range));

        self.def_locations
            .push(self.def_location_fact(python::Definition::func(func_definition), func.range));
    }

    fn variable_facts(&mut self, expr: &Expr, _type_info: Option<&Expr>) {
        // TODO(@rubmary) add type_info
        if let Some(name) = expr.as_name_expr() {
            let fqname = python::Name::new(self.make_fq_name(&name.id, None));
            let variable_declaration = python::VariableDeclaration::new(fqname);
            let variable_definition =
                python::VariableDefinition::new(variable_declaration.clone(), None, None);
            self.decl_locations.push(self.decl_location_fact(
                python::Declaration::variable(variable_declaration),
                name.range,
            ));
            self.def_locations.push(self.def_location_fact(
                python::Definition::variable(variable_definition),
                name.range,
            ))
        }
        expr.recurse(&mut |expr| self.variable_facts(expr, _type_info));
    }

    fn import_facts(&mut self, imports: &Vec<Alias>, from_module_id: &Option<Identifier>) {
        //TODO(@rubmary) Handle level for imports. Ex from ..a import A
        for import in imports {
            let from_module = from_module_id.as_ref().map(|module| module.as_str());
            let from_name = &import.name.id;
            let star_import = "*";

            if *from_name.as_str() == *star_import {
                let import_star = python::ImportStarStatement::new(
                    python::Name::new(from_module.unwrap_or_default().to_owned()),
                    self.module.clone(),
                );
                self.import_star_locations
                    .push(python::ImportStarLocation::new(
                        import_star,
                        self.file.clone(),
                        to_span(import.range()),
                    ));
            } else {
                let as_name = import.asname.as_ref().map_or(from_name, |x| &x.id);

                let import_fact = python::ImportStatement::new(
                    python::Name::new(self.make_fq_name(from_name, from_module)),
                    python::Name::new(self.make_fq_name(as_name, Some(&self.module_name))),
                );
                self.decl_locations.push(
                    self.decl_location_fact(python::Declaration::imp(import_fact), import.range()),
                );
            }
        }
    }

    fn file_call_facts(&mut self, call: &ExprCall) {
        let callee_span = to_span(call.func.range());
        // TODO(@rubmary) Generate `argument` value for CallArgument predicate
        let mut call_args: Vec<python::CallArgument> = call
            .arguments
            .args
            .iter()
            .map(|arg| python::CallArgument {
                label: None,
                span: to_span(arg.range()),
                argument: None,
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
                    .map(|id| python::Name::new(self.make_fq_name(id.id(), None))),
                span: to_span(keyword.range()),
                argument: None,
            });

        call_args.extend(keyword_args);

        self.file_calls.push(python::FileCall::new(
            self.file.clone(),
            callee_span,
            call_args,
        ));
    }

    fn callee_to_caller_facts(&mut self, call: &ExprCall, caller: &StmtFunctionDef) {
        let caller_fact = python::Name::new(self.make_fq_name(&caller.name.id, None));
        let callee_name = match call.func.as_ref() {
            Expr::Attribute(attr) => Some(attr.attr.id()),
            Expr::Name(expr_name) => Some(expr_name.id()),
            _ => None,
        };
        if let Some(name) = callee_name {
            let callee_fact = python::Name::new(self.make_fq_name(name, None));
            self.callee_to_callers
                .push(python::CalleeToCaller::new(callee_fact, caller_fact));
        }
    }

    fn generate_facts_from_exprs(&mut self, expr: &Expr, container: Option<&Stmt>) {
        if let Some(call) = expr.as_call_expr() {
            self.file_call_facts(call);
            if let Some(caller) = container.and_then(|p| p.as_function_def_stmt()) {
                self.callee_to_caller_facts(call, caller);
            }
        }
        expr.recurse(&mut |s| self.generate_facts_from_exprs(s, container));
    }

    fn visit_exprs(&mut self, node: &impl Visit<Expr>, container: Option<&Stmt>) {
        node.visit(&mut |expr| self.generate_facts_from_exprs(expr, container));
    }

    fn generate_facts(&mut self, stmt: &Stmt, container: Option<&Stmt>) {
        let mut new_container = container;
        match stmt {
            Stmt::ClassDef(cls) => {
                self.class_facts(cls);
                self.visit_exprs(&cls.decorator_list, container);
                self.visit_exprs(&cls.type_params, container);
                self.visit_exprs(&cls.arguments, container);
                new_container = Some(stmt);
            }
            Stmt::FunctionDef(func) => {
                self.function_facts(func);
                self.visit_exprs(&func.decorator_list, container);
                self.visit_exprs(&func.type_params, container);
                self.visit_exprs(&func.parameters, container);
                self.visit_exprs(&func.returns, container);
                new_container = Some(stmt);
            }
            Stmt::Assign(assign) => {
                assign
                    .targets
                    .visit(&mut |target| self.variable_facts(target, None));
                self.visit_exprs(&assign.value, container);
            }
            Stmt::AnnAssign(assign) => {
                self.variable_facts(&assign.target, Some(&assign.annotation));
                self.visit_exprs(&assign.annotation, container);
                self.visit_exprs(&assign.value, container);
            }
            Stmt::AugAssign(assign) => {
                self.variable_facts(&assign.target, None);
                self.visit_exprs(&assign.value, container);
            }
            Stmt::Import(import) => self.import_facts(&import.names, &None),
            Stmt::ImportFrom(import) => self.import_facts(&import.names, &import.module),
            Stmt::For(stmt_for) => {
                stmt_for
                    .target
                    .visit(&mut |target| self.variable_facts(target, None));
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
                    item.optional_vars
                        .visit(&mut |target| self.variable_facts(target, None));
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
        stmt.recurse(&mut |x| self.generate_facts(x, new_container));
    }
}

impl Glean {
    #[allow(unused_variables)]
    pub fn new(
        module_info: &ModuleInfo,
        ast: &ModModule,
        bindings: &Bindings,
        answers: &Answers,
    ) -> Self {
        let module_name = python::Name::new(module_info.name().as_str().to_owned());
        let module_fact = python::Module::new(module_name);
        let file_fact = src::File::new(module_info.path().to_string());

        let mut facts = Facts::new(
            file_fact.clone(),
            module_fact.clone(),
            module_info.name().to_string(),
        );

        facts.module_facts(&module_fact, ast.range);

        ast.body
            .visit(&mut |stmt: &Stmt| facts.generate_facts(stmt, None));

        let digest = digest::Digest {
            hash: hash(module_info.contents().as_bytes()),
            size: module_info.contents().len() as u64,
        };
        let digest_fact = digest::FileDigest::new(file_fact, digest);

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
                facts: vec![json(module_fact)],
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
        ];

        // TODO: Add many more predicates here.

        Glean { entries }
    }
}
