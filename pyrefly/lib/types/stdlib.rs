/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::name::Name;

use crate::module::module_name::ModuleName;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::class::TArgs;
use crate::types::types::Type;
use crate::PythonVersion;

#[derive(Debug, Clone)]
struct StdlibError {
    bootstrapping: bool,
    name: &'static str,
}

type StdlibResult<T> = Result<T, StdlibError>;

#[derive(Debug, Clone)]
pub struct Stdlib {
    str: StdlibResult<Class>,
    bool: StdlibResult<Class>,
    int: StdlibResult<Class>,
    bytes: StdlibResult<Class>,
    float: StdlibResult<Class>,
    complex: StdlibResult<Class>,
    slice: StdlibResult<Class>,
    base_exception: StdlibResult<Class>,
    /// Introduced in Python 3.11.
    base_exception_group: StdlibResult<Class>,
    /// Introduced in Python 3.11.
    exception_group: StdlibResult<Class>,
    list: StdlibResult<Class>,
    dict: StdlibResult<Class>,
    mapping: StdlibResult<Class>,
    set: StdlibResult<Class>,
    tuple: StdlibResult<Class>,
    iterable: StdlibResult<Class>,
    async_iterable: StdlibResult<Class>,
    generator: StdlibResult<Class>,
    async_generator: StdlibResult<Class>,
    awaitable: StdlibResult<Class>,
    coroutine: StdlibResult<Class>,
    type_var: StdlibResult<Class>,
    /// Defined in `typing_extensions` util 3.13, defined in `typing` since 3.10.
    /// After 3.13, `typing_extensions` reexports from `typing`.
    /// For 3.10 to 3.12 defined separately in both locations.
    param_spec: StdlibResult<Class>,
    /// Moved from `typing` to `typing_extensions` in 3.10.
    param_spec_args: StdlibResult<Class>,
    /// Moved from `typing` to `typing_extensions` in 3.10.
    param_spec_kwargs: StdlibResult<Class>,
    /// Defined in `typing_extensions` until 3.13, defined in `typing` since 3.11.
    /// After 3.13, `typing_extensions` reexports from `typing`.
    /// For 3.11 and 3.12 defined separately in both locations.
    type_var_tuple: StdlibResult<Class>,
    /// Defined in `typing_extensions` until 3.14, defined in `typing` since 3.12.
    /// After 3.14, `typing_extensions` reexports from `typing`.
    /// For 3.12 and 3.13 defined separately in both locations.
    type_alias_type: StdlibResult<Class>,
    traceback_type: StdlibResult<Class>,
    builtins_type: StdlibResult<Class>,
    /// Introduced in Python 3.10.
    ellipsis_type: StdlibResult<Class>,
    /// Moved from `_typeshed` to `types` in 3.10.
    none_type: StdlibResult<Class>,
    function_type: StdlibResult<Class>,
    method_type: StdlibResult<Class>,
    enum_meta: StdlibResult<Class>,
    enum_flag: StdlibResult<Class>,
    named_tuple: StdlibResult<Class>,
    property: StdlibResult<Class>,
    object: StdlibResult<ClassType>,
}

impl Stdlib {
    pub fn new(
        version: PythonVersion,
        lookup_class: &dyn Fn(ModuleName, &Name) -> Option<Class>,
    ) -> Self {
        Self::new_with_bootstrapping(false, version, lookup_class)
    }

    pub fn new_with_bootstrapping(
        bootstrapping: bool,
        version: PythonVersion,
        lookup_class: &dyn Fn(ModuleName, &Name) -> Option<Class>,
    ) -> Self {
        let builtins = ModuleName::builtins();
        let types = ModuleName::types();
        let typing = ModuleName::typing();
        let enum_ = ModuleName::enum_();

        let lookup_generic = |module: ModuleName, name: &'static str| {
            lookup_class(module, &Name::new_static(name)).ok_or(StdlibError {
                bootstrapping,
                name,
            })
        };
        let lookup_concrete = |module: ModuleName, name: &'static str| {
            lookup_generic(module, name).map(|obj| ClassType::new_for_stdlib(obj, TArgs::default()))
        };

        let none_location = if version >= PythonVersion::new(3, 10, 0) {
            types
        } else {
            ModuleName::from_str("_typeshed")
        };

        Self {
            str: lookup_generic(builtins, "str"),
            bool: lookup_generic(builtins, "bool"),
            int: lookup_generic(builtins, "int"),
            bytes: lookup_generic(builtins, "bytes"),
            float: lookup_generic(builtins, "float"),
            complex: lookup_generic(builtins, "complex"),
            slice: lookup_generic(builtins, "slice"),
            base_exception: lookup_generic(builtins, "BaseException"),
            base_exception_group: lookup_generic(builtins, "BaseExceptionGroup"),
            exception_group: lookup_generic(builtins, "ExceptionGroup"),
            list: lookup_generic(builtins, "list"),
            dict: lookup_generic(builtins, "dict"),
            set: lookup_generic(builtins, "set"),
            tuple: lookup_generic(builtins, "tuple"),
            builtins_type: lookup_generic(builtins, "type"),
            ellipsis_type: lookup_generic(types, "EllipsisType"),
            none_type: lookup_generic(none_location, "NoneType"),
            iterable: lookup_generic(typing, "Iterable"),
            async_iterable: lookup_generic(typing, "AsyncIterable"),
            generator: lookup_generic(typing, "Generator"),
            async_generator: lookup_generic(typing, "AsyncGenerator"),
            awaitable: lookup_generic(typing, "Awaitable"),
            coroutine: lookup_generic(typing, "Coroutine"),
            type_var: lookup_generic(typing, "TypeVar"),
            param_spec: lookup_generic(typing, "ParamSpec"),
            param_spec_args: lookup_generic(typing, "ParamSpecArgs"),
            param_spec_kwargs: lookup_generic(typing, "ParamSpecKwargs"),
            type_var_tuple: lookup_generic(typing, "TypeVarTuple"),
            type_alias_type: lookup_generic(typing, "TypeAliasType"),
            traceback_type: lookup_generic(types, "TracebackType"),
            function_type: lookup_generic(types, "FunctionType"),
            method_type: lookup_generic(types, "MethodType"),
            mapping: lookup_generic(typing, "Mapping"),
            enum_meta: lookup_generic(enum_, "EnumMeta"),
            enum_flag: lookup_generic(enum_, "Flag"),
            named_tuple: lookup_generic(typing, "NamedTuple"),
            property: lookup_generic(builtins, "property"),
            object: lookup_concrete(builtins, "object"),
        }
    }

    /// Create a new Stdlib with all types set to `Any``.
    ///
    /// This is needed because bootstrapping a `Stdlib` requires an `AnswersSolver` for the
    /// `lookup`, but `AnswersSolver` itself depends on `Stdlib`.
    ///
    /// It works because the lookups only need a tiny subset of all `AnswersSolver` functionality,
    /// none of which actually depends on `Stdlib`.
    pub fn for_bootstrapping() -> Stdlib {
        Self::new_with_bootstrapping(true, PythonVersion::default(), &|_, _| None)
    }

    fn unwrap<T>(x: &StdlibResult<T>) -> &T {
        match x {
            Ok(x) => x,
            Err(err) => {
                unreachable!(
                    "Stdlib missing class `{}`{}",
                    err.name,
                    if err.bootstrapping {
                        " (while bootstrapping)"
                    } else {
                        ""
                    },
                )
            }
        }
    }

    fn primitive(cls: &StdlibResult<Class>) -> ClassType {
        // Note: this construction will panic if we incorrectly mark a generic type as primitive.
        ClassType::new_for_stdlib(Self::unwrap(cls).dupe(), TArgs::default())
    }

    pub fn object(&self) -> &ClassType {
        Self::unwrap(&self.object)
    }

    pub fn bool(&self) -> ClassType {
        Self::primitive(&self.bool)
    }

    pub fn builtins_type(&self) -> ClassType {
        Self::primitive(&self.builtins_type)
    }

    pub fn enum_meta(&self) -> ClassType {
        Self::primitive(&self.enum_meta)
    }

    pub fn enum_flag(&self) -> ClassType {
        Self::primitive(&self.enum_flag)
    }

    pub fn named_tuple(&self) -> ClassType {
        Self::primitive(&self.named_tuple)
    }

    pub fn ellipsis_type(&self) -> ClassType {
        Self::primitive(&self.ellipsis_type)
    }

    pub fn none_type(&self) -> ClassType {
        Self::primitive(&self.none_type)
    }

    pub fn int(&self) -> ClassType {
        Self::primitive(&self.int)
    }

    pub fn float(&self) -> ClassType {
        Self::primitive(&self.float)
    }

    pub fn complex(&self) -> ClassType {
        Self::primitive(&self.complex)
    }

    pub fn bytes(&self) -> ClassType {
        Self::primitive(&self.bytes)
    }

    pub fn str(&self) -> ClassType {
        Self::primitive(&self.str)
    }

    pub fn slice(&self, start_ty: Type, stop_ty: Type, step_ty: Type) -> ClassType {
        Self::apply(&self.slice, vec![start_ty, stop_ty, step_ty])
    }

    pub fn base_exception(&self) -> ClassType {
        Self::primitive(&self.base_exception)
    }

    fn apply(cls: &StdlibResult<Class>, targs: Vec<Type>) -> ClassType {
        // Note: this construction will panic if we use `apply` with the wrong arity.
        ClassType::new_for_stdlib(Self::unwrap(cls).dupe(), TArgs::new(targs))
    }

    pub fn base_exception_group(&self, x: Type) -> ClassType {
        Self::apply(&self.base_exception_group, vec![x])
    }

    pub fn exception_group(&self, x: Type) -> ClassType {
        Self::apply(&self.exception_group, vec![x])
    }

    pub fn tuple(&self, x: Type) -> ClassType {
        Self::apply(&self.tuple, vec![x])
    }

    pub fn tuple_class_object(&self) -> Class {
        Self::unwrap(&self.tuple).dupe()
    }

    pub fn list(&self, x: Type) -> ClassType {
        Self::apply(&self.list, vec![x])
    }

    pub fn dict(&self, key: Type, value: Type) -> ClassType {
        Self::apply(&self.dict, vec![key, value])
    }

    pub fn mapping(&self, key: Type, value: Type) -> ClassType {
        Self::apply(&self.mapping, vec![key, value])
    }

    pub fn set(&self, x: Type) -> ClassType {
        Self::apply(&self.set, vec![x])
    }

    pub fn iterable(&self, x: Type) -> ClassType {
        Self::apply(&self.iterable, vec![x])
    }

    pub fn async_iterable(&self, x: Type) -> ClassType {
        Self::apply(&self.async_iterable, vec![x])
    }

    pub fn generator(&self, yield_ty: Type, send_ty: Type, return_ty: Type) -> ClassType {
        Self::apply(&self.generator, vec![yield_ty, send_ty, return_ty])
    }

    pub fn async_generator(&self, yield_ty: Type, send_ty: Type) -> ClassType {
        Self::apply(&self.async_generator, vec![yield_ty, send_ty])
    }

    pub fn awaitable(&self, x: Type) -> ClassType {
        Self::apply(&self.awaitable, vec![x])
    }

    pub fn coroutine(&self, yield_ty: Type, send_ty: Type, return_ty: Type) -> ClassType {
        Self::apply(&self.coroutine, vec![yield_ty, send_ty, return_ty])
    }

    pub fn type_var(&self) -> ClassType {
        Self::primitive(&self.type_var)
    }

    pub fn param_spec(&self) -> ClassType {
        Self::primitive(&self.param_spec)
    }

    pub fn type_var_tuple(&self) -> ClassType {
        Self::primitive(&self.type_var_tuple)
    }

    pub fn param_spec_args(&self) -> ClassType {
        Self::primitive(&self.param_spec_args)
    }

    pub fn param_spec_kwargs(&self) -> ClassType {
        Self::primitive(&self.param_spec_kwargs)
    }

    pub fn type_alias_type(&self) -> ClassType {
        Self::primitive(&self.type_alias_type)
    }

    pub fn traceback_type(&self) -> ClassType {
        Self::primitive(&self.traceback_type)
    }

    pub fn function_type(&self) -> ClassType {
        Self::primitive(&self.function_type)
    }

    pub fn method_type(&self) -> ClassType {
        Self::primitive(&self.method_type)
    }

    pub fn property(&self) -> ClassType {
        Self::primitive(&self.property)
    }
}
