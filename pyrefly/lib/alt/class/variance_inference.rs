use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::sync::Arc;

use pyrefly_derive::TypeEq;
use pyrefly_util::visit::VisitMut;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::class::variance_inference::variance_visitor::Injectivity;
use crate::alt::class::variance_inference::variance_visitor::TParamArray;
use crate::alt::class::variance_inference::variance_visitor::VarianceEnv;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::types::class::Class;
use crate::types::stdlib::Stdlib;
use crate::types::type_var::PreInferenceVariance;
use crate::types::type_var::Variance;
use crate::types::types::TParam;
use crate::types::types::TParams;
use crate::types::types::Type;

// TODO zeina: This algorithm still needs to handle class properties.
// To do so, we have to thread class property information to the algorithm
// After this, we simply call on_type on all class property signatures
// we have to special case some properties like __init__ and private properties

#[derive(Debug, Clone, PartialEq, Eq, TypeEq)]
pub struct VarianceMap(pub SmallMap<String, Variance>);

impl VisitMut<Type> for VarianceMap {
    fn visit_mut(&mut self, _visitor: &mut dyn FnMut(&mut Type)) {
        // No-op: VarianceMap does not contain any Type
    }

    fn recurse_mut(&mut self, _visitor: &mut dyn FnMut(&mut Type)) {
        // No-op: VarianceMap does not contain any Type
    }
}

impl Display for VarianceMap {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{{")?;
        for (key, value) in self.0.iter() {
            write!(f, "{}: {}, ", key, value)?;
        }
        write!(f, "}}")
    }
}

pub mod variance_visitor {
    use std::sync::Arc;

    use starlark_map::small_map::SmallMap;

    use crate::alt::types::class_metadata::ClassMetadata;
    use crate::types::class::Class;
    use crate::types::stdlib::Stdlib;
    use crate::types::type_var::Variance;
    use crate::types::types::Type;
    pub type Injectivity = bool;
    pub type TypeParam = (String, Variance, Injectivity);
    pub type TParamArray = Vec<TypeParam>;

    // A map from class name to tparam environment
    pub type VarianceEnv = SmallMap<String, TParamArray>;

    pub fn on_class(
        class: &Class,
        on_edge: &mut impl FnMut(&Class) -> TParamArray,
        on_var: &mut impl FnMut(&str, Variance, Injectivity),
        get_metadata: &impl Fn(&Class) -> Arc<ClassMetadata>,
        _stdlib: &Stdlib, // todo zeina: check if we still need this arg to get class properties
    ) {
        fn on_type(
            variance: Variance,
            inj: Injectivity,
            typ: &Type,
            on_edge: &mut impl FnMut(&Class) -> TParamArray,
            on_var: &mut impl FnMut(&str, Variance, Injectivity),
        ) {
            match typ {
                Type::Type(t) => {
                    on_type(variance, inj, t, on_edge, on_var);
                }

                Type::ClassType(class) => {
                    let params: Vec<(String, Variance, bool)> = on_edge(class.class_object());

                    let targs = class.targs().as_slice();

                    for (i, param) in params.iter().enumerate() {
                        if let Some(ty) = targs.get(i) {
                            let (_, variance_param, inj_param) = param;

                            on_type(
                                variance.compose(*variance_param),
                                *inj_param,
                                ty,
                                on_edge,
                                on_var,
                            );
                        }
                    }
                }

                Type::Quantified(q) => {
                    on_var(q.name().as_str(), variance, inj);
                }

                _ => {}
            }
        }

        let metadata = get_metadata(class);
        let base_types = metadata.bases_with_metadata();

        for base_type in base_types {
            on_type(
                Variance::Covariant,
                true,
                &base_type.0.clone().to_type(),
                on_edge,
                on_var,
            );
        }
    }
}

fn default_variance_and_inj(gp: &TParam, contains_bivariant: &mut bool) -> (Variance, Injectivity) {
    let variance = pre_to_post_variance(gp.variance, contains_bivariant);
    let inj = match variance {
        Variance::Bivariant => false,
        _ => true,
    };
    (variance, inj)
}

fn from_gp_to_decl(gp: &TParam, contains_bivariant: &mut bool) -> (String, Variance, Injectivity) {
    let (variance, inj) = default_variance_and_inj(gp, contains_bivariant);
    (gp.name().as_str().to_owned(), variance, inj)
}

pub fn params_from_gp(tparams: &[TParam], contains_bivariant: &mut bool) -> TParamArray {
    let mut params: Vec<(String, Variance, Injectivity)> = tparams
        .iter()
        .map(|param| (param.name().as_str().to_owned(), Variance::Bivariant, false))
        .collect();

    for (i, param) in tparams.iter().enumerate() {
        let (name, variance, inj) = from_gp_to_decl(param, contains_bivariant);
        params[i] = (name, variance, inj);
    }
    params
}

pub fn convert_gp_to_map(
    tparams: &TParams,
    contains_bivariant: &mut bool,
) -> SmallMap<String, Variance> {
    let mut lookup = SmallMap::new();

    for param in tparams.iter() {
        lookup.insert(
            param.name().as_str().to_owned(),
            pre_to_post_variance(param.variance, contains_bivariant),
        );
    }

    lookup
}

fn pre_to_post_variance(
    pre_variance: PreInferenceVariance,
    contains_bivariant: &mut bool,
) -> Variance {
    match pre_variance {
        PreInferenceVariance::PCovariant => Variance::Covariant,
        PreInferenceVariance::PContravariant => Variance::Contravariant,
        PreInferenceVariance::PInvariant => Variance::Invariant,
        PreInferenceVariance::PUndefined => {
            *contains_bivariant = true;
            Variance::Bivariant
        }
    }
}

fn loop_fn<'a>(
    class: &'a Class,
    environment: &mut VarianceEnv,
    contains_bivariant: &mut bool,
    get_metadata: &impl Fn(&Class) -> Arc<ClassMetadata>,
    stdlib: &Stdlib,
) -> TParamArray {
    let class_name = class.name().as_str().to_owned();

    if let Some(params) = environment.get(&class_name) {
        return params.clone();
    }

    let params: Vec<(String, Variance, bool)> =
        params_from_gp(class.tparams().as_vec(), contains_bivariant);

    environment.insert(class_name.clone(), params.clone());
    let mut on_var = |_name: &str, _variance: Variance, _inj: Injectivity| {};

    // get the variance results of a given class c
    let mut on_edge = |c: &Class| loop_fn(c, environment, contains_bivariant, get_metadata, stdlib);

    variance_visitor::on_class(class, &mut on_edge, &mut on_var, get_metadata, stdlib);

    params
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn variance_map(&self, class: &Class) -> Arc<VarianceMap> {
        let mut contains_bivariant: bool = false;

        let post_inference_initial = convert_gp_to_map(class.tparams(), &mut contains_bivariant);

        fn to_map(
            params: &TParamArray,
            post_inference_initial: &SmallMap<String, Variance>,
        ) -> SmallMap<String, Variance> {
            let mut map = SmallMap::new();

            for (name, variance, inj) in params.iter() {
                let inferred_variance = match post_inference_initial.get(name) {
                    Some(&Variance::Bivariant) => match (*variance, *inj) {
                        (_, false) => Variance::Bivariant,
                        (Variance::Bivariant, _) => Variance::Bivariant,
                        (Variance::Covariant, _) => Variance::Covariant,
                        (Variance::Contravariant, _) => Variance::Contravariant,
                        (Variance::Invariant, _) => Variance::Invariant,
                    },
                    Some(&res) => res,
                    None => panic!(
                        "Impossible. Class name {} must be present in variance map",
                        name
                    ),
                };
                map.insert(name.clone(), inferred_variance);
            }
            map
        }

        fn fixpoint<'a, Ans: LookupAnswer>(
            solver: &AnswersSolver<'a, Ans>,
            class: &Class,
            env: &VarianceEnv,
        ) -> VarianceEnv {
            let mut environment_prime: VarianceEnv = SmallMap::new();
            let mut changed = false;

            for (class_name, params) in env.iter() {
                let mut params_prime = params.clone();

                let metadata = solver.get_metadata_for_class(class);

                let ancestor_class = metadata.ancestors(solver.stdlib).find(|ancestor| {
                    let class_obj = ancestor.class_object();
                    class_obj.name() == class_name
                });

                // RFC: In pyre1, we hit this point when we encounter a parametric type
                // but here, we are coming from a class type, which should be int
                // as shown here, I am exiting the fixpoint when the class isn't found
                let my_class = if let Some(ancestor) = ancestor_class {
                    ancestor.class_object()
                } else if class.name() == class_name {
                    class
                } else {
                    continue;
                };

                let mut on_var = |name: &str, variance: Variance, inj: Injectivity| {
                    for (n, variance_prime, inj_prime) in params_prime.iter_mut() {
                        if n == name {
                            *variance_prime = variance.union(*variance_prime);
                            *inj_prime = *inj_prime || inj;
                        }
                    }
                };

                let mut on_edge =
                    |c: &Class| env.get(c.name().as_str()).cloned().unwrap_or_else(Vec::new);

                variance_visitor::on_class(
                    my_class,
                    &mut on_edge,
                    &mut on_var,
                    &|c| solver.get_metadata_for_class(c),
                    solver.stdlib,
                );

                if params != &params_prime {
                    changed = true;
                }

                environment_prime.insert(class_name.clone(), params_prime);
            }

            if changed {
                fixpoint(solver, class, &environment_prime)
            } else {
                environment_prime
            }
        }

        if !contains_bivariant {
            Arc::new(VarianceMap(post_inference_initial))
        } else {
            let mut environment = VarianceEnv::new();

            loop_fn(
                class,
                &mut environment,
                &mut contains_bivariant,
                &|c| self.get_metadata_for_class(c),
                self.stdlib,
            );

            let environment = fixpoint(self, class, &environment);

            let class_name = class.name().as_str();

            let params = environment
                .get(class_name)
                .expect("class name must be present in environment");

            let class_variances = to_map(params, &post_inference_initial);
            Arc::new(VarianceMap(class_variances))
        }
    }
}
