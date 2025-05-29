use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

use pyrefly_derive::TypeEq;
use pyrefly_util::visit::VisitMut;
use starlark_map::small_map::SmallMap;

use crate::types::type_var::PreInferenceVariance;
use crate::types::type_var::Variance;
use crate::types::types::Type;

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

pub fn pre_to_post_variance(pre_variance: PreInferenceVariance) -> Variance {
    match pre_variance {
        PreInferenceVariance::PCovariant => Variance::Covariant,
        PreInferenceVariance::PContravariant => Variance::Contravariant,
        PreInferenceVariance::PInvariant => Variance::Invariant,
        // TODO: we should infer variance for this case
        PreInferenceVariance::PUndefined => Variance::Invariant,
    }
}
