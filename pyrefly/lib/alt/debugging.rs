use pyrefly_util::display::DisplayWithCtx;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::binding::binding::Binding;
use crate::binding::binding::Key;
use crate::binding::binding::Keyed;
use crate::binding::bindings::BindingEntry;
use crate::binding::bindings::BindingTable;
use crate::binding::table::TableKeyed;
use crate::graph::index::Idx;

/// Debugging helpers for the AnswersSolver.
///
/// These are all string-returning functions, which make them potentially less efficient
/// but more convienient than `Display` implementations because they are easy to use
/// for string-based comparisons for filtered debugging.
///
/// For example, one useful snippet in unit tests is:
///   let debug = self.show_current_module() == "main";
///   if debug {
///      ... dump some information that would be too verbose if printed for stdlib modules ...
///   }
#[expect(dead_code)]
impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn show_idx<K>(&self, idx: Idx<K>) -> String
    where
        K: Keyed,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        format!(
            "{}",
            self.bindings()
                .idx_to_key(idx)
                .display_with(self.module_info())
        )
    }

    pub fn show_binding_generic<K>(&self, binding: &K::Value) -> String
    where
        K: Keyed,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        format!("{}", binding.display_with(self.bindings()))
    }

    pub fn show_binding(&self, binding: &Binding) -> String {
        self.show_binding_generic::<Key>(binding)
    }

    pub fn show_current_module(&self) -> String {
        format!("{}", self.module_info().name())
    }
}
