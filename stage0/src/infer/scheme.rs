use std::collections::BTreeSet;

use super::types::{Type, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeScheme {
    pub(crate) quantifiers: Vec<TypeVar>,
    pub(crate) body: Type,
}

impl TypeScheme {
    pub(crate) fn monomorphic(body: Type) -> Self {
        Self {
            quantifiers: Vec::new(),
            body,
        }
    }

    pub(crate) fn free_vars(&self) -> BTreeSet<TypeVar> {
        let mut vars = self.body.free_vars();
        for var in &self.quantifiers {
            vars.remove(var);
        }
        vars
    }
}
