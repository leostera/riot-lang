use std::collections::BTreeMap;

use super::scheme::TypeScheme;
use super::types::TypeVar;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ValueBinding {
    pub(crate) scheme: TypeScheme,
    pub(crate) ordinal: usize,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    values: BTreeMap<String, ValueBinding>,
}

#[derive(Debug, Clone)]
pub(crate) struct Env {
    prelude: Scope,
    scopes: Vec<Scope>,
    next_ordinal: usize,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            prelude: Scope::default(),
            scopes: vec![Scope::default()],
            next_ordinal: 0,
        }
    }
}

impl Env {
    pub(crate) fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub(crate) fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub(crate) fn add_value(&mut self, name: impl Into<String>, scheme: TypeScheme) {
        let binding = ValueBinding {
            scheme,
            ordinal: self.next_ordinal,
        };
        self.next_ordinal += 1;

        let current = self
            .scopes
            .last_mut()
            .expect("inference environment always has a root scope");
        current.values.insert(name.into(), binding);
    }

    pub(crate) fn add_prelude_value(&mut self, name: impl Into<String>, scheme: TypeScheme) {
        self.prelude
            .values
            .insert(name.into(), ValueBinding { scheme, ordinal: 0 });
    }

    pub(crate) fn get_value(&self, name: &str) -> Option<&TypeScheme> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.values.get(name).map(|binding| &binding.scheme))
            .or_else(|| self.prelude.values.get(name).map(|binding| &binding.scheme))
    }

    pub(crate) fn exported_values(&self) -> Vec<(String, TypeScheme)> {
        let root = self
            .scopes
            .first()
            .expect("inference environment always has a root scope");
        let mut values = root.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(_, binding)| binding.ordinal);
        values
            .into_iter()
            .map(|(name, binding)| (name.clone(), binding.scheme.clone()))
            .collect()
    }

    pub(crate) fn free_vars(&self) -> std::collections::BTreeSet<TypeVar> {
        self.scopes
            .iter()
            .chain(std::iter::once(&self.prelude))
            .flat_map(|scope| scope.values.values())
            .flat_map(|binding| binding.scheme.free_vars())
            .collect()
    }
}
