use std::collections::HashMap;

use super::{
    scheme::Scheme,
    tst::{BindingId, EntityId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LevelId(usize);

impl LevelId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn root() -> Self {
        Self(0)
    }

    pub fn inc(self) -> Self {
        Self(self.0 + 1)
    }

    pub fn decr(self) -> Option<Self> {
        self.0.checked_sub(1).map(Self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntityKind {
    Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entity {
    pub id: EntityId,
    pub kind: EntityKind,
    pub binding: Option<BindingId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueSymbol {
    pub name: String,
    pub binding: BindingId,
    pub entity: EntityId,
    pub scheme: Scheme,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnvError {
    DuplicateValue { name: String },
    CannotPopRootScope,
}

#[derive(Debug, Clone)]
struct ValueBinding {
    symbol: ValueSymbol,
    ordinal: usize,
}

#[derive(Debug, Clone, Default)]
struct ValueScope {
    values: HashMap<String, ValueBinding>,
}

#[derive(Debug, Clone)]
struct ValueScopes {
    scopes: Vec<ValueScope>,
}

impl ValueScopes {
    fn new() -> Self {
        Self {
            scopes: vec![ValueScope::default()],
        }
    }

    fn push(&mut self) -> LevelId {
        let next = self.current_level().inc();
        self.scopes.push(ValueScope::default());
        next
    }

    fn pop(&mut self) -> Result<(), EnvError> {
        if self.scopes.len() == 1 {
            return Err(EnvError::CannotPopRootScope);
        }
        self.scopes.pop();
        Ok(())
    }

    fn current_level(&self) -> LevelId {
        self.scopes
            .iter()
            .skip(1)
            .fold(LevelId::root(), |level, _| level.inc())
    }

    fn add(&mut self, symbol: ValueSymbol, ordinal: usize) -> Result<(), EnvError> {
        let current = self.scopes.last_mut().expect("value scope");
        if current.values.contains_key(&symbol.name) {
            return Err(EnvError::DuplicateValue { name: symbol.name });
        }
        current
            .values
            .insert(symbol.name.clone(), ValueBinding { symbol, ordinal });
        Ok(())
    }

    fn get(&self, name: &str) -> Option<&ValueSymbol> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.values.get(name).map(|binding| &binding.symbol))
    }

    fn get_mut_by_entity(&mut self, entity: EntityId) -> Option<&mut ValueSymbol> {
        self.scopes
            .iter_mut()
            .flat_map(|scope| scope.values.values_mut())
            .find(|binding| binding.symbol.entity == entity)
            .map(|binding| &mut binding.symbol)
    }

    fn get_by_entity(&self, entity: EntityId) -> Option<&ValueSymbol> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.values.values())
            .find(|binding| binding.symbol.entity == entity)
            .map(|binding| &binding.symbol)
    }

    fn root_bindings(&self) -> Vec<&ValueSymbol> {
        let mut bindings = self.scopes[0].values.values().collect::<Vec<_>>();
        bindings.sort_by_key(|binding| binding.ordinal);
        bindings
            .into_iter()
            .map(|binding| &binding.symbol)
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorBinding;

#[derive(Debug, Clone, PartialEq)]
pub struct RecordFieldBinding;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeBinding;

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleBinding;

#[derive(Debug, Clone, PartialEq)]
pub struct EnvModuleSummary {
    pub values: Vec<ValueSymbol>,
    pub constructors: Vec<ConstructorBinding>,
    pub record_fields: Vec<RecordFieldBinding>,
    pub types: Vec<TypeBinding>,
    pub modules: Vec<ModuleBinding>,
}

#[derive(Debug, Clone, Default)]
struct ModuleFrame {
    constructors: HashMap<String, ConstructorBinding>,
    record_fields: HashMap<String, RecordFieldBinding>,
    types: HashMap<String, TypeBinding>,
    modules: HashMap<String, ModuleBinding>,
}

#[derive(Debug, Clone)]
pub struct Env {
    values: ValueScopes,
    module: ModuleFrame,
    entities: Vec<Entity>,
    next_binding: usize,
    next_entity: usize,
    next_ordinal: usize,
}

impl Env {
    pub fn new() -> Self {
        Self {
            values: ValueScopes::new(),
            module: ModuleFrame::default(),
            entities: Vec::new(),
            next_binding: 0,
            next_entity: 0,
            next_ordinal: 0,
        }
    }

    pub fn current_level(&self) -> LevelId {
        self.values.current_level()
    }

    pub fn push_scope(&mut self) -> LevelId {
        self.values.push()
    }

    pub fn pop_scope(&mut self) -> Result<(), EnvError> {
        self.values.pop()
    }

    pub fn add_value(
        &mut self,
        name: impl Into<String>,
        scheme: Scheme,
    ) -> Result<ValueSymbol, EnvError> {
        let name = name.into();
        let binding = BindingId::new(self.next_binding);
        self.next_binding += 1;
        let entity = EntityId::new(self.next_entity);
        self.next_entity += 1;
        let ordinal = self.next_ordinal;
        self.next_ordinal += 1;

        let symbol = ValueSymbol {
            name,
            binding,
            entity,
            scheme,
        };
        self.values.add(symbol.clone(), ordinal)?;
        self.entities.push(Entity {
            id: entity,
            kind: EntityKind::Value,
            binding: Some(binding),
        });
        Ok(symbol)
    }

    pub fn has_value(&self, name: &str) -> bool {
        self.get_value(name).is_some()
    }

    pub fn get_value(&self, name: &str) -> Option<&ValueSymbol> {
        self.values.get(name)
    }

    pub fn update_value_scheme(&mut self, entity: EntityId, scheme: Scheme) {
        if let Some(symbol) = self.values.get_mut_by_entity(entity) {
            symbol.scheme = scheme;
        }
    }

    pub fn value_by_entity(&self, entity: EntityId) -> Option<&ValueSymbol> {
        self.values.get_by_entity(entity)
    }

    pub fn entity(&self, entity: EntityId) -> Option<&Entity> {
        self.entities.iter().find(|item| item.id == entity)
    }

    pub fn root_values(&self) -> Vec<&ValueSymbol> {
        self.values.root_bindings()
    }

    pub fn module_summary(&self) -> EnvModuleSummary {
        EnvModuleSummary {
            values: self.root_values().into_iter().cloned().collect(),
            constructors: self.module.constructors.values().cloned().collect(),
            record_fields: self.module.record_fields.values().cloned().collect(),
            types: self.module.types.values().cloned().collect(),
            modules: self.module.modules.values().cloned().collect(),
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::{builtin, scheme::Scheme};

    fn scheme() -> Scheme {
        Scheme::monomorphic(builtin::i32())
    }

    #[test]
    fn creates_root_env() {
        let env = Env::new();
        assert_eq!(env.current_level(), LevelId::root());
        let summary = env.module_summary();
        assert!(summary.values.is_empty());
        assert!(summary.constructors.is_empty());
        assert!(summary.record_fields.is_empty());
        assert!(summary.types.is_empty());
        assert!(summary.modules.is_empty());
    }

    #[test]
    fn push_and_pop_value_scope() {
        let mut env = Env::new();
        let root = env.current_level();
        let next = root.inc();
        assert_eq!(next.decr(), Some(root));
        assert_eq!(env.push_scope(), next);
        assert_eq!(env.current_level(), next);
        env.pop_scope().expect("pop");
        assert_eq!(env.current_level(), root);
        assert_eq!(env.pop_scope(), Err(EnvError::CannotPopRootScope));
    }

    #[test]
    fn add_and_lookup_value() {
        let mut env = Env::new();
        let symbol = env.add_value("answer", scheme()).expect("add");
        assert_eq!(env.get_value("answer"), Some(&symbol));
        assert!(env.has_value("answer"));
    }

    #[test]
    fn duplicate_values_in_same_scope_are_rejected() {
        let mut env = Env::new();
        env.add_value("answer", scheme()).expect("add");
        assert_eq!(
            env.add_value("answer", scheme()).unwrap_err(),
            EnvError::DuplicateValue {
                name: "answer".into()
            }
        );
    }

    #[test]
    fn nested_scope_shadowing_is_allowed() {
        let mut env = Env::new();
        let outer = env.add_value("answer", scheme()).expect("outer");
        env.push_scope();
        let inner = env.add_value("answer", scheme()).expect("inner");
        assert_eq!(
            env.get_value("answer").expect("lookup").entity,
            inner.entity
        );
        env.pop_scope().expect("pop");
        assert_eq!(
            env.get_value("answer").expect("lookup").entity,
            outer.entity
        );
    }

    #[test]
    fn root_bindings_expose_only_root_values() {
        let mut env = Env::new();
        let root = env.add_value("root", scheme()).expect("root");
        env.push_scope();
        env.add_value("local", scheme()).expect("local");
        let roots = env.root_values();
        assert_eq!(roots.len(), 1);
        assert_eq!(roots[0].entity, root.entity);
    }

    #[test]
    fn symbol_scheme_and_entity_can_be_looked_up() {
        let mut env = Env::new();
        let symbol = env.add_value("answer", scheme()).expect("add");
        assert_eq!(env.value_by_entity(symbol.entity), Some(&symbol));
        assert_eq!(
            env.entity(symbol.entity).expect("entity").binding,
            Some(symbol.binding)
        );
    }
}
