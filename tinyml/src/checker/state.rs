use std::collections::HashMap;

use crate::parser::ast::TypeVar;

use super::{
    diagnostic::CheckDiagnostic,
    env::{Env, EnvError, LevelId, ValueSymbol},
    scheme::Scheme,
    tst::{Type, TypeVarId},
};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeVarSlot {
    pub level: LevelId,
    pub link: Option<Type>,
}

#[derive(Debug, Clone, Default)]
pub struct State {
    next_var: usize,
    env: Env,
    type_params: Option<HashMap<String, Type>>,
    diagnostics: Vec<CheckDiagnostic>,
    vars: Vec<TypeVarSlot>,
}

impl State {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    pub fn fresh_var(&mut self) -> Type {
        let id = TypeVarId::new(self.next_var);
        self.next_var += 1;
        self.vars.push(TypeVarSlot {
            level: self.env.current_level(),
            link: None,
        });
        Type::Var(id)
    }

    pub fn type_var(&self, id: TypeVarId) -> Option<&TypeVarSlot> {
        self.vars.get(id.index())
    }

    pub fn link_var(&mut self, id: TypeVarId, ty: Type) -> Option<()> {
        let slot = self.vars.get_mut(id.index())?;
        slot.link = Some(ty);
        Some(())
    }

    pub fn add_diagnostic(&mut self, diagnostic: CheckDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn diagnostics(&self) -> &[CheckDiagnostic] {
        &self.diagnostics
    }

    pub fn with_type_params<T>(
        &mut self,
        scope: HashMap<String, Type>,
        f: impl FnOnce(&mut State) -> T,
    ) -> T {
        let previous = self.type_params.replace(scope);
        let result = f(self);
        self.type_params = previous;
        result
    }

    pub fn get_type_param(&self, name: &str) -> Option<&Type> {
        self.type_params.as_ref()?.get(name)
    }

    pub fn get_type_param_var(&self, var: &TypeVar) -> Option<&Type> {
        self.get_type_param(&var.name)
    }

    pub fn add_value(
        &mut self,
        name: impl Into<String>,
        scheme: Scheme,
    ) -> Result<ValueSymbol, EnvError> {
        self.env.add_value(name, scheme)
    }

    pub fn get_value(&self, name: &str) -> Option<&ValueSymbol> {
        self.env.get_value(name)
    }

    pub fn has_value(&self, name: &str) -> bool {
        self.env.has_value(name)
    }

    pub fn update_value_scheme(&mut self, entity: super::tst::EntityId, scheme: Scheme) {
        self.env.update_value_scheme(entity, scheme);
    }

    pub fn push_scope(&mut self) -> LevelId {
        self.env.push_scope()
    }

    pub fn pop_scope(&mut self) -> Result<(), EnvError> {
        self.env.pop_scope()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{checker::builtin, parser::lexer::Span};

    #[test]
    fn fresh_vars_are_level_scoped_from_env() {
        let mut state = State::new();
        let Type::Var(root) = state.fresh_var() else {
            panic!("expected var");
        };
        assert_eq!(state.type_var(root).expect("slot").level, LevelId::new(0));

        state.push_scope();
        let Type::Var(nested) = state.fresh_var() else {
            panic!("expected var");
        };
        assert_eq!(state.type_var(nested).expect("slot").level, LevelId::new(1));
    }

    #[test]
    fn records_diagnostics_in_order() {
        let mut state = State::new();
        state.add_diagnostic(CheckDiagnostic::UnsupportedExpression);
        state.add_diagnostic(CheckDiagnostic::UnsupportedUseDeclaration);
        assert_eq!(
            state.diagnostics(),
            [
                CheckDiagnostic::UnsupportedExpression,
                CheckDiagnostic::UnsupportedUseDeclaration,
            ]
        );
    }

    #[test]
    fn fresh_vars_get_increasing_ids() {
        let mut state = State::new();
        assert_eq!(state.fresh_var(), Type::Var(TypeVarId::new(0)));
        assert_eq!(state.fresh_var(), Type::Var(TypeVarId::new(1)));
    }

    #[test]
    fn link_var_updates_slot() {
        let mut state = State::new();
        let Type::Var(id) = state.fresh_var() else {
            panic!("expected var");
        };
        state.link_var(id, builtin::bool()).expect("link");
        assert_eq!(
            state.type_var(id).expect("slot").link,
            Some(builtin::bool())
        );
    }

    #[test]
    fn value_env_conveniences_work() {
        let mut state = State::new();
        state
            .add_value("answer", Scheme::monomorphic(builtin::i32()))
            .expect("add");
        assert!(state.has_value("answer"));
        assert!(state.get_value("answer").is_some());
        state.push_scope();
        state
            .add_value("answer", Scheme::monomorphic(builtin::bool()))
            .expect("shadow");
        state.pop_scope().expect("pop");
        assert!(state.get_value("answer").is_some());
    }

    #[test]
    fn type_param_scope_is_temporary() {
        let mut state = State::new();
        let ty = state.fresh_var();
        let mut scope = HashMap::new();
        scope.insert("a".into(), ty.clone());
        let result = state.with_type_params(scope, |state| state.get_type_param("a").cloned());
        assert_eq!(result, Some(ty));
        assert_eq!(state.get_type_param("a"), None);
    }

    #[test]
    fn nested_type_param_scopes_restore_previous_scope() {
        let mut state = State::new();
        let outer = state.fresh_var();
        let inner = state.fresh_var();
        let mut outer_scope = HashMap::new();
        outer_scope.insert("a".into(), outer.clone());
        let result = state.with_type_params(outer_scope, |state| {
            let mut inner_scope = HashMap::new();
            inner_scope.insert("a".into(), inner.clone());
            let inner_result =
                state.with_type_params(inner_scope, |state| state.get_type_param("a").cloned());
            let outer_result = state.get_type_param("a").cloned();
            (inner_result, outer_result)
        });
        assert_eq!(result, (Some(inner), Some(outer)));
    }

    #[test]
    fn type_var_lookup_uses_type_var_name() {
        let mut state = State::new();
        let ty = state.fresh_var();
        let mut scope = HashMap::new();
        scope.insert("a".into(), ty.clone());
        let var = TypeVar {
            name: "a".into(),
            span: Span::new(0, 2),
        };
        let result = state.with_type_params(scope, |state| state.get_type_param_var(&var).cloned());
        assert_eq!(result, Some(ty));
    }
}
