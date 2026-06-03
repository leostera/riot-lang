//! Mutable checker state.
//!
//! `State` is the shared, mutable context threaded through TinyML type checking.
//! It deliberately owns the pieces that must agree globally while a module is
//! checked:
//!
//! - the value/type environment (`Env`), including lexical scopes;
//! - inference variable allocation and union-style links installed by unification;
//! - level information used by Hindley-Milner generalization;
//! - source diagnostics accumulated during checking; and
//! - the temporary type-parameter scope used while lowering type annotations and
//!   type declarations.
//!
//! The checker architecture mirrors the shape used by ML implementations such as
//! OCaml: fresh type variables are allocated at the current environment level,
//! unification links those variables to concrete types, and let-generalization
//! quantifies only variables created below the binding's base level.  Keeping all
//! of that in one state object makes it explicit that type inference is a checker
//! concern, not a backend concern.

use std::collections::HashMap;

use crate::parser::ast::TypeVar;

use super::{
    diagnostic::CheckDiagnostic,
    env::{Env, EnvError, LevelId, ValueSymbol},
    scheme::Scheme,
    tir::{Type, TypeVarId},
};

/// Storage for one inference type variable.
///
/// A slot has two jobs:
///
/// - `level` records the lexical checker level at which the variable was
///   created.  Let-generalization compares this level with the level of the
///   binding being generalized.
/// - `link` is the unifier's mutable solution for the variable.  `None` means
///   the variable is still unsolved; `Some(ty)` means future reads should treat
///   this variable as `ty`.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeVarSlot {
    /// Lexical level at which this inference variable was allocated.
    pub level: LevelId,
    /// Unifier solution for this variable, if it has been solved.
    pub link: Option<Type>,
}

/// Mutable module-checking context.
///
/// `State` is intentionally small and operational: it provides allocation,
/// lookup, scope management, and scheme operations.  Higher-level typing rules
/// live in `typer.rs`; low-level equality/occurs-check logic lives in
/// `unifier.rs`.
#[derive(Debug, Clone, Default)]
pub struct State {
    next_var: usize,
    env: Env,
    type_params: Option<HashMap<String, Type>>,
    diagnostics: Vec<CheckDiagnostic>,
    vars: Vec<TypeVarSlot>,
}

impl State {
    /// Create an empty checker state with a root value scope and no diagnostics.
    pub fn new() -> Self {
        Self::default()
    }

    /// Borrow the current environment for read-only inspection.
    pub fn env(&self) -> &Env {
        &self.env
    }

    /// Borrow the current environment for symbol registration.
    pub fn env_mut(&mut self) -> &mut Env {
        &mut self.env
    }

    /// Return the lexical level at which newly-created inference variables live.
    pub fn current_level(&self) -> LevelId {
        self.env.current_level()
    }

    /// Allocate a fresh unsolved inference variable at the current environment level.
    ///
    /// This is the single entry point for creating checker inference variables.
    /// Its level is what later lets `generalize` distinguish variables local to a
    /// binding from variables captured from an outer scope.
    pub fn fresh_var(&mut self) -> Type {
        let id = TypeVarId::new(self.next_var);
        self.next_var += 1;
        self.vars.push(TypeVarSlot {
            level: self.env.current_level(),
            link: None,
        });
        Type::Var(id)
    }

    /// Read the slot for an inference variable.
    pub fn type_var(&self, id: TypeVarId) -> Option<&TypeVarSlot> {
        self.vars.get(id.index())
    }

    /// Lower an inference variable's level.
    ///
    /// Unification uses this when an older variable is linked to a type that
    /// contains younger variables.  Lowering those variables prevents later
    /// let-generalization from treating them as local to the younger scope.
    pub fn set_var_level(&mut self, id: TypeVarId, level: LevelId) -> Option<()> {
        let slot = self.vars.get_mut(id.index())?;
        if level.index() < slot.level.index() {
            slot.level = level;
        }
        Some(())
    }

    /// Record the unifier solution for an inference variable.
    ///
    /// The unifier performs occurs checks and level adjustment before calling
    /// this method; `State` only stores the link.
    pub fn link_var(&mut self, id: TypeVarId, ty: Type) -> Option<()> {
        let slot = self.vars.get_mut(id.index())?;
        slot.link = Some(ty);
        Some(())
    }

    /// Append a checker diagnostic while continuing to build best-effort TIR.
    pub fn add_diagnostic(&mut self, diagnostic: CheckDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Return diagnostics accumulated so far, in source/checking order.
    pub fn diagnostics(&self) -> &[CheckDiagnostic] {
        &self.diagnostics
    }

    /// Run an operation with a temporary source type-parameter scope.
    ///
    /// Type declarations and type annotations introduce source names such as
    /// `'a`.  While lowering those annotations, every occurrence of the same name
    /// must resolve to the same checker `Type`.  This helper installs that map for
    /// the duration of `f` and restores the previous map afterwards.
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

    /// Look up a source type parameter currently in scope, by name without the quote.
    pub fn get_type_param(&self, name: &str) -> Option<&Type> {
        self.type_params.as_ref()?.get(name)
    }

    pub fn get_type_param_var(&self, var: &TypeVar) -> Option<&Type> {
        self.get_type_param(&var.name)
    }

    /// Add a value binding to the current lexical scope.
    ///
    /// The stored scheme may be a placeholder during top-level pre-registration,
    /// then updated after the value body is inferred and generalized.
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

    /// Replace the scheme for an already-registered value entity.
    pub fn update_value_scheme(&mut self, entity: super::tir::EntityId, scheme: Scheme) {
        self.env.update_value_scheme(entity, scheme);
    }

    /// Enter a nested value scope and inference level.
    pub fn push_scope(&mut self) -> LevelId {
        self.env.push_scope()
    }

    /// Leave the current value scope.
    pub fn pop_scope(&mut self) -> Result<(), EnvError> {
        self.env.pop_scope()
    }

    /// Instantiate a polymorphic scheme for one use site.
    ///
    /// Each quantified `Type::Generic` variable becomes a fresh inference
    /// variable at the current level.  Repeated occurrences of the same generic
    /// in the scheme body share the same fresh variable for this instantiation,
    /// preserving relationships such as `'a -> 'a`.
    pub fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut substitutions = HashMap::new();
        self.instantiate_type(&scheme.body, &scheme.quantifier, &mut substitutions)
    }

    /// Recursive implementation of scheme instantiation.
    ///
    /// `substitutions` is per-instantiation state: it maps each quantified
    /// generic to exactly one fresh inference variable.  Non-quantified generics
    /// and ordinary inference variables are left alone because they belong to a
    /// surrounding context rather than this scheme.
    fn instantiate_type(
        &mut self,
        ty: &Type,
        quantifier: &[TypeVarId],
        substitutions: &mut HashMap<TypeVarId, Type>,
    ) -> Type {
        match ty {
            Type::Generic(id) if quantifier.contains(id) => substitutions
                .entry(*id)
                .or_insert_with(|| {
                    let id = TypeVarId::new(self.next_var);
                    self.next_var += 1;
                    self.vars.push(TypeVarSlot {
                        level: self.env.current_level(),
                        link: None,
                    });
                    Type::Var(id)
                })
                .clone(),
            Type::Var(_) | Type::Generic(_) => ty.clone(),
            Type::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .map(|item| self.instantiate_type(item, quantifier, substitutions))
                    .collect(),
            ),
            Type::Arrow { parameter, result } => Type::Arrow {
                parameter: Box::new(self.instantiate_type(parameter, quantifier, substitutions)),
                result: Box::new(self.instantiate_type(result, quantifier, substitutions)),
            },
            Type::Apply { ident, arguments } => Type::Apply {
                ident: ident.clone(),
                arguments: arguments
                    .iter()
                    .map(|arg| self.instantiate_type(arg, quantifier, substitutions))
                    .collect(),
            },
        }
    }

    /// Generalize an inferred type into a reusable scheme.
    ///
    /// Variables whose allocation level is newer than `base_level` are local to
    /// the binding and can be quantified.  Variables at `base_level` or above are
    /// captured from the surrounding environment and must remain monomorphic.
    pub fn generalize(&self, ty: Type, base_level: LevelId) -> Scheme {
        let mut quantifier = Vec::new();
        let body = self.generalize_type(ty, base_level, &mut quantifier);
        Scheme { quantifier, body }
    }

    /// Recursive implementation of let-generalization.
    ///
    /// Linked variables are followed before deciding whether to quantify.  This
    /// means we generalize the solved shape of a type rather than the stale
    /// pre-unification variable graph.
    fn generalize_type(
        &self,
        ty: Type,
        base_level: LevelId,
        quantifier: &mut Vec<TypeVarId>,
    ) -> Type {
        match ty {
            Type::Var(id) => {
                let Some(slot) = self.type_var(id) else {
                    return Type::Var(id);
                };
                if let Some(link) = &slot.link {
                    return self.generalize_type(link.clone(), base_level, quantifier);
                }
                if slot.level.index() > base_level.index() {
                    if !quantifier.contains(&id) {
                        quantifier.push(id);
                    }
                    Type::Generic(id)
                } else {
                    Type::Var(id)
                }
            }
            Type::Generic(_) => ty,
            Type::Tuple(items) => Type::Tuple(
                items
                    .into_iter()
                    .map(|item| self.generalize_type(item, base_level, quantifier))
                    .collect(),
            ),
            Type::Arrow { parameter, result } => Type::Arrow {
                parameter: Box::new(self.generalize_type(*parameter, base_level, quantifier)),
                result: Box::new(self.generalize_type(*result, base_level, quantifier)),
            },
            Type::Apply { ident, arguments } => Type::Apply {
                ident,
                arguments: arguments
                    .into_iter()
                    .map(|arg| self.generalize_type(arg, base_level, quantifier))
                    .collect(),
            },
        }
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
        state.add_diagnostic(CheckDiagnostic::UnsupportedExpression {
            span: crate::parser::lexer::Span::new(0, 1),
        });
        state.add_diagnostic(CheckDiagnostic::UnsupportedUseDeclaration {
            span: crate::parser::lexer::Span::new(2, 3),
        });
        assert_eq!(
            state.diagnostics(),
            [
                CheckDiagnostic::UnsupportedExpression {
                    span: crate::parser::lexer::Span::new(0, 1)
                },
                CheckDiagnostic::UnsupportedUseDeclaration {
                    span: crate::parser::lexer::Span::new(2, 3)
                },
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
