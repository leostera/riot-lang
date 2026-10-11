//! Type unification.
//!
//! The unifier is the low-level equality engine for checker types.  It consumes
//! two `Type` values that should describe the same thing, resolves any existing
//! inference-variable links, and either records new links or reports a
//! structured `UnifyError`.
//!
//! This module intentionally knows nothing about source syntax.  It does not
//! decide *why* two types must be equal; `typer.rs` does that.  `unifier.rs`
//! only implements the mechanical Hindley-Milner operations:
//!
//! - resolving linked inference variables;
//! - checking whether a variable occurs inside a candidate solution;
//! - solving unbound variables by linking them to types;
//! - recursively unifying tuples, arrows, and nominal applications; and
//! - preserving generic variables as rigid variables.
//!
//! The occurs check is what prevents constructing infinite types such as
//! `'a = 'a -> 'b`.  Generic variables are intentionally rigid: they only unify
//! with the same generic variable, while schemes are instantiated into fresh
//! inference variables before normal use-site unification.

use thiserror::Error;

use super::{
    state::State,
    tir::{Type, TypeVarId},
};

/// Errors produced by type equality/unification.
#[derive(Debug, Clone, PartialEq, Error)]
pub enum UnifyError {
    #[error("type mismatch: expected {expected:?}, got {actual:?}")]
    TypeMismatch { expected: Type, actual: Type },
    #[error("recursive type: type variable {var:?} occurs in {ty:?}")]
    InfiniteSubstitution { var: TypeVarId, ty: Type },
    #[error("unknown type variable {0:?}")]
    UnknownVar(TypeVarId),
}

/// Mutable unifier view over checker state.
///
/// The unifier borrows `State` mutably because solving a variable records a link
/// in that state's `TypeVarSlot` table.  A fresh `Unifier` is typically created
/// for each typing constraint; all of them share the same underlying state.
pub struct Unifier<'state> {
    state: &'state mut State,
}

impl<'state> Unifier<'state> {
    pub fn new(state: &'state mut State) -> Self {
        Self { state }
    }

    /// Resolve type-variable links inside `ty`.
    ///
    /// This performs path compression for linked variables: when a variable
    /// points to another variable that points to a concrete type, the original
    /// variable is updated to point directly at the concrete resolved type.
    pub fn resolve(&mut self, ty: &Type) -> Result<Type, UnifyError> {
        match ty {
            Type::Var(id) => {
                let Some(slot) = self.state.type_var(*id) else {
                    return Err(UnifyError::UnknownVar(*id));
                };
                match slot.link.clone() {
                    Some(linked) => {
                        let resolved = self.resolve(&linked)?;
                        self.state
                            .link_var(*id, resolved.clone())
                            .ok_or(UnifyError::UnknownVar(*id))?;
                        Ok(resolved)
                    }
                    None => Ok(ty.clone()),
                }
            }
            Type::Arrow { parameter, result } => Ok(Type::Arrow {
                parameter: Box::new(self.resolve(parameter)?),
                result: Box::new(self.resolve(result)?),
            }),
            Type::Tuple(items) => Ok(Type::Tuple(
                items
                    .iter()
                    .map(|item| self.resolve(item))
                    .collect::<Result<_, _>>()?,
            )),
            Type::Apply { ident, arguments } => Ok(Type::Apply {
                ident: ident.clone(),
                arguments: arguments
                    .iter()
                    .map(|arg| self.resolve(arg))
                    .collect::<Result<_, _>>()?,
            }),
            Type::Generic(_) => Ok(ty.clone()),
        }
    }

    /// Return whether `var` appears anywhere inside `ty` after resolving links.
    ///
    /// This is the occurs check used before solving a variable.  Without it,
    /// unifying a variable with a type containing that same variable would create
    /// an infinite recursive type.
    pub fn occurs_in(&mut self, var: TypeVarId, ty: &Type) -> Result<bool, UnifyError> {
        match self.resolve(ty)? {
            Type::Var(other) => Ok(var == other),
            Type::Arrow { parameter, result } => {
                Ok(self.occurs_in(var, &parameter)? || self.occurs_in(var, &result)?)
            }
            Type::Tuple(items) => {
                for item in items {
                    if self.occurs_in(var, &item)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Type::Apply { arguments, .. } => {
                for arg in arguments {
                    if self.occurs_in(var, &arg)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Type::Generic(_) => Ok(false),
        }
    }

    /// Lower free inference variables in `ty` so they cannot escape `max_level`.
    ///
    /// This is the core Rémy-style level adjustment.  If an older variable is
    /// solved to a type containing younger variables, those younger variables are
    /// now reachable from the older scope and must be lowered before the link is
    /// installed.  Rigid generics are left untouched.
    pub fn adjust_levels(
        &mut self,
        max_level: super::env::LevelId,
        ty: &Type,
    ) -> Result<(), UnifyError> {
        match ty {
            Type::Var(id) => {
                let Some(slot) = self.state.type_var(*id).cloned() else {
                    return Err(UnifyError::UnknownVar(*id));
                };
                if let Some(link) = slot.link {
                    self.adjust_levels(max_level, &link)
                } else {
                    self.state
                        .set_var_level(*id, max_level)
                        .ok_or(UnifyError::UnknownVar(*id))
                }
            }
            Type::Arrow { parameter, result } => {
                self.adjust_levels(max_level, parameter)?;
                self.adjust_levels(max_level, result)
            }
            Type::Tuple(items) => {
                for item in items {
                    self.adjust_levels(max_level, item)?;
                }
                Ok(())
            }
            Type::Apply { arguments, .. } => {
                for arg in arguments {
                    self.adjust_levels(max_level, arg)?;
                }
                Ok(())
            }
            Type::Generic(_) => Ok(()),
        }
    }

    /// Solve one inference variable against a type.
    ///
    /// If the type is the same variable, the constraint is already satisfied. If
    /// the variable occurs in the type, solving would create an infinite type and
    /// is rejected. Otherwise the variable is linked to the resolved type.
    pub fn solve_var(&mut self, var: TypeVarId, ty: &Type) -> Result<(), UnifyError> {
        match self.resolve(ty)? {
            Type::Var(other) if var == other => Ok(()),
            resolved if self.occurs_in(var, &resolved)? => {
                Err(UnifyError::InfiniteSubstitution { var, ty: resolved })
            }
            resolved => {
                let Some(slot) = self.state.type_var(var).cloned() else {
                    return Err(UnifyError::UnknownVar(var));
                };
                self.adjust_levels(slot.level, &resolved)?;
                self.state
                    .link_var(var, resolved)
                    .ok_or(UnifyError::UnknownVar(var))?;
                Ok(())
            }
        }
    }

    /// Enforce equality between `expected` and `actual`.
    ///
    /// The names are for diagnostics only; unification itself is symmetric for
    /// inference variables.  Nominal applications must have the same name and
    /// arity, arrows unify parameter/result pairwise, and tuples unify element by
    /// element.
    pub fn unify(&mut self, expected: &Type, actual: &Type) -> Result<(), UnifyError> {
        match (self.resolve(expected)?, self.resolve(actual)?) {
            (Type::Generic(a), Type::Generic(b)) if a == b => Ok(()),
            (Type::Var(a), Type::Var(b)) if a == b => Ok(()),
            (Type::Var(var), ty) | (ty, Type::Var(var)) => self.solve_var(var, &ty),
            (Type::Tuple(expected), Type::Tuple(actual)) => self.unify_many(&expected, &actual),
            (
                Type::Arrow {
                    parameter: expected_parameter,
                    result: expected_result,
                },
                Type::Arrow {
                    parameter: actual_parameter,
                    result: actual_result,
                },
            ) => self.unify_arrow(
                &expected_parameter,
                &expected_result,
                &actual_parameter,
                &actual_result,
            ),
            (
                Type::Apply {
                    ident: expected_ident,
                    arguments: expected_arguments,
                },
                Type::Apply {
                    ident: actual_ident,
                    arguments: actual_arguments,
                },
            ) => self.unify_applications(
                &expected_ident,
                &expected_arguments,
                &actual_ident,
                &actual_arguments,
            ),
            (expected, actual) => Err(UnifyError::TypeMismatch { expected, actual }),
        }
    }

    pub fn unify_many(&mut self, expected: &[Type], actual: &[Type]) -> Result<(), UnifyError> {
        if expected.len() != actual.len() {
            return Err(UnifyError::TypeMismatch {
                expected: Type::Tuple(expected.to_vec()),
                actual: Type::Tuple(actual.to_vec()),
            });
        }
        for (expected, actual) in expected.iter().zip(actual) {
            self.unify(expected, actual)?;
        }
        Ok(())
    }

    fn unify_arrow(
        &mut self,
        expected_parameter: &Type,
        expected_result: &Type,
        actual_parameter: &Type,
        actual_result: &Type,
    ) -> Result<(), UnifyError> {
        self.unify(expected_parameter, actual_parameter)?;
        self.unify(expected_result, actual_result)
    }

    fn unify_applications(
        &mut self,
        expected_ident: &crate::parser::ident::Ident,
        expected_arguments: &[Type],
        actual_ident: &crate::parser::ident::Ident,
        actual_arguments: &[Type],
    ) -> Result<(), UnifyError> {
        if expected_ident.same_path(actual_ident)
            && expected_arguments.len() == actual_arguments.len()
        {
            self.unify_many(expected_arguments, actual_arguments)
        } else {
            Err(UnifyError::TypeMismatch {
                expected: Type::Apply {
                    ident: expected_ident.clone(),
                    arguments: expected_arguments.to_vec(),
                },
                actual: Type::Apply {
                    ident: actual_ident.clone(),
                    arguments: actual_arguments.to_vec(),
                },
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        checker::{builtin, state::State},
        parser::ident::Ident,
    };

    fn app(name: &str, arguments: Vec<Type>) -> Type {
        Type::Apply {
            ident: Ident::from_string(name),
            arguments,
        }
    }

    #[test]
    fn unifies_identical_primitive_applications() {
        let mut state = State::new();
        let mut unifier = Unifier::new(&mut state);
        assert!(unifier.unify(&builtin::i32(), &builtin::i32()).is_ok());
    }

    #[test]
    fn rejects_different_primitive_applications() {
        let mut state = State::new();
        let mut unifier = Unifier::new(&mut state);
        assert!(matches!(
            unifier.unify(&builtin::i32(), &builtin::bool()),
            Err(UnifyError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn unifies_nominal_applications_pairwise() {
        let mut state = State::new();
        let mut unifier = Unifier::new(&mut state);
        assert!(
            unifier
                .unify(
                    &app("Option", vec![builtin::i32()]),
                    &app("Option", vec![builtin::i32()]),
                )
                .is_ok()
        );
    }

    #[test]
    fn rejects_nominal_application_arity_mismatch() {
        let mut state = State::new();
        let mut unifier = Unifier::new(&mut state);
        assert!(matches!(
            unifier.unify(&app("Option", vec![builtin::i32()]), &app("Option", vec![]),),
            Err(UnifyError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn unifies_tuples_pairwise() {
        let mut state = State::new();
        let mut unifier = Unifier::new(&mut state);
        assert!(
            unifier
                .unify(
                    &Type::Tuple(vec![builtin::i32(), builtin::bool()]),
                    &Type::Tuple(vec![builtin::i32(), builtin::bool()]),
                )
                .is_ok()
        );
    }

    #[test]
    fn unifies_unary_arrows_pairwise() {
        let mut state = State::new();
        let mut unifier = Unifier::new(&mut state);
        let arrow = Type::Arrow {
            parameter: Box::new(builtin::i32()),
            result: Box::new(Type::Arrow {
                parameter: Box::new(builtin::i32()),
                result: Box::new(builtin::i32()),
            }),
        };
        assert!(unifier.unify(&arrow, &arrow).is_ok());
    }

    #[test]
    fn links_unbound_variable_to_concrete_type() {
        let mut state = State::new();
        let var = state.fresh_var();
        let mut unifier = Unifier::new(&mut state);
        unifier.unify(&var, &builtin::i32()).expect("unify");
        assert_eq!(unifier.resolve(&var).expect("resolve"), builtin::i32());
    }

    #[test]
    fn resolve_path_compresses_link_chains() {
        let mut state = State::new();
        let a = state.fresh_var();
        let b = state.fresh_var();
        {
            let mut unifier = Unifier::new(&mut state);
            unifier.unify(&a, &b).expect("a b");
            unifier.unify(&b, &builtin::bool()).expect("b bool");
            assert_eq!(unifier.resolve(&a).expect("resolve"), builtin::bool());
        }
        let Type::Var(id) = a else {
            panic!("expected var");
        };
        assert_eq!(
            state.type_var(id).expect("slot").link,
            Some(builtin::bool())
        );
    }

    #[test]
    fn occurs_check_rejects_recursive_type() {
        let mut state = State::new();
        let var = state.fresh_var();
        let mut unifier = Unifier::new(&mut state);
        assert!(matches!(
            unifier.unify(&var, &Type::Tuple(vec![var.clone()])),
            Err(UnifyError::InfiniteSubstitution { .. })
        ));
    }

    #[test]
    fn generic_vars_unify_only_with_same_generic() {
        let mut state = State::new();
        let mut unifier = Unifier::new(&mut state);
        assert!(
            unifier
                .unify(
                    &Type::Generic(TypeVarId::new(0)),
                    &Type::Generic(TypeVarId::new(0)),
                )
                .is_ok()
        );
        assert!(matches!(
            unifier.unify(
                &Type::Generic(TypeVarId::new(0)),
                &Type::Generic(TypeVarId::new(1)),
            ),
            Err(UnifyError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn mismatch_errors_are_stable() {
        let mut state = State::new();
        let mut unifier = Unifier::new(&mut state);
        assert_eq!(
            unifier.unify(&builtin::string(), &Type::Tuple(vec![])),
            Err(UnifyError::TypeMismatch {
                expected: builtin::string(),
                actual: Type::Tuple(vec![]),
            })
        );
    }
}
