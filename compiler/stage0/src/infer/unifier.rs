use std::collections::BTreeMap;

use thiserror::Error;

use super::types::{Type, TypeVar};

#[derive(Debug, Default, Clone)]
pub(crate) struct Substitution {
    bindings: BTreeMap<TypeVar, Type>,
}

impl Substitution {
    pub(crate) fn resolve(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(var) => match self.bindings.get(var) {
                Some(bound) => self.resolve(bound),
                None => ty.clone(),
            },
            Type::ActorId(message) => Type::ActorId(Box::new(self.resolve(message))),
            Type::Function(params, result) => Type::Function(
                params.iter().map(|param| self.resolve(param)).collect(),
                Box::new(self.resolve(result)),
            ),
            Type::List(element) => Type::List(Box::new(self.resolve(element))),
            Type::Tuple(items) => {
                Type::Tuple(items.iter().map(|item| self.resolve(item)).collect())
            }
            Type::Bool
            | Type::Char
            | Type::F64
            | Type::I64
            | Type::String
            | Type::Unit
            | Type::Record(_) => ty.clone(),
        }
    }

    pub(crate) fn unify(&mut self, lhs: &Type, rhs: &Type) -> Result<(), UnifyError> {
        let lhs = self.resolve(lhs);
        let rhs = self.resolve(rhs);

        match (lhs, rhs) {
            (Type::Var(var), ty) | (ty, Type::Var(var)) => self.bind(var, ty),
            (Type::Bool, Type::Bool)
            | (Type::Char, Type::Char)
            | (Type::F64, Type::F64)
            | (Type::I64, Type::I64)
            | (Type::String, Type::String)
            | (Type::Unit, Type::Unit) => Ok(()),
            (Type::Record(lhs), Type::Record(rhs)) if lhs == rhs => Ok(()),
            (Type::ActorId(lhs), Type::ActorId(rhs)) | (Type::List(lhs), Type::List(rhs)) => {
                self.unify(&lhs, &rhs)
            }
            (Type::Tuple(lhs), Type::Tuple(rhs)) => {
                if lhs.len() != rhs.len() {
                    return Err(UnifyError::TupleArityMismatch {
                        lhs: lhs.len(),
                        rhs: rhs.len(),
                    });
                }

                for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
                    self.unify(lhs, rhs)?;
                }

                Ok(())
            }
            (Type::Function(lhs_params, lhs_result), Type::Function(rhs_params, rhs_result)) => {
                if lhs_params.len() != rhs_params.len() {
                    return Err(UnifyError::FunctionArityMismatch {
                        lhs: lhs_params.len(),
                        rhs: rhs_params.len(),
                    });
                }

                for (lhs, rhs) in lhs_params.iter().zip(rhs_params.iter()) {
                    self.unify(lhs, rhs)?;
                }
                self.unify(&lhs_result, &rhs_result)
            }
            (lhs, rhs) => Err(UnifyError::TypeMismatch { lhs, rhs }),
        }
    }

    fn bind(&mut self, var: TypeVar, ty: Type) -> Result<(), UnifyError> {
        if ty == Type::Var(var) {
            return Ok(());
        }

        if ty.contains_var(var) {
            return Err(UnifyError::OccursCheck { var, ty });
        }

        self.bindings.insert(var, ty);
        Ok(())
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub(crate) enum UnifyError {
    #[error("cannot unify function types with {lhs} and {rhs} parameters")]
    FunctionArityMismatch { lhs: usize, rhs: usize },
    #[error("cannot bind type variable {var:?} to recursive type {ty:?}")]
    OccursCheck { var: TypeVar, ty: Type },
    #[error("cannot unify tuple types with {lhs} and {rhs} fields")]
    TupleArityMismatch { lhs: usize, rhs: usize },
    #[error("cannot unify {lhs:?} with {rhs:?}")]
    TypeMismatch { lhs: Type, rhs: Type },
}

#[cfg(test)]
mod tests {
    use super::{Substitution, UnifyError};
    use crate::infer::state::State;
    use crate::infer::types::{Type, TypeVar};

    #[test]
    fn fresh_type_variables_are_unique() {
        let mut state = State::default();

        let first = state.fresh_var();
        let second = state.fresh_var();

        assert_ne!(first, second);
        assert_eq!(Type::Var(TypeVar::new(0)), first);
        assert_eq!(Type::Var(TypeVar::new(1)), second);
    }

    #[test]
    fn unifies_scalar_with_same_scalar() {
        let mut subst = Substitution::default();

        subst.unify(&Type::I64, &Type::I64).unwrap();

        assert_eq!(Type::I64, subst.resolve(&Type::I64));
    }

    #[test]
    fn binds_type_variable_to_concrete_type() {
        let mut state = State::default();
        let var = state.fresh_var();
        let mut subst = Substitution::default();

        subst.unify(&var, &Type::String).unwrap();

        assert_eq!(Type::String, subst.resolve(&var));
    }

    #[test]
    fn applies_transitive_substitution() {
        let mut state = State::default();
        let first = state.fresh_var();
        let second = state.fresh_var();
        let mut subst = Substitution::default();

        subst.unify(&first, &second).unwrap();
        subst.unify(&second, &Type::Bool).unwrap();

        assert_eq!(Type::Bool, subst.resolve(&first));
        assert_eq!(Type::Bool, subst.resolve(&second));
    }

    #[test]
    fn rejects_occurs_check_cycle() {
        let mut state = State::default();
        let var = state.fresh_var();
        let mut subst = Substitution::default();

        let err = subst
            .unify(&var, &Type::List(Box::new(var.clone())))
            .unwrap_err();

        assert!(matches!(err, UnifyError::OccursCheck { .. }));
    }

    #[test]
    fn unifies_function_argument_and_result_types() {
        let mut state = State::default();
        let param = state.fresh_var();
        let result = state.fresh_var();
        let mut subst = Substitution::default();

        let inferred = Type::Function(vec![param.clone()], Box::new(result.clone()));
        let concrete = Type::Function(vec![Type::I64], Box::new(Type::Bool));
        subst.unify(&inferred, &concrete).unwrap();

        assert_eq!(Type::I64, subst.resolve(&param));
        assert_eq!(Type::Bool, subst.resolve(&result));
    }

    #[test]
    fn rejects_function_arity_mismatch() {
        let mut subst = Substitution::default();
        let lhs = Type::Function(vec![Type::I64], Box::new(Type::I64));
        let rhs = Type::Function(vec![Type::I64, Type::I64], Box::new(Type::I64));

        let err = subst.unify(&lhs, &rhs).unwrap_err();

        assert_eq!(UnifyError::FunctionArityMismatch { lhs: 1, rhs: 2 }, err);
    }

    #[test]
    fn unifies_tuple_and_list_members() {
        let mut state = State::default();
        let tuple_item = state.fresh_var();
        let list_item = state.fresh_var();
        let mut subst = Substitution::default();

        let inferred = Type::Tuple(vec![
            tuple_item.clone(),
            Type::List(Box::new(list_item.clone())),
        ]);
        let concrete = Type::Tuple(vec![Type::Char, Type::List(Box::new(Type::String))]);
        subst.unify(&inferred, &concrete).unwrap();

        assert_eq!(Type::Char, subst.resolve(&tuple_item));
        assert_eq!(Type::String, subst.resolve(&list_item));
    }
}
