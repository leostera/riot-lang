#![allow(dead_code)]

use std::collections::BTreeMap;

use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TypeVar(u32);

impl TypeVar {
    pub(crate) fn index(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum InferType {
    Bool,
    Char,
    F64,
    I64,
    String,
    Unit,
    Var(TypeVar),
    ActorId(Box<InferType>),
    Function(Vec<InferType>, Box<InferType>),
    List(Box<InferType>),
    Record(String),
    Tuple(Vec<InferType>),
}

impl InferType {
    fn contains_var(&self, target: TypeVar) -> bool {
        match self {
            InferType::Var(var) => *var == target,
            InferType::ActorId(message) | InferType::List(message) => message.contains_var(target),
            InferType::Function(params, result) => {
                params.iter().any(|param| param.contains_var(target)) || result.contains_var(target)
            }
            InferType::Tuple(items) => items.iter().any(|item| item.contains_var(target)),
            InferType::Bool
            | InferType::Char
            | InferType::F64
            | InferType::I64
            | InferType::String
            | InferType::Unit
            | InferType::Record(_) => false,
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct TypeVarSupply {
    next: u32,
}

impl TypeVarSupply {
    pub(crate) fn fresh(&mut self) -> InferType {
        let var = TypeVar(self.next);
        self.next += 1;
        InferType::Var(var)
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Substitution {
    bindings: BTreeMap<TypeVar, InferType>,
}

impl Substitution {
    pub(crate) fn resolve(&self, ty: &InferType) -> InferType {
        match ty {
            InferType::Var(var) => match self.bindings.get(var) {
                Some(bound) => self.resolve(bound),
                None => ty.clone(),
            },
            InferType::ActorId(message) => InferType::ActorId(Box::new(self.resolve(message))),
            InferType::Function(params, result) => InferType::Function(
                params.iter().map(|param| self.resolve(param)).collect(),
                Box::new(self.resolve(result)),
            ),
            InferType::List(element) => InferType::List(Box::new(self.resolve(element))),
            InferType::Tuple(items) => {
                InferType::Tuple(items.iter().map(|item| self.resolve(item)).collect())
            }
            InferType::Bool
            | InferType::Char
            | InferType::F64
            | InferType::I64
            | InferType::String
            | InferType::Unit
            | InferType::Record(_) => ty.clone(),
        }
    }

    pub(crate) fn unify(&mut self, lhs: &InferType, rhs: &InferType) -> Result<(), UnifyError> {
        let lhs = self.resolve(lhs);
        let rhs = self.resolve(rhs);

        match (lhs, rhs) {
            (InferType::Var(var), ty) | (ty, InferType::Var(var)) => self.bind(var, ty),
            (InferType::Bool, InferType::Bool)
            | (InferType::Char, InferType::Char)
            | (InferType::F64, InferType::F64)
            | (InferType::I64, InferType::I64)
            | (InferType::String, InferType::String)
            | (InferType::Unit, InferType::Unit) => Ok(()),
            (InferType::Record(lhs), InferType::Record(rhs)) if lhs == rhs => Ok(()),
            (InferType::ActorId(lhs), InferType::ActorId(rhs))
            | (InferType::List(lhs), InferType::List(rhs)) => self.unify(&lhs, &rhs),
            (InferType::Tuple(lhs), InferType::Tuple(rhs)) => {
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
            (
                InferType::Function(lhs_params, lhs_result),
                InferType::Function(rhs_params, rhs_result),
            ) => {
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

    fn bind(&mut self, var: TypeVar, ty: InferType) -> Result<(), UnifyError> {
        if ty == InferType::Var(var) {
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
    OccursCheck { var: TypeVar, ty: InferType },
    #[error("cannot unify tuple types with {lhs} and {rhs} fields")]
    TupleArityMismatch { lhs: usize, rhs: usize },
    #[error("cannot unify {lhs:?} with {rhs:?}")]
    TypeMismatch { lhs: InferType, rhs: InferType },
}

#[cfg(test)]
mod tests {
    use super::{InferType, Substitution, TypeVarSupply, UnifyError};

    #[test]
    fn fresh_type_variables_are_unique() {
        let mut supply = TypeVarSupply::default();

        let first = supply.fresh();
        let second = supply.fresh();

        assert_ne!(first, second);
        assert_eq!(InferType::Var(super::TypeVar(0)), first);
        assert_eq!(InferType::Var(super::TypeVar(1)), second);
    }

    #[test]
    fn unifies_scalar_with_same_scalar() {
        let mut subst = Substitution::default();

        subst.unify(&InferType::I64, &InferType::I64).unwrap();

        assert_eq!(InferType::I64, subst.resolve(&InferType::I64));
    }

    #[test]
    fn binds_type_variable_to_concrete_type() {
        let mut supply = TypeVarSupply::default();
        let var = supply.fresh();
        let mut subst = Substitution::default();

        subst.unify(&var, &InferType::String).unwrap();

        assert_eq!(InferType::String, subst.resolve(&var));
    }

    #[test]
    fn applies_transitive_substitution() {
        let mut supply = TypeVarSupply::default();
        let first = supply.fresh();
        let second = supply.fresh();
        let mut subst = Substitution::default();

        subst.unify(&first, &second).unwrap();
        subst.unify(&second, &InferType::Bool).unwrap();

        assert_eq!(InferType::Bool, subst.resolve(&first));
        assert_eq!(InferType::Bool, subst.resolve(&second));
    }

    #[test]
    fn rejects_occurs_check_cycle() {
        let mut supply = TypeVarSupply::default();
        let var = supply.fresh();
        let mut subst = Substitution::default();

        let err = subst
            .unify(&var, &InferType::List(Box::new(var.clone())))
            .unwrap_err();

        assert!(matches!(err, UnifyError::OccursCheck { .. }));
    }

    #[test]
    fn unifies_function_argument_and_result_types() {
        let mut supply = TypeVarSupply::default();
        let param = supply.fresh();
        let result = supply.fresh();
        let mut subst = Substitution::default();

        let inferred = InferType::Function(vec![param.clone()], Box::new(result.clone()));
        let concrete = InferType::Function(vec![InferType::I64], Box::new(InferType::Bool));
        subst.unify(&inferred, &concrete).unwrap();

        assert_eq!(InferType::I64, subst.resolve(&param));
        assert_eq!(InferType::Bool, subst.resolve(&result));
    }

    #[test]
    fn rejects_function_arity_mismatch() {
        let mut subst = Substitution::default();
        let lhs = InferType::Function(vec![InferType::I64], Box::new(InferType::I64));
        let rhs = InferType::Function(
            vec![InferType::I64, InferType::I64],
            Box::new(InferType::I64),
        );

        let err = subst.unify(&lhs, &rhs).unwrap_err();

        assert_eq!(UnifyError::FunctionArityMismatch { lhs: 1, rhs: 2 }, err);
    }

    #[test]
    fn unifies_tuple_and_list_members() {
        let mut supply = TypeVarSupply::default();
        let tuple_item = supply.fresh();
        let list_item = supply.fresh();
        let mut subst = Substitution::default();

        let inferred = InferType::Tuple(vec![
            tuple_item.clone(),
            InferType::List(Box::new(list_item.clone())),
        ]);
        let concrete = InferType::Tuple(vec![
            InferType::Char,
            InferType::List(Box::new(InferType::String)),
        ]);
        subst.unify(&inferred, &concrete).unwrap();

        assert_eq!(InferType::Char, subst.resolve(&tuple_item));
        assert_eq!(InferType::String, subst.resolve(&list_item));
    }
}
