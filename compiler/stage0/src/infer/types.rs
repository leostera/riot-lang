use std::collections::BTreeSet;

use crate::signature::TypeName;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TypeVar(u32);

impl TypeVar {
    pub(crate) fn new(index: u32) -> Self {
        Self(index)
    }

    pub(crate) fn index(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Type {
    Bool,
    Char,
    F64,
    I32,
    I64,
    String,
    Unit,
    Var(TypeVar),
    ActorId(Box<Type>),
    Arrow {
        parameter: Box<Type>,
        result: Box<Type>,
    },
    List(Box<Type>),
    Record(TypeName),
    RecordApp {
        name: TypeName,
        args: Vec<Type>,
    },
    Variant(TypeName),
    VariantApp {
        name: TypeName,
        args: Vec<Type>,
    },
    Tuple(Vec<Type>),
}

impl Type {
    pub(crate) fn arrow(parameter: Type, result: Type) -> Self {
        Type::Arrow {
            parameter: Box::new(parameter),
            result: Box::new(result),
        }
    }

    pub(crate) fn arrows(params: Vec<Type>, result: Type) -> Self {
        params
            .into_iter()
            .rev()
            .fold(result, |result, parameter| Type::arrow(parameter, result))
    }

    pub(crate) fn contains_var(&self, target: TypeVar) -> bool {
        match self {
            Type::Var(var) => *var == target,
            Type::ActorId(message) | Type::List(message) => message.contains_var(target),
            Type::Arrow { parameter, result } => {
                parameter.contains_var(target) || result.contains_var(target)
            }
            Type::Tuple(items) => items.iter().any(|item| item.contains_var(target)),
            Type::RecordApp { args, .. } | Type::VariantApp { args, .. } => {
                args.iter().any(|arg| arg.contains_var(target))
            }
            Type::Bool
            | Type::Char
            | Type::F64
            | Type::I32
            | Type::I64
            | Type::String
            | Type::Unit
            | Type::Record(_)
            | Type::Variant(_) => false,
        }
    }

    pub(crate) fn free_vars(&self) -> BTreeSet<TypeVar> {
        let mut vars = BTreeSet::new();
        self.collect_free_vars(&mut vars);
        vars
    }

    fn collect_free_vars(&self, vars: &mut BTreeSet<TypeVar>) {
        match self {
            Type::Var(var) => {
                vars.insert(*var);
            }
            Type::ActorId(message) | Type::List(message) => message.collect_free_vars(vars),
            Type::Arrow { parameter, result } => {
                parameter.collect_free_vars(vars);
                result.collect_free_vars(vars);
            }
            Type::Tuple(items) => {
                for item in items {
                    item.collect_free_vars(vars);
                }
            }
            Type::RecordApp { args, .. } | Type::VariantApp { args, .. } => {
                for arg in args {
                    arg.collect_free_vars(vars);
                }
            }
            Type::Bool
            | Type::Char
            | Type::F64
            | Type::I32
            | Type::I64
            | Type::String
            | Type::Unit
            | Type::Record(_)
            | Type::Variant(_) => {}
        }
    }
}
