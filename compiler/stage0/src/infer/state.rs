use std::collections::BTreeMap;

use super::env::Env;
use super::scheme::TypeScheme;
use super::types::{Type, TypeVar};
use super::unifier::{Substitution, UnifyError};

#[derive(Debug, Default)]
pub(crate) struct State {
    next_var: u32,
    env: Env,
    substitution: Substitution,
}

impl State {
    pub(crate) fn into_env(self) -> Env {
        self.env
    }

    pub(crate) fn fresh_var(&mut self) -> Type {
        let var = TypeVar::new(self.next_var);
        self.next_var += 1;
        Type::Var(var)
    }

    pub(crate) fn push_scope(&mut self) {
        self.env.push_scope();
    }

    pub(crate) fn pop_scope(&mut self) {
        self.env.pop_scope();
    }

    pub(crate) fn add_value(&mut self, name: impl Into<String>, scheme: TypeScheme) {
        self.env.add_value(name, scheme);
    }

    pub(crate) fn add_prelude_value(&mut self, name: impl Into<String>, scheme: TypeScheme) {
        self.env.add_prelude_value(name, scheme);
    }

    pub(crate) fn get_value(&self, name: &str) -> Option<&TypeScheme> {
        self.env.get_value(name)
    }

    pub(crate) fn resolve(&self, ty: &Type) -> Type {
        self.substitution.resolve(ty)
    }

    pub(crate) fn unify(&mut self, lhs: &Type, rhs: &Type) -> Result<(), UnifyError> {
        self.substitution.unify(lhs, rhs)
    }

    pub(crate) fn monomorphic(&self, ty: Type) -> TypeScheme {
        TypeScheme::monomorphic(self.resolve(&ty))
    }

    pub(crate) fn generalize(&self, ty: Type) -> TypeScheme {
        let resolved = self.resolve(&ty);
        let env_vars = self.env.free_vars();
        let quantifiers = resolved
            .free_vars()
            .into_iter()
            .filter(|var| !env_vars.contains(var))
            .collect();
        TypeScheme {
            quantifiers,
            body: resolved,
        }
    }

    pub(crate) fn instantiate(&mut self, scheme: &TypeScheme) -> Type {
        let replacements = scheme
            .quantifiers
            .iter()
            .map(|var| (*var, self.fresh_var()))
            .collect::<BTreeMap<_, _>>();
        instantiate_type(&scheme.body, &replacements)
    }
}

fn instantiate_type(ty: &Type, replacements: &BTreeMap<TypeVar, Type>) -> Type {
    match ty {
        Type::Var(var) => replacements.get(var).cloned().unwrap_or(Type::Var(*var)),
        Type::ActorId(message) => Type::ActorId(Box::new(instantiate_type(message, replacements))),
        Type::Arrow { parameter, result } => Type::Arrow {
            parameter: Box::new(instantiate_type(parameter, replacements)),
            result: Box::new(instantiate_type(result, replacements)),
        },
        Type::List(element) => Type::List(Box::new(instantiate_type(element, replacements))),
        Type::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| instantiate_type(item, replacements))
                .collect(),
        ),
        Type::RecordApp { name, args } => Type::RecordApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| instantiate_type(arg, replacements))
                .collect(),
        },
        Type::VariantApp { name, args } => Type::VariantApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| instantiate_type(arg, replacements))
                .collect(),
        },
        Type::Bool
        | Type::Char
        | Type::F64
        | Type::I32
        | Type::I64
        | Type::String
        | Type::Unit
        | Type::Record(_)
        | Type::Variant(_) => ty.clone(),
    }
}
