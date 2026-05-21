pub(crate) mod closure;
pub(crate) mod ir;
pub(crate) mod lower;
mod operators;

pub(crate) use lower::LambdaSimplifier;
