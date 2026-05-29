use super::tst::{Type, TypeVarId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchemeMode {
    Local,
    Generalized,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub quantifier: Vec<TypeVarId>,
    pub body: Type,
}

impl Scheme {
    pub fn monomorphic(body: Type) -> Self {
        Self {
            quantifier: Vec::new(),
            body,
        }
    }
}
