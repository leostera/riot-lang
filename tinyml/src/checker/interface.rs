//! Checked module interface summaries.
//!
//! These in-memory interfaces describe exported values, nominal types,
//! constructors, and record fields. Source modules are resolved and checked in
//! one graph, so no serialized interface file is needed between compiler runs.

use crate::parser::{ast, ident::Ident};

use super::{scheme::Scheme, tir::Type};

/// The declarations exported by one checked source module.
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleInterface {
    pub values: Vec<ValueDescription>,
    pub constructors: Vec<ConstructorDescription>,
    pub record_fields: Vec<RecordFieldDescription>,
    pub types: Vec<TypeDescription>,
    /// Direct dependencies, used only while checking a resolved source graph.
    pub modules: Vec<ModuleDescription>,
}

impl ModuleInterface {
    pub fn empty() -> Self {
        Self {
            values: Vec::new(),
            constructors: Vec::new(),
            record_fields: Vec::new(),
            types: Vec::new(),
            modules: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDescription {
    pub name: Ident,
    pub scheme: Scheme,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDescription {
    pub name: Ident,
    pub scheme: Scheme,
    pub owner: Ident,
    /// Field names for record constructors; empty for positional constructors.
    pub field_names: Vec<Ident>,
    pub tag: u32,
    pub result: Type,
    pub arguments: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordFieldDescription {
    pub name: Ident,
    pub owner: Type,
    pub field: Type,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDescription {
    pub name: Ident,
    pub params: Vec<ast::TypeVar>,
    pub body: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDescription {
    pub name: Ident,
    pub interface: Box<ModuleInterface>,
}
