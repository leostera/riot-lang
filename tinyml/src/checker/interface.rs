use crate::parser::{ast, ident::Ident};

use super::{scheme::Scheme, tst::Type};

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleInterface {
    pub values: Vec<ValueDescription>,
    pub constructors: Vec<ConstructorDescription>,
    pub record_fields: Vec<RecordFieldDescription>,
    pub types: Vec<TypeDescription>,
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
    pub result: Type,
    pub arguments: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordFieldDescription {
    pub name: Ident,
    pub owner: Type,
    pub field: Type,
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
