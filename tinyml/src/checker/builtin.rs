//! Checker built-in types and constants.
//!
//! Centralizes semantic built-ins such as primitive types and the unit
//! constructor name so later phases do not duplicate magic strings.

use crate::parser::ident::Ident;

use super::tir::Type;

pub const UNIT_CONSTRUCTOR: &str = "()";

pub const TYPE_ARITIES: &[(&str, usize)] = &[
    ("unit", 0),
    ("bool", 0),
    ("i8", 0),
    ("i16", 0),
    ("i32", 0),
    ("i64", 0),
    ("u8", 0),
    ("u16", 0),
    ("u32", 0),
    ("u64", 0),
    ("string", 0),
];

fn primitive(name: &str) -> Type {
    Type::Apply {
        ident: Ident::from_string(name),
        arguments: Vec::new(),
    }
}

macro_rules! builtins {
    ($($name:ident),+ $(,)?) => {
        $(
            pub fn $name() -> Type {
                primitive(stringify!($name))
            }
        )+
    };
}

builtins![
    u8, u16, u32, u64, i8, i16, i32, i64, f8, f16, f32, f64, bool, string, unit
];

/// Return the unqualified name of a type application.
/// Return whether `ty` is the unqualified, zero-argument primitive `name`.
///
/// Identifier spans are source metadata, so semantic primitive checks must
/// compare the identifier text rather than derived `Type` equality.
pub fn is_unqualified_type(ty: &Type, name: &str) -> bool {
    matches!(ty, Type::Apply { ident, arguments } if arguments.is_empty() && ident.as_name() == Some(name))
}

pub fn is_integer_type(ty: &Type) -> bool {
    ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"]
        .iter()
        .any(|name| is_unqualified_type(ty, name))
}

pub fn type_name(ty: &Type) -> Option<&str> {
    fn ident_name(ident: &Ident) -> Option<&str> {
        match ident {
            Ident::Name { name, .. } => Some(name),
            Ident::Qualified { rest, .. } => ident_name(rest),
        }
    }
    match ty {
        Type::Apply { ident, .. } => ident_name(ident),
        _ => None,
    }
}

pub fn canonical_type_ident(name: &str) -> Ident {
    Ident::from_string(name)
}

pub fn is_std_type(ty: &Type, name: &str) -> bool {
    matches!(ty, Type::Apply { ident, .. } if ident.is_qualified_as("Std", name))
}
