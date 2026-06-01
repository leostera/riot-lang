//! Checker built-in types and constants.
//!
//! Centralizes semantic built-ins such as primitive types and the unit
//! constructor name so later phases do not duplicate magic strings.

use crate::parser::ident::Ident;

use super::tir::Type;

pub const UNIT_CONSTRUCTOR: &str = "()";

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
