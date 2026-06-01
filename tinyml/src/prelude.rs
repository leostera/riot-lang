//! Built-in TinyML prelude definitions.
//!
//! This module lists primitive type names accepted by the parser/checker and
//! provides one central place for language-level built-ins shared across phases.

pub const PRIMITIVE_TYPES: &[&str] = &[
    "u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f8", "f16", "f32", "f64", "bool",
    "unit", "string",
];
