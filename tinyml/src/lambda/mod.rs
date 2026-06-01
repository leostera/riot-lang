//! Backend-independent Lambda IR stage.
//!
//! Lambda is a semantic, typed intermediate representation shared by backends
//! before target-specific lowering chooses layouts or source forms.

pub mod ir;
pub mod lower;

pub use ir::Module as Lambda;

/// Translate checked typed IR into backend-independent Lambda IR.
pub fn translate(tir: &crate::checker::Tir) -> Lambda {
    lower::Lowerer.lower_module(tir)
}
