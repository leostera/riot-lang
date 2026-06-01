//! Type checker and typed tree construction.
//!
//! The checker owns type state, environments, unification, typed diagnostics, and
//! the source-shaped typed IR consumed by later lowering stages.

pub mod builtin;
pub mod checker;
pub mod diagnostic;
pub mod env;
pub mod interface;
pub mod scheme;
pub mod state;
pub mod tir;
pub mod type_expr;
pub mod typer;
pub mod unifier;

pub use checker::{Checker, ModuleSummary};
pub use diagnostic::CheckDiagnostic;
pub use interface::ModuleInterface;
pub use tir::Module as Tir;

/// Check a parsed TinyML AST and return the full checker summary.
pub fn check_module(ast: &crate::parser::Ast) -> ModuleSummary {
    Checker::new().check_module(ast)
}

/// Check a parsed TinyML AST and return just the typed IR.
///
/// Use [`check_module`] when diagnostics or the exported module interface are
/// needed by callers such as the JS backend.
pub fn check(ast: &crate::parser::Ast) -> Tir {
    check_module(ast).tir
}
