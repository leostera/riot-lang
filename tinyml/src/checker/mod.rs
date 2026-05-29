pub mod builtin;
pub mod checker;
pub mod diagnostic;
pub mod env;
pub mod scheme;
pub mod state;
pub mod tst;
pub mod type_expr;
pub mod typer;
pub mod unifier;

pub use checker::{Checker, ModuleSummary};
pub use diagnostic::CheckDiagnostic;
