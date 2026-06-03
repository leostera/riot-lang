//! TinyML compiler library.
//!
//! This crate exposes the compiler pipeline as reusable modules: parser AST,
//! checker/TIR, Lambda IR, and backend-specific lowering/emission. Integration
//! tests and the CLI both use these APIs directly so the compiler can be tested
//! without shelling out to the binary.

pub mod backend;
pub mod checker;
pub mod lambda;
pub mod lsp;
pub mod parser;
pub mod prelude;
