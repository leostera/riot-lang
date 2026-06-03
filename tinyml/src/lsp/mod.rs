//! TinyML Language Server Protocol support.
//!
//! The LSP server is intentionally small and single-module. It reuses the
//! existing parser/checker pipeline for diagnostics, inlay hints, and hover type
//! information over open in-memory documents.

mod diagnostics;
mod position;
mod server;
mod type_hints;

pub use server::{check_source, run_stdio};
