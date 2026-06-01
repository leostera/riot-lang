//! Backend-specific compiler stages.
//!
//! Backends consume Lambda and produce target-specific representations such as
//! TypeScript source, native assembly, or future WASM.

pub mod js;
pub mod native;
