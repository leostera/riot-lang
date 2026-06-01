//! Native code generation pipeline.
//!
//! Native lowering consumes Lambda, produces machine-independent NIR, then target
//! selectors produce target MIRs such as AArch64 MIR for emission.

pub mod aarch64;
pub mod ir;
pub mod layout;
pub mod lower;

pub use ir::Module as Nir;

/// Lower Lambda into TinyML's native, machine-independent IR.
pub fn lower(lambda: &crate::lambda::Lambda) -> Nir {
    lower::lower_module(lambda)
}
