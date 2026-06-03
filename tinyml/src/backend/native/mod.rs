//! Native code generation pipeline.
//!
//! Native lowering consumes Lambda, produces machine-independent native SSA,
//! then target selectors produce target MIRs such as AArch64 MIR for emission.

pub mod aarch64;
pub mod layout;
pub mod opt;
pub mod ssa;

/// Lower Lambda into TinyML's native SSA IR.
pub fn lower(lambda: &crate::lambda::Lambda) -> ssa::SsaProgram {
    ssa::builder::lower_module(lambda)
}
