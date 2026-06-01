//! AArch64 native backend.

pub mod emit;
pub mod mir;
pub mod select;

pub use emit::Emitter;
pub use mir::{Diagnostic, Function, Instr, Mir, PhysReg};
pub use select::select;
