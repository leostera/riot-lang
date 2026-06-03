//! AArch64 target MIR.

use crate::backend::native::ssa::Symbol;

/// Selected AArch64 MIR ready for textual assembly emission.
#[derive(Debug, Clone, PartialEq)]
pub struct Mir {
    pub entries: Vec<Entry>,
    pub diagnostics: Vec<Diagnostic>,
}

/// One emitted assembly entry point.
///
/// Native globals and TinyML functions are both currently emitted as callable
/// labels so existing call-based global lookup and function invocation can share
/// the same AArch64 path. This is intentionally named `Entry`, not `Function`,
/// because it is an assembly-level label plus instructions, not necessarily a
/// source/native function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    pub name: Symbol,
    pub instructions: Vec<Instr>,
    pub stack_slots: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Diagnostic {
    UnsupportedTopLevelValue { name: Symbol },
    UnsupportedExpression { context: &'static str },
    UnsupportedLiteral { literal: String },
    TooManyFunctionParameters { name: Symbol, count: usize },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PhysReg {
    X(u8),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    Label(Symbol),
    BlockLabel(String),
    MovImm {
        dst: PhysReg,
        value: i64,
    },
    Mov {
        dst: PhysReg,
        src: PhysReg,
    },
    LoadStack {
        dst: PhysReg,
        slot: usize,
    },
    StoreStack {
        slot: usize,
        src: PhysReg,
    },
    Add {
        dst: PhysReg,
        lhs: PhysReg,
        rhs: PhysReg,
    },
    Sub {
        dst: PhysReg,
        lhs: PhysReg,
        rhs: PhysReg,
    },
    Mul {
        dst: PhysReg,
        lhs: PhysReg,
        rhs: PhysReg,
    },
    SDiv {
        dst: PhysReg,
        lhs: PhysReg,
        rhs: PhysReg,
    },
    MSub {
        dst: PhysReg,
        lhs: PhysReg,
        rhs: PhysReg,
        acc: PhysReg,
    },
    CmpImm {
        lhs: PhysReg,
        imm: i64,
    },
    BCond {
        cond: String,
        label: String,
    },
    B {
        label: String,
    },
    Call {
        symbol: Symbol,
    },
    Ret,
    Comment(String),
}
