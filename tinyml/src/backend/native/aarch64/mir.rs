//! AArch64 target MIR.

use crate::backend::native::ir::Symbol;

/// Selected AArch64 MIR ready for textual assembly emission.
#[derive(Debug, Clone, PartialEq)]
pub struct Mir {
    pub functions: Vec<Function>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
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
    Call {
        symbol: Symbol,
    },
    Ret,
    Comment(String),
}
