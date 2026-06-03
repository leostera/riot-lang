//! Native backend SSA IR.
//!
//! Native IR is the first machine-oriented IR after Lambda.  It has explicit
//! basic blocks, simple SSA values, block parameters, and a small machine
//! representation classification.  It deliberately avoids target registers and
//! stack slots; those belong to target backends.

pub mod builder;
mod printer;

use crate::{
    checker::tir::{BindingId, EntityId, Type},
    lambda::ir::Primitive,
    parser::{ast, ident::Ident, lexer::Span},
};

#[derive(Debug, Clone, PartialEq)]
pub struct SsaProgram {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
}

impl SsaProgram {
    pub fn optimize(mut self) -> Self {
        crate::backend::native::opt::optimize(&mut self);
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    pub binding: BindingId,
    pub entity: EntityId,
    pub name: Ident,
    pub symbol: Symbol,
    pub value: Constant,
    pub repr: MachineRepr,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub binding: BindingId,
    pub entity: EntityId,
    pub name: Ident,
    pub symbol: Symbol,
    pub params: Vec<ValueId>,
    pub values: Vec<ValueInfo>,
    pub entry: BlockId,
    pub blocks: Vec<Block>,
    pub ret: MachineRepr,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct ValueInfo {
    pub repr: MachineRepr,
    pub ty: Type,
    pub debug_name: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MachineRepr {
    Unit,
    Bool,
    I32,
    I64,
    F32,
    F64,
    Ptr,
}

impl MachineRepr {
    pub fn of_type(ty: &Type) -> Self {
        match ty {
            Type::Apply { ident, .. } => match ident.as_name() {
                Some("unit") => Self::Unit,
                Some("bool") => Self::Bool,
                Some("i8" | "i16" | "i32" | "u8" | "u16" | "u32") => Self::I32,
                Some("i64" | "u64") => Self::I64,
                Some("f32") => Self::F32,
                Some("f64") => Self::F64,
                Some("string") => Self::Ptr,
                _ => Self::Ptr,
            },
            Type::Arrow { .. } => Self::Ptr,
            Type::Tuple(_) => Self::Ptr,
            Type::Var(_) | Type::Generic(_) => Self::Ptr,
        }
    }

    pub fn is_zero_sized(self) -> bool {
        matches!(self, Self::Unit)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub id: BlockId,
    pub params: Vec<ValueId>,
    pub stmts: Vec<Stmt>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assign {
        dst: ValueId,
        rhs: Rvalue,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Rvalue {
    Const(Constant),
    Copy(ValueId),
    Prim {
        op: Primitive,
        args: Vec<ValueId>,
    },
    Call {
        callee: Symbol,
        args: Vec<ValueId>,
    },
    Global(Symbol),
    MakeBlock {
        tag: BlockTag,
        fields: Vec<ValueId>,
    },
    GetField {
        block: ValueId,
        index: usize,
    },
    GetTag {
        block: ValueId,
    },
    StringLiteral {
        value: String,
    },
    FloatLiteral {
        value: String,
    },
    RuntimeMatch {
        scrutinee: ValueId,
        arm_count: usize,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockTag {
    Tuple,
    Record,
    Variant(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Return(Option<ValueId>),
    Jump {
        target: BlockId,
        args: Vec<ValueId>,
    },
    Branch {
        cond: ValueId,
        then_target: BlockId,
        then_args: Vec<ValueId>,
        else_target: BlockId,
        else_args: Vec<ValueId>,
    },
    Switch {
        discr: ValueId,
        cases: Vec<(i64, BlockId, Vec<ValueId>)>,
        default: BlockId,
        default_args: Vec<ValueId>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(String);

impl Symbol {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub fn from_ident(ident: &Ident) -> Self {
        Self(symbol_name(ident))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

fn symbol_name(ident: &Ident) -> String {
    match ident {
        Ident::Name { name, .. } => sanitize_symbol(name),
        Ident::Qualified { name, rest, .. } => {
            format!("{}_{}", sanitize_symbol(name), symbol_name(rest))
        }
    }
}

fn sanitize_symbol(name: &str) -> String {
    name.chars()
        .map(|ch| {
            if ch == '_' || ch.is_ascii_alphanumeric() {
                ch
            } else {
                '_'
            }
        })
        .collect()
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Unit,
    Literal(ast::Literal),
    ConstructorTag { name: Ident, tag: u32 },
}

impl From<crate::lambda::ir::Constant> for Constant {
    fn from(value: crate::lambda::ir::Constant) -> Self {
        match value {
            crate::lambda::ir::Constant::Unit => Self::Unit,
            crate::lambda::ir::Constant::Literal(value) => Self::Literal(value),
        }
    }
}
