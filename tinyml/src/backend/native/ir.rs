//! Native backend IR.
//!
//! This IR is backend-specific and may contain native representation constructs
//! such as `MakeBlock`, stack/heap layout decisions, and native constants.

use crate::{
    backend::native::layout::BlockLayout,
    checker::tir::{BindingId, EntityId, Type},
    lambda::ir::Primitive,
    parser::{ast, ident::Ident, lexer::Span},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub values: Vec<ValueDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDecl {
    pub binding: BindingId,
    pub entity: EntityId,
    pub name: Ident,
    pub symbol: Symbol,
    pub body: Expr,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub ty: Type,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Constant(Constant),
    Var {
        name: Ident,
        symbol: Symbol,
        entity: EntityId,
    },
    Function {
        params: Vec<Param>,
        body: Box<Expr>,
    },
    Let {
        bind: Pattern,
        hint: Option<Type>,
        body: Box<Expr>,
    },
    Apply {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Prim {
        op: Primitive,
        args: Vec<Expr>,
    },
    MakeBlock {
        layout: BlockLayout,
        fields: Vec<Expr>,
    },
    Block(Vec<Expr>),
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Unit,
    Literal(ast::Literal),
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

impl From<crate::lambda::ir::Constant> for Constant {
    fn from(value: crate::lambda::ir::Constant) -> Self {
        match value {
            crate::lambda::ir::Constant::Unit => Self::Unit,
            crate::lambda::ir::Constant::Literal(value) => Self::Literal(value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub pattern: Pattern,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub ty: Type,
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Var {
        name: Ident,
        binding: BindingId,
    },
    Constant(Constant),
    Construct {
        name: Ident,
        entity: EntityId,
        args: Vec<Pattern>,
    },
    Tuple(Vec<Pattern>),
    Record {
        name: Ident,
        fields: Vec<(Ident, Pattern)>,
    },
}
