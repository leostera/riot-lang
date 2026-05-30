use crate::{
    checker::tst::{BindingId, EntityId, Type},
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
        entity: EntityId,
    },
    Construct {
        name: Ident,
        entity: EntityId,
        args: Vec<Expr>,
    },
    Function {
        params: Vec<Pattern>,
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
    Tuple(Vec<Expr>),
    Record {
        name: Ident,
        fields: Vec<(Ident, Expr)>,
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

/// Primitive operations for built-in behavior.
///
/// Numeric primitives carry the typed operand so backend lowering can
/// specialize later (`u8` addition, `f16` addition, etc.).
#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Add { operand: Type },
    Sub { operand: Type },
    Mul { operand: Type },
    Div { operand: Type },
    Rem { operand: Type },
    Named(Ident),
}

impl Primitive {
    pub fn from_ident(ident: &Ident, operand: Type) -> Self {
        match ident.as_name() {
            Some("+") => Self::Add { operand },
            Some("-") => Self::Sub { operand },
            Some("*") => Self::Mul { operand },
            Some("/") => Self::Div { operand },
            Some("%") => Self::Rem { operand },
            _ => Self::Named(ident.clone()),
        }
    }
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
