#![allow(dead_code)]

use super::{ident::Ident, lexer::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub items: Vec<ModuleItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleItem {
    UseDecl(UseDecl),
    LetDecl(LetDecl),
    TypeDecl(TypeDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseDecl {
    Concrete {
        module: Ident,
        members: Vec<Ident>,
        span: Span,
    },
    Wildcard {
        module: Ident,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetDecl {
    Const {
        name: Ident,
        body: Expr,
        span: Span,
    },
    Fn {
        name: Ident,
        args: Vec<Pattern>,
        body: Expr,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub name: Ident,
    pub params: Vec<TypeVar>,
    pub body: TypeExpr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeVar {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variant {
    TupleConstructor {
        ident: Ident,
        args: Vec<TypeExpr>,
        span: Span,
    },
    RecordConstructor {
        ident: Ident,
        fields: Vec<(Ident, TypeExpr)>,
        span: Span,
    },
}

impl Variant {
    pub fn span(&self) -> Span {
        match self {
            Variant::TupleConstructor { span, .. } | Variant::RecordConstructor { span, .. } => {
                *span
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(String),
    Float(String),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal {
        value: Literal,
        span: Span,
    },
    Var {
        name: Ident,
        span: Span,
    },
    Constructor {
        name: Ident,
        args: Vec<Expr>,
        span: Span,
    },
    Fn {
        params: Vec<Pattern>,
        body: Box<Expr>,
        span: Span,
    },
    Let {
        bind: Pattern,
        hint: Option<TypeExpr>,
        body: Box<Expr>,
        span: Span,
    },
    Apply {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    BinaryOp {
        ident: Ident,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    TypeHint {
        expr: Box<Expr>,
        hint: TypeExpr,
        span: Span,
    },
    Tuple {
        items: Vec<Expr>,
        span: Span,
    },
    Record {
        name: Ident,
        fields: Vec<(Ident, Expr)>,
        span: Span,
    },
    Block {
        exprs: Vec<Expr>,
        span: Span,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal { span, .. }
            | Expr::Var { span, .. }
            | Expr::Constructor { span, .. }
            | Expr::Fn { span, .. }
            | Expr::Let { span, .. }
            | Expr::Apply { span, .. }
            | Expr::BinaryOp { span, .. }
            | Expr::TypeHint { span, .. }
            | Expr::Tuple { span, .. }
            | Expr::Record { span, .. }
            | Expr::Block { span, .. }
            | Expr::Match { span, .. } => *span,
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
pub enum Pattern {
    Wildcard {
        span: Span,
    },
    Var {
        name: Ident,
        span: Span,
    },
    Literal {
        value: Literal,
        span: Span,
    },
    Constructor {
        name: Ident,
        args: Vec<Pattern>,
        span: Span,
    },
    Tuple {
        items: Vec<Pattern>,
        span: Span,
    },
    TypeHint {
        pattern: Box<Pattern>,
        hint: TypeExpr,
        span: Span,
    },
    Record {
        name: Ident,
        fields: Vec<(Ident, Pattern)>,
        span: Span,
    },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard { span }
            | Pattern::Var { span, .. }
            | Pattern::Literal { span, .. }
            | Pattern::Constructor { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::TypeHint { span, .. }
            | Pattern::Record { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Var {
        var: TypeVar,
        span: Span,
    },
    App {
        name: Ident,
        args: Vec<TypeExpr>,
        span: Span,
    },
    Fn {
        params: Vec<TypeExpr>,
        result: Box<TypeExpr>,
        span: Span,
    },
    Tuple {
        items: Vec<TypeExpr>,
        span: Span,
    },
    Record {
        name: Ident,
        fields: Vec<(Ident, TypeExpr)>,
        span: Span,
    },
    Variant {
        variants: Vec<Variant>,
        span: Span,
    },
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        match self {
            TypeExpr::Var { span, .. }
            | TypeExpr::App { span, .. }
            | TypeExpr::Fn { span, .. }
            | TypeExpr::Tuple { span, .. }
            | TypeExpr::Record { span, .. }
            | TypeExpr::Variant { span, .. } => *span,
        }
    }
}
