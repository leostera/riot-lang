use crate::{
    backend::native::layout::BlockLayout,
    checker::tst::{BindingId, EntityId, Type},
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
