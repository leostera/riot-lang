use crate::parser::{ast, ident::Ident, lexer::Span};

use super::{builtin, scheme::Scheme};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindingId(usize);

impl BindingId {
    pub(crate) fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityId(usize);

impl EntityId {
    pub(crate) fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(usize);

impl TypeVarId {
    pub(crate) fn new(id: usize) -> Self {
        Self(id)
    }

    pub(crate) fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(TypeVarId),
    Generic(TypeVarId),
    Tuple(Vec<Type>),
    Arrow {
        parameter: Box<Type>,
        result: Box<Type>,
    },
    Apply {
        ident: Ident,
        arguments: Vec<Type>,
    },
}

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
        binding: BindingId,
        entity: EntityId,
        name: Ident,
        hint: Option<Type>,
        body: Expr,
        scheme: Scheme,
        span: Span,
    },
    Fn {
        binding: BindingId,
        entity: EntityId,
        name: Ident,
        args: Vec<Pattern>,
        body: Expr,
        scheme: Scheme,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub name: Ident,
    pub params: Vec<ast::TypeVar>,
    pub body: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub ty: Type,
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn is_unit_constructor(&self) -> bool {
        matches!(
            &self.kind,
            ExprKind::Constructor { name, args, .. }
                if args.is_empty() && matches!(name.as_name(), Some(builtin::UNIT_CONSTRUCTOR))
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal {
        value: ast::Literal,
    },
    Var {
        name: Ident,
        entity: EntityId,
    },
    Constructor {
        name: Ident,
        entity: EntityId,
        args: Vec<Expr>,
    },
    Fn {
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
    BinaryOp {
        ident: Ident,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    TypeHint {
        expr: Box<Expr>,
        hint: Type,
    },
    Tuple {
        items: Vec<Expr>,
    },
    Record {
        name: Ident,
        fields: Vec<(Ident, Expr)>,
    },
    Block {
        exprs: Vec<Expr>,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
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

impl Pattern {
    pub fn is_unit_constructor(&self) -> bool {
        matches!(
            &self.kind,
            PatternKind::Constructor { name, args, .. }
                if args.is_empty() && matches!(name.as_name(), Some(builtin::UNIT_CONSTRUCTOR))
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Var {
        name: Ident,
        binding: BindingId,
    },
    Literal {
        value: ast::Literal,
    },
    Constructor {
        name: Ident,
        entity: EntityId,
        args: Vec<Pattern>,
    },
    Tuple {
        items: Vec<Pattern>,
    },
    TypeHint {
        pattern: Box<Pattern>,
        hint: Type,
    },
    Record {
        name: Ident,
        fields: Vec<(Ident, Pattern)>,
    },
}
