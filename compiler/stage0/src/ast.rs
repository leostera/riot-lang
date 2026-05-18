use chumsky::span::SimpleSpan;

pub(crate) type TextSpan = SimpleSpan<usize>;

#[derive(Debug, Clone)]
pub(crate) struct AstProgram {
    pub(crate) decls: Vec<AstFnDecl>,
}

#[derive(Debug, Clone)]
pub(crate) struct AstFnDecl {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) body: AstBlock,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone)]
pub(crate) struct AstBlock {
    pub(crate) statements: Vec<AstStmt>,
    pub(crate) tail: Option<AstExpr>,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone)]
pub(crate) enum AstStmt {
    Let {
        name: String,
        name_span: TextSpan,
        value: AstExpr,
        span: TextSpan,
    },
    Expr(AstExpr),
}

#[derive(Debug, Clone)]
pub(crate) enum AstExpr {
    Add {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    Sub {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    Mul {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    Div {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    Mod {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    Neg {
        expr: Box<AstExpr>,
        span: TextSpan,
    },
    Eq {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    Bool { value: bool, span: TextSpan },
    Call { callee: AstPath, args: Vec<AstExpr> },
    Int { value: i64, span: TextSpan },
    Path { path: AstPath, span: TextSpan },
    String { value: String, span: TextSpan },
}

#[derive(Debug, Clone)]
pub(crate) struct AstPath {
    pub(crate) segments: Vec<String>,
}
