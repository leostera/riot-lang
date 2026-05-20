#![allow(dead_code)]

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TextSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl TextSpan {
    pub(crate) fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub(crate) fn join(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct AstProgram {
    pub(crate) decls: Vec<AstDecl>,
}

#[derive(Debug, Clone)]
pub(crate) enum AstDecl {
    Use(AstUseDecl),
    Module(AstModuleDecl),
    Include(AstIncludeDecl),
    External(AstExternalDecl),
    Type(AstTypeDecl),
    Function(AstFnDecl),
}

#[derive(Debug, Clone)]
pub(crate) struct AstUseDecl {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone)]
pub(crate) struct AstModuleDecl {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) public: bool,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone)]
pub(crate) struct AstIncludeDecl {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone)]
pub(crate) struct AstExternalDecl {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) type_text: String,
    pub(crate) type_span: TextSpan,
    pub(crate) abi: String,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone)]
pub(crate) struct AstTypeDecl {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) params: Vec<AstTypeParam>,
    pub(crate) body: AstTypeBody,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone)]
pub(crate) struct AstTypeParam {
    pub(crate) name: String,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone)]
pub(crate) enum AstTypeBody {
    Abstract,
    Variant {
        constructors: Vec<AstVariantConstructor>,
    },
    Record {
        fields: Vec<AstRecordTypeField>,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct AstVariantConstructor {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) payload: Vec<AstTypeAnnotation>,
}

#[derive(Debug, Clone)]
pub(crate) struct AstRecordTypeField {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) type_annotation: AstTypeAnnotation,
}

#[derive(Debug, Clone)]
pub(crate) struct AstFnDecl {
    pub(crate) name: String,
    pub(crate) name_span: TextSpan,
    pub(crate) params: Vec<String>,
    pub(crate) param_types: Vec<Option<AstTypeAnnotation>>,
    pub(crate) return_type: Option<AstTypeAnnotation>,
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
pub(crate) struct AstMatchArm {
    pub(crate) pattern: AstPattern,
    pub(crate) body: AstExpr,
}

#[derive(Debug, Clone)]
pub(crate) struct AstReceiveArm {
    pub(crate) pattern: AstPattern,
    pub(crate) body: AstExpr,
}

#[derive(Debug, Clone)]
pub(crate) enum AstPattern {
    Wildcard {
        span: TextSpan,
    },
    Bind {
        name: String,
        span: TextSpan,
    },
    Constructor {
        path: AstPath,
        payload: Vec<AstPattern>,
        span: TextSpan,
    },
    Unit {
        span: TextSpan,
    },
    Tuple {
        items: Vec<AstPattern>,
        span: TextSpan,
    },
    Record {
        path: AstPath,
        fields: Vec<(String, AstPattern)>,
        span: TextSpan,
    },
    Bool {
        value: bool,
        span: TextSpan,
    },
    Int {
        value: i64,
        span: TextSpan,
    },
    String {
        value: String,
        span: TextSpan,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum AstStmt {
    Let {
        name: String,
        name_span: TextSpan,
        type_annotation: Option<AstTypeAnnotation>,
        value: AstExpr,
        span: TextSpan,
    },
    Expr(AstExpr),
}

#[derive(Debug, Clone)]
pub(crate) struct AstTypeAnnotation {
    pub(crate) text: String,
    pub(crate) span: TextSpan,
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
    Lt {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    And {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    Or {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        span: TextSpan,
    },
    Not {
        expr: Box<AstExpr>,
        span: TextSpan,
    },
    If {
        condition: Box<AstExpr>,
        then_branch: Box<AstExpr>,
        else_branch: Box<AstExpr>,
        span: TextSpan,
    },
    Match {
        scrutinee: Box<AstExpr>,
        arms: Vec<AstMatchArm>,
        span: TextSpan,
    },
    Block {
        block: Box<AstBlock>,
        span: TextSpan,
    },
    Bool {
        value: bool,
        span: TextSpan,
    },
    Call {
        callee: AstPath,
        args: Vec<AstExpr>,
        span: TextSpan,
    },
    Apply {
        callee: Box<AstExpr>,
        args: Vec<AstExpr>,
        span: TextSpan,
    },
    Lambda {
        params: Vec<String>,
        param_types: Vec<Option<AstTypeAnnotation>>,
        body: Box<AstBlock>,
        span: TextSpan,
    },
    Spawn {
        body: Box<AstBlock>,
        span: TextSpan,
    },
    Receive {
        arms: Vec<AstReceiveArm>,
        span: TextSpan,
    },
    Unit {
        span: TextSpan,
    },
    Tuple {
        items: Vec<AstExpr>,
        span: TextSpan,
    },
    List {
        items: Vec<AstExpr>,
        span: TextSpan,
    },
    Record {
        path: AstPath,
        fields: Vec<(String, AstExpr)>,
        span: TextSpan,
    },
    Field {
        base: Box<AstExpr>,
        field: String,
        span: TextSpan,
    },
    TupleIndex {
        base: Box<AstExpr>,
        index: usize,
        span: TextSpan,
    },
    Char {
        value: char,
        span: TextSpan,
    },
    Float {
        value: String,
        span: TextSpan,
    },
    Int {
        value: i64,
        span: TextSpan,
    },
    Path {
        path: AstPath,
        span: TextSpan,
    },
    String {
        value: String,
        span: TextSpan,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct AstPath {
    pub(crate) segments: Vec<String>,
}
