use crate::signature::{Rsig, RsigType};

#[derive(Debug, Clone)]
pub(crate) struct CheckedProgram {
    pub(crate) typed_tree: TypedProgram,
    pub(crate) signature: Rsig,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedProgram {
    pub(crate) module_name: String,
    pub(crate) uses: Vec<TypedUse>,
    pub(crate) externals: Vec<TypedExternal>,
    pub(crate) functions: Vec<TypedFunction>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedUse {
    pub(crate) name: String,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedExternal {
    pub(crate) name: String,
    pub(crate) type_text: String,
    pub(crate) params: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) abi: String,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedFunction {
    pub(crate) name: String,
    pub(crate) params: Vec<TypedParam>,
    pub(crate) body: TypedBlock,
    pub(crate) result: RsigType,
    pub(crate) symbol: String,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedParam {
    pub(crate) name: String,
    pub(crate) type_: RsigType,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedBlock {
    pub(crate) statements: Vec<TypedStmt>,
    pub(crate) tail: Option<TypedExpr>,
    pub(crate) type_: RsigType,
}

#[derive(Debug, Clone)]
pub(crate) enum TypedStmt {
    Let { name: String, value: TypedExpr },
    Expr(TypedExpr),
}

#[derive(Debug, Clone)]
pub(crate) struct TypedExpr {
    pub(crate) type_: RsigType,
    pub(crate) kind: TypedExprKind,
}

#[derive(Debug, Clone)]
pub(crate) enum TypedExprKind {
    Add(Box<TypedExpr>, Box<TypedExpr>),
    Sub(Box<TypedExpr>, Box<TypedExpr>),
    Mul(Box<TypedExpr>, Box<TypedExpr>),
    Div(Box<TypedExpr>, Box<TypedExpr>),
    Mod(Box<TypedExpr>, Box<TypedExpr>),
    Neg(Box<TypedExpr>),
    Eq(Box<TypedExpr>, Box<TypedExpr>),
    Lt(Box<TypedExpr>, Box<TypedExpr>),
    And(Box<TypedExpr>, Box<TypedExpr>),
    Or(Box<TypedExpr>, Box<TypedExpr>),
    Not(Box<TypedExpr>),
    If {
        condition: Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Box<TypedExpr>,
    },
    Bool(bool),
    Call {
        callee: Vec<String>,
        args: Vec<TypedExpr>,
    },
    Apply {
        callee: Box<TypedExpr>,
        args: Vec<TypedExpr>,
    },
    Lambda {
        params: Vec<TypedParam>,
        body: Box<TypedBlock>,
    },
    Unit,
    Tuple(Vec<TypedExpr>),
    List(Vec<TypedExpr>),
    Record {
        path: Vec<String>,
        fields: Vec<(String, TypedExpr)>,
    },
    Field {
        base: Box<TypedExpr>,
        field: String,
    },
    TupleIndex {
        base: Box<TypedExpr>,
        index: usize,
    },
    Char(char),
    Float(String),
    Int(i64),
    Path(Vec<String>),
    String(String),
    Spawn {
        body: Box<TypedBlock>,
    },
    Receive {
        binder: String,
        body: Box<TypedExpr>,
    },
}
