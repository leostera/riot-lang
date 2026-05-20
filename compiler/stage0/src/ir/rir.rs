use crate::signature::RsigType;

#[derive(Debug, Clone)]
pub(crate) struct RirProgram {
    pub(crate) module_name: String,
    pub(crate) uses: Vec<String>,
    pub(crate) externals: Vec<RirExternal>,
    pub(crate) functions: Vec<RirFunction>,
}

#[derive(Debug, Clone)]
pub(crate) struct RirExternal {
    pub(crate) name: String,
    pub(crate) params: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) abi: String,
}

#[derive(Debug, Clone)]
pub(crate) struct RirFunction {
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) param_types: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) body: RirBlock,
    pub(crate) symbol: String,
}

#[derive(Debug, Clone)]
pub(crate) struct RirBlock {
    pub(crate) statements: Vec<RirStmt>,
    pub(crate) tail: Option<RirExpr>,
}

#[derive(Debug, Clone)]
pub(crate) enum RirStmt {
    Let { name: String, value: RirExpr },
    Expr(RirExpr),
}

#[derive(Debug, Clone)]
pub(crate) enum RirExpr {
    Add(Box<RirExpr>, Box<RirExpr>),
    Sub(Box<RirExpr>, Box<RirExpr>),
    Mul(Box<RirExpr>, Box<RirExpr>),
    Div(Box<RirExpr>, Box<RirExpr>),
    Mod(Box<RirExpr>, Box<RirExpr>),
    Neg(Box<RirExpr>),
    Eq(Box<RirExpr>, Box<RirExpr>),
    Lt(Box<RirExpr>, Box<RirExpr>),
    And(Box<RirExpr>, Box<RirExpr>),
    Or(Box<RirExpr>, Box<RirExpr>),
    Not(Box<RirExpr>),
    If {
        condition: Box<RirExpr>,
        then_branch: Box<RirExpr>,
        else_branch: Box<RirExpr>,
    },
    Bool(bool),
    Call {
        callee: Vec<String>,
        args: Vec<RirExpr>,
    },
    Unit,
    Tuple(Vec<RirExpr>),
    List(Vec<RirExpr>),
    Record {
        path: Vec<String>,
        fields: Vec<(String, RirExpr)>,
    },
    Field {
        base: Box<RirExpr>,
        field: String,
    },
    Char(char),
    Float(String),
    Int(i64),
    Path(Vec<String>),
    String(String),
    Spawn {
        actor_id: usize,
        body: Box<RirBlock>,
    },
    Receive {
        binder: String,
        body: Box<RirExpr>,
    },
}
