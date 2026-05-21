use std::fmt;

use crate::signature::{ConstructorName, ModuleName, RsigType, TypeName};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BindingKey(String);

impl BindingKey {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Param(BindingKey);

impl Param {
    pub(crate) fn from_key(key: BindingKey) -> Self {
        Self(key)
    }

    pub(crate) fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub(crate) fn key(&self) -> &BindingKey {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Capture(BindingKey);

impl Capture {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(BindingKey::new(name))
    }

    pub(crate) fn from_key(key: BindingKey) -> Self {
        Self(key)
    }

    pub(crate) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct RirProgram {
    pub(crate) module_name: ModuleName,
    pub(crate) uses: Vec<ModuleName>,
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
    pub(crate) params: Vec<Param>,
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
    Let { name: BindingKey, value: RirExpr },
    Expr(RirExpr),
}

#[derive(Debug, Clone)]
pub(crate) struct RirMatchArm {
    pub(crate) pattern: RirPattern,
    pub(crate) body: RirExpr,
}

#[derive(Debug, Clone)]
pub(crate) struct RirReceiveArm {
    pub(crate) pattern: RirPattern,
    pub(crate) body: RirExpr,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct RirPath(Vec<String>);

impl RirPath {
    pub(crate) fn from_segments(segments: Vec<String>) -> Self {
        Self(segments)
    }

    pub(crate) fn singleton(name: impl Into<String>) -> Self {
        Self(vec![name.into()])
    }

    pub(crate) fn as_slice(&self) -> &[String] {
        &self.0
    }

    pub(crate) fn first(&self) -> Option<&String> {
        self.0.first()
    }

    pub(crate) fn into_segments(self) -> Vec<String> {
        self.0
    }
}

impl fmt::Debug for RirPath {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum RirPattern {
    Wildcard,
    Bind {
        binding: BindingKey,
        type_: RsigType,
    },
    Constructor {
        type_name: TypeName,
        constructor: ConstructorName,
        payload: Vec<RirPattern>,
    },
    Unit,
    Tuple(Vec<RirPattern>),
    List {
        prefix: Vec<RirPattern>,
        tail: Option<Box<RirPattern>>,
    },
    Record {
        type_name: TypeName,
        fields: Vec<(String, RirPattern)>,
    },
    Bool(bool),
    Int(i64),
    String(String),
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
    Match {
        scrutinee: Box<RirExpr>,
        arms: Vec<RirMatchArm>,
    },
    Block(Box<RirBlock>),
    Bool(bool),
    Call {
        callee: Vec<String>,
        args: Vec<RirExpr>,
    },
    Apply {
        callee: Box<RirExpr>,
        args: Vec<RirExpr>,
        result: RsigType,
    },
    Lambda {
        params: Vec<Param>,
        captures: Vec<Capture>,
        body: Box<RirBlock>,
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
    TupleIndex {
        base: Box<RirExpr>,
        index: usize,
    },
    Variant {
        type_name: TypeName,
        constructor: ConstructorName,
        payload: Vec<RirExpr>,
    },
    Char(char),
    Float(String),
    Int(i64),
    Path(RirPath),
    String(String),
    Spawn {
        actor_id: usize,
        body: Box<RirBlock>,
    },
    Receive {
        arms: Vec<RirReceiveArm>,
    },
}
