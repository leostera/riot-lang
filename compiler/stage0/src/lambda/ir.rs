use std::fmt;

use crate::signature::{ConstructorName, ModuleName, RsigType, TypeName};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BindingKey {
    source_name: String,
    generation: Option<usize>,
    rendered: String,
}

impl BindingKey {
    #[cfg(test)]
    pub(crate) fn new(name: impl Into<String>) -> Self {
        let source_name = name.into();
        Self {
            rendered: source_name.clone(),
            source_name,
            generation: None,
        }
    }

    pub(crate) fn resolved(name: impl Into<String>, generation: usize) -> Self {
        let source_name = name.into();
        Self {
            rendered: format!("{source_name}${generation}"),
            source_name,
            generation: Some(generation),
        }
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.rendered
    }
}

impl fmt::Debug for BindingKey {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rendered = match self.generation {
            Some(generation) => format!("{}${generation}", self.source_name),
            None => self.source_name.clone(),
        };
        formatter
            .debug_tuple("BindingKey")
            .field(&rendered)
            .finish()
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
    pub(crate) fn from_key(key: BindingKey) -> Self {
        Self(key)
    }

    pub(crate) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct LambdaProgram {
    pub(crate) module_name: ModuleName,
    pub(crate) uses: Vec<ModuleName>,
    pub(crate) externals: Vec<LambdaExternal>,
    pub(crate) functions: Vec<LambdaFunction>,
}

#[derive(Debug, Clone)]
pub(crate) struct LambdaExternal {
    pub(crate) name: String,
    pub(crate) params: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) abi: String,
}

#[derive(Debug, Clone)]
pub(crate) struct LambdaFunction {
    pub(crate) name: String,
    pub(crate) params: Vec<Param>,
    pub(crate) param_types: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) body: LambdaBlock,
    pub(crate) symbol: String,
}

#[derive(Debug, Clone)]
pub(crate) struct LambdaBlock {
    pub(crate) statements: Vec<LambdaStmt>,
    pub(crate) tail: Option<LambdaExpr>,
}

#[derive(Debug, Clone)]
pub(crate) enum LambdaStmt {
    Let { name: BindingKey, value: LambdaExpr },
    Expr(LambdaExpr),
}

#[derive(Debug, Clone)]
pub(crate) struct LambdaMatchArm {
    pub(crate) pattern: LambdaPattern,
    pub(crate) body: LambdaExpr,
}

#[derive(Debug, Clone)]
pub(crate) struct LambdaReceiveArm {
    pub(crate) pattern: LambdaPattern,
    pub(crate) body: LambdaExpr,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct LambdaPath(Vec<String>);

impl LambdaPath {
    pub(crate) fn from_segments(segments: Vec<String>) -> Self {
        Self(segments)
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

impl fmt::Debug for LambdaPath {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum LambdaPattern {
    Wildcard,
    Bind {
        binding: BindingKey,
        type_: RsigType,
    },
    Constructor {
        type_name: TypeName,
        constructor: ConstructorName,
        payload: Vec<LambdaPattern>,
    },
    Unit,
    Tuple(Vec<LambdaPattern>),
    List {
        prefix: Vec<LambdaPattern>,
        tail: Option<Box<LambdaPattern>>,
    },
    Record {
        type_name: TypeName,
        fields: Vec<(String, LambdaPattern)>,
    },
    Bool(bool),
    Int(i64),
    String(String),
}

#[derive(Debug, Clone)]
pub(crate) enum LambdaExpr {
    Add(Box<LambdaExpr>, Box<LambdaExpr>),
    Sub(Box<LambdaExpr>, Box<LambdaExpr>),
    Mul(Box<LambdaExpr>, Box<LambdaExpr>),
    Div(Box<LambdaExpr>, Box<LambdaExpr>),
    Mod(Box<LambdaExpr>, Box<LambdaExpr>),
    Neg(Box<LambdaExpr>),
    Eq(Box<LambdaExpr>, Box<LambdaExpr>),
    Lt(Box<LambdaExpr>, Box<LambdaExpr>),
    And(Box<LambdaExpr>, Box<LambdaExpr>),
    Or(Box<LambdaExpr>, Box<LambdaExpr>),
    Not(Box<LambdaExpr>),
    If {
        condition: Box<LambdaExpr>,
        then_branch: Box<LambdaExpr>,
        else_branch: Box<LambdaExpr>,
    },
    Match {
        scrutinee: Box<LambdaExpr>,
        arms: Vec<LambdaMatchArm>,
    },
    Block(Box<LambdaBlock>),
    Bool(bool),
    Call {
        callee: Vec<String>,
        args: Vec<LambdaExpr>,
    },
    Apply {
        callee: Box<LambdaExpr>,
        args: Vec<LambdaExpr>,
        result: RsigType,
    },
    Lambda {
        params: Vec<Param>,
        captures: Vec<Capture>,
        body: Box<LambdaBlock>,
    },
    Unit,
    Tuple(Vec<LambdaExpr>),
    List(Vec<LambdaExpr>),
    Record {
        path: Vec<String>,
        fields: Vec<(String, LambdaExpr)>,
    },
    Field {
        base: Box<LambdaExpr>,
        field: String,
    },
    TupleIndex {
        base: Box<LambdaExpr>,
        index: usize,
    },
    Variant {
        type_name: TypeName,
        constructor: ConstructorName,
        payload: Vec<LambdaExpr>,
    },
    Char(char),
    Float(String),
    Int(i64),
    Path(LambdaPath),
    Local(BindingKey),
    String(String),
    Spawn {
        actor_id: usize,
        body: Box<LambdaBlock>,
    },
    Receive {
        arms: Vec<LambdaReceiveArm>,
    },
}
