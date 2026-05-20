use crate::signature::{
    ConstructorName, FieldName, ModuleName, Rsig, RsigType, RsigTypeScheme, TypeName,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct HirBinding {
    pub(crate) name: String,
    pub(crate) id: usize,
}

impl HirBinding {
    pub(crate) fn new(name: impl Into<String>, id: usize) -> Self {
        Self {
            name: name.into(),
            id,
        }
    }

    pub(crate) fn key_name(&self) -> String {
        format!("{}${}", self.name, self.id)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CheckedProgram {
    pub(crate) typed_tree: TypedProgram,
    pub(crate) signature: Rsig,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedProgram {
    pub(crate) module_name: ModuleName,
    pub(crate) uses: Vec<TypedUse>,
    pub(crate) externals: Vec<TypedExternal>,
    pub(crate) types: Vec<TypedTypeDecl>,
    pub(crate) functions: Vec<TypedFunction>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedUse {
    pub(crate) name: ModuleName,
    pub(crate) fingerprint: u64,
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
pub(crate) struct TypedTypeDecl {
    pub(crate) name: TypeName,
    pub(crate) body: TypedTypeBody,
}

#[derive(Debug, Clone)]
pub(crate) enum TypedTypeBody {
    Variant {
        constructors: Vec<TypedVariantConstructor>,
    },
    Record {
        fields: Vec<TypedRecordField>,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct TypedVariantConstructor {
    pub(crate) name: ConstructorName,
    pub(crate) payload: Vec<RsigType>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedRecordField {
    pub(crate) name: FieldName,
    pub(crate) type_: RsigType,
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
    pub(crate) binding: HirBinding,
    pub(crate) scheme: RsigTypeScheme,
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
    Let {
        binding: HirBinding,
        scheme: RsigTypeScheme,
        value: TypedExpr,
    },
    Expr(TypedExpr),
}

#[derive(Debug, Clone)]
pub(crate) struct TypedExpr {
    pub(crate) type_: RsigType,
    pub(crate) kind: TypedExprKind,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedMatchArm {
    pub(crate) pattern: TypedPattern,
    pub(crate) body: TypedExpr,
}

#[derive(Debug, Clone)]
pub(crate) enum TypedPattern {
    Wildcard,
    Bind {
        binding: HirBinding,
        type_: RsigType,
    },
    Constructor {
        type_name: TypeName,
        constructor: ConstructorName,
    },
    Unit,
    Bool(bool),
    Int(i64),
    String(String),
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
    Match {
        scrutinee: Box<TypedExpr>,
        arms: Vec<TypedMatchArm>,
    },
    Block(Box<TypedBlock>),
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
    Variant {
        type_name: TypeName,
        constructor: ConstructorName,
        payload: Vec<TypedExpr>,
    },
    Char(char),
    Float(String),
    Int(i64),
    Local(HirBinding),
    Path(Vec<String>),
    String(String),
    Spawn {
        body: Box<TypedBlock>,
    },
    Receive {
        binder: HirBinding,
        body: Box<TypedExpr>,
    },
}
