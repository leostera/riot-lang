#![allow(dead_code)]

use crate::signature::{
    ConstructorName, FieldName, ModuleName, Rsig, RsigType, RsigTypeScheme, TypeName, TypeParamName,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BindingId {
    pub(crate) name: String,
    pub(crate) id: usize,
}

impl BindingId {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Ident {
    Name(String),
    Qualified(String, Box<Ident>),
}

impl Ident {
    pub(crate) fn from_segments(segments: Vec<String>) -> Self {
        Self::from_segment_slice(&segments)
    }

    fn from_segment_slice(segments: &[String]) -> Self {
        let Some((first, rest)) = segments.split_first() else {
            return Self::Name("_".to_owned());
        };
        if rest.is_empty() {
            Self::Name(first.clone())
        } else {
            Self::Qualified(first.clone(), Box::new(Self::from_segment_slice(rest)))
        }
    }

    pub(crate) fn as_strings(&self) -> Vec<String> {
        match self {
            Ident::Name(name) => vec![name.clone()],
            Ident::Qualified(prefix, rest) => {
                let mut segments = vec![prefix.clone()];
                segments.extend(rest.as_strings());
                segments
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum TypedLiteral {
    Bool(bool),
    Char(char),
    Float(String),
    Int(i64),
    String(String),
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
    pub(crate) params: Vec<TypeParamName>,
    pub(crate) body: TypedTypeBody,
}

#[derive(Debug, Clone)]
pub(crate) enum TypedTypeBody {
    Abstract,
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
    pub(crate) binding: BindingId,
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
        binding: BindingId,
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
pub(crate) struct TypedReceiveArm {
    pub(crate) pattern: TypedPattern,
    pub(crate) body: TypedExpr,
}

#[derive(Debug, Clone)]
pub(crate) enum TypedPattern {
    Wildcard,
    Bind {
        binding: BindingId,
        type_: RsigType,
    },
    Constructor {
        type_name: TypeName,
        constructor: ConstructorName,
        payload: Vec<TypedPattern>,
    },
    Unit,
    Tuple(Vec<TypedPattern>),
    List {
        prefix: Vec<TypedPattern>,
        tail: Option<Box<TypedPattern>>,
    },
    Record {
        type_name: TypeName,
        fields: Vec<(String, TypedPattern)>,
    },
    Bool(bool),
    Int(i64),
    String(String),
}

#[derive(Debug, Clone)]
pub(crate) enum TypedExprKind {
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
    Literal(TypedLiteral),
    Call {
        callee: Ident,
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
    Tuple(Vec<TypedExpr>),
    List(Vec<TypedExpr>),
    Record {
        path: Ident,
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
    Constructor {
        type_name: Option<TypeName>,
        constructor: ConstructorName,
        payload: Vec<TypedExpr>,
    },
    Ident(Ident),
    Local(BindingId),
    Spawn {
        body: Box<TypedBlock>,
    },
    Receive {
        arms: Vec<TypedReceiveArm>,
    },
}
