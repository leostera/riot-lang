use crate::signature::{
    ConstructorName, FieldName, ModuleName, Rsig, RsigType, TypeName, TypeParamName,
};
use std::collections::BTreeMap;

use crate::ast::{AstProgram, TextSpan};
use crate::signature::ImportedSignatures;

pub(crate) struct TyIrBuilder<'a> {
    module_name: ModuleName,
    imports: &'a ImportedSignatures,
    function_types: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    expression_types: Option<&'a BTreeMap<TextSpan, RsigType>>,
}

impl<'a> TyIrBuilder<'a> {
    pub(crate) fn new(
        module_name: ModuleName,
        imports: &'a ImportedSignatures,
        function_types: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
        expression_types: Option<&'a BTreeMap<TextSpan, RsigType>>,
    ) -> Self {
        Self {
            module_name,
            imports,
            function_types,
            expression_types,
        }
    }

    pub(crate) fn build(self, ast: AstProgram) -> TypedProgram {
        super::lower::typed_program_from_ast(
            self.module_name,
            ast,
            self.imports,
            self.function_types,
            self.expression_types,
        )
    }
}

#[derive(Debug, Default)]
pub(crate) struct RsigBuilder;

impl RsigBuilder {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn build(&self, program: &TypedProgram) -> Rsig {
        super::signature::signature_for(program)
    }
}

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
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct SurfacePath {
    segments: Vec<String>,
}

impl SurfacePath {
    pub(crate) fn from_segments(segments: Vec<String>) -> Self {
        if segments.is_empty() {
            Self {
                segments: vec!["_".to_owned()],
            }
        } else {
            Self { segments }
        }
    }

    pub(crate) fn as_strings(&self) -> Vec<String> {
        self.segments.clone()
    }

    pub(crate) fn as_string(&self) -> String {
        self.segments.join(".")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct EntityId {
    pub(crate) binding_id: BindingId,
    pub(crate) surface_path: SurfacePath,
}

impl EntityId {
    pub(crate) fn from_segments(segments: Vec<String>) -> Self {
        let surface_path = SurfacePath::from_segments(segments);
        Self {
            binding_id: BindingId::new(surface_path.as_string(), usize::MAX),
            surface_path,
        }
    }

    pub(crate) fn as_strings(&self) -> Vec<String> {
        self.surface_path.as_strings()
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
        callee: EntityId,
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
        path: EntityId,
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
    Entity(EntityId),
    Local(BindingId),
    Spawn {
        body: Box<TypedBlock>,
    },
    Receive {
        arms: Vec<TypedReceiveArm>,
    },
}
