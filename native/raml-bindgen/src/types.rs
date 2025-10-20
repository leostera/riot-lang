use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bindings {
    pub functions: Vec<Function>,
    pub types: Vec<TypeDef>,
    pub modules: Vec<Module>,
}

impl Bindings {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            types: Vec::new(),
            modules: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub rust_name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub docs: Option<String>,
    pub is_unsafe: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeDef {
    pub name: String,
    pub rust_name: String,
    pub kind: TypeKind,
    pub docs: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeKind {
    Struct {
        fields: Vec<Field>,
    },
    Enum {
        variants: Vec<Variant>,
    },
    Alias {
        target: Type,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub docs: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Type>,
    pub docs: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Unit,
    Bool,
    I8, I16, I32, I64, Isize,
    U8, U16, U32, U64, Usize,
    F32, F64,
    String,
    Vec(Box<Type>),
    Option(Box<Type>),
    Result { ok: Box<Type>, err: Box<Type> },
    Tuple(Vec<Type>),
    Named(String),
    Reference { mutable: bool, inner: Box<Type> },
}

impl Type {
    pub fn to_ocaml(&self) -> String {
        match self {
            Type::Unit => "unit".to_string(),
            Type::Bool => "bool".to_string(),
            Type::I8 | Type::I16 | Type::I32 | Type::Isize => "int".to_string(),
            Type::I64 => "int64".to_string(),
            Type::U8 | Type::U16 | Type::U32 | Type::Usize => "int".to_string(),
            Type::U64 => "int64".to_string(),
            Type::F32 | Type::F64 => "float".to_string(),
            Type::String => "string".to_string(),
            Type::Vec(inner) => format!("{} list", inner.to_ocaml()),
            Type::Option(inner) => format!("{} option", inner.to_ocaml()),
            Type::Result { ok, err } => {
                format!("({}, {}) result", ok.to_ocaml(), err.to_ocaml())
            }
            Type::Tuple(types) => {
                let types_str = types.iter()
                    .map(|t| t.to_ocaml())
                    .collect::<Vec<_>>()
                    .join(" * ");
                format!("({})", types_str)
            }
            Type::Named(name) => name.to_lowercase(),
            Type::Reference { inner, .. } => inner.to_ocaml(),
        }
    }
}
