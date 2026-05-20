#![allow(dead_code)]

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PrimitiveType {
    Bool,
    Byte,
    Bytes,
    Char,
    String,
    Unit,
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    F16,
    F32,
    F64,
}

impl PrimitiveType {
    pub(super) fn name(self) -> &'static str {
        match self {
            PrimitiveType::Bool => "bool",
            PrimitiveType::Byte => "byte",
            PrimitiveType::Bytes => "bytes",
            PrimitiveType::Char => "char",
            PrimitiveType::String => "string",
            PrimitiveType::Unit => "unit",
            PrimitiveType::I8 => "i8",
            PrimitiveType::I16 => "i16",
            PrimitiveType::I32 => "i32",
            PrimitiveType::I64 => "i64",
            PrimitiveType::I128 => "i128",
            PrimitiveType::ISize => "isize",
            PrimitiveType::U8 => "u8",
            PrimitiveType::U16 => "u16",
            PrimitiveType::U32 => "u32",
            PrimitiveType::U64 => "u64",
            PrimitiveType::U128 => "u128",
            PrimitiveType::USize => "usize",
            PrimitiveType::F16 => "f16",
            PrimitiveType::F32 => "f32",
            PrimitiveType::F64 => "f64",
        }
    }
}

pub(super) struct TypeAnnotationError {
    pub(super) message: String,
    pub(super) help: Option<&'static str>,
}

pub(super) fn parse_primitive_type(text: &str) -> Result<PrimitiveType, TypeAnnotationError> {
    match text {
        "bool" => Ok(PrimitiveType::Bool),
        "byte" => Ok(PrimitiveType::Byte),
        "bytes" => Ok(PrimitiveType::Bytes),
        "char" => Ok(PrimitiveType::Char),
        "string" => Ok(PrimitiveType::String),
        "unit" => Ok(PrimitiveType::Unit),
        "i8" => Ok(PrimitiveType::I8),
        "i16" => Ok(PrimitiveType::I16),
        "i32" => Ok(PrimitiveType::I32),
        "i64" => Ok(PrimitiveType::I64),
        "i128" => Ok(PrimitiveType::I128),
        "isize" => Ok(PrimitiveType::ISize),
        "u8" => Ok(PrimitiveType::U8),
        "u16" => Ok(PrimitiveType::U16),
        "u32" => Ok(PrimitiveType::U32),
        "u64" => Ok(PrimitiveType::U64),
        "u128" => Ok(PrimitiveType::U128),
        "usize" => Ok(PrimitiveType::USize),
        "f16" => Ok(PrimitiveType::F16),
        "f32" => Ok(PrimitiveType::F32),
        "f64" => Ok(PrimitiveType::F64),
        "int" => Ok(PrimitiveType::ISize),
        "float" => Err(TypeAnnotationError {
            message: "`float` is not a primitive Riot ML type".to_owned(),
            help: Some("use an explicitly sized floating-point type, for example `f32` or `f64`"),
        }),
        _ => Err(TypeAnnotationError {
            message: format!("unsupported type annotation `{text}`"),
            help: Some(
                "stage0 currently supports simple primitive annotations like `i64` and `bool`",
            ),
        }),
    }
}
