//! Checked module interface summaries.
//!
//! Interfaces describe exported values, types, constructors, fields, and modules
//! independently from source syntax so backends can emit API information.

use crate::parser::{ast, ident::Ident};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct InterfaceArtifact {
    version: u32,
    values: Vec<ArtifactValue>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ArtifactValue {
    name: String,
    scheme: ArtifactScheme,
}

#[derive(Debug, Serialize, Deserialize)]
struct ArtifactScheme {
    quantifier: Vec<usize>,
    body: ArtifactType,
}

#[derive(Debug, Serialize, Deserialize)]
enum ArtifactType {
    Var(usize),
    Generic(usize),
    Tuple(Vec<ArtifactType>),
    Arrow(Box<ArtifactType>, Box<ArtifactType>),
    Apply(String, Vec<ArtifactType>),
}

/// Serialize the checked value signatures as a versioned binary artifact.
pub fn encode_interface(interface: &ModuleInterface) -> Result<Vec<u8>, String> {
    let artifact = InterfaceArtifact {
        version: 1,
        values: interface.values.iter().filter_map(|value| {
            value.name.as_name().map(|name| ArtifactValue {
                name: name.to_string(),
                scheme: ArtifactScheme {
                    quantifier: value.scheme.quantifier.iter().map(|id| id.index()).collect(),
                    body: artifact_type(&value.scheme.body),
                },
            })
        }).collect(),
    };
    bincode::serialize(&artifact).map_err(|error| error.to_string())
}

/// Deserialize value signatures from a binary interface artifact.
pub fn decode_interface(bytes: &[u8]) -> Result<Vec<(String, Scheme)>, String> {
    let artifact: InterfaceArtifact = bincode::deserialize(bytes).map_err(|error| error.to_string())?;
    if artifact.version != 1 { return Err("unsupported interface version".into()); }
    artifact.values.into_iter().map(|value| {
        Ok((value.name, Scheme {
            quantifier: value.scheme.quantifier.into_iter().map(TypeVarId::from_index).collect(),
            body: tir_type(value.scheme.body),
        }))
    }).collect()
}

fn artifact_type(ty: &Type) -> ArtifactType {
    match ty {
        Type::Var(id) => ArtifactType::Var(id.index()),
        Type::Generic(id) => ArtifactType::Generic(id.index()),
        Type::Tuple(items) => ArtifactType::Tuple(items.iter().map(artifact_type).collect()),
        Type::Arrow { parameter, result } => ArtifactType::Arrow(Box::new(artifact_type(parameter)), Box::new(artifact_type(result))),
        Type::Apply { ident, arguments } => ArtifactType::Apply(ident_path(ident), arguments.iter().map(artifact_type).collect()),
    }
}

fn ident_path(ident: &Ident) -> String {
    match ident {
        Ident::Name { name, .. } => name.clone(),
        Ident::Qualified { name, rest, .. } => format!("{}.{}", name, ident_path(rest)),
    }
}

fn ident_from_path(path: &str) -> Ident {
    let mut parts = path.split('.').rev();
    let mut ident = Ident::from_string(parts.next().unwrap_or(path));
    for part in parts {
        ident = Ident::Qualified {
            name: part.to_string(),
            rest: Box::new(ident),
            span: crate::parser::lexer::Span::new(0, 0),
        };
    }
    ident
}

fn tir_type(ty: ArtifactType) -> Type {
    match ty {
        ArtifactType::Var(id) => Type::Var(TypeVarId::from_index(id)),
        ArtifactType::Generic(id) => Type::Generic(TypeVarId::from_index(id)),
        ArtifactType::Tuple(items) => Type::Tuple(items.into_iter().map(tir_type).collect()),
        ArtifactType::Arrow(parameter, result) => Type::Arrow { parameter: Box::new(tir_type(*parameter)), result: Box::new(tir_type(*result)) },
        ArtifactType::Apply(name, arguments) => Type::Apply { ident: ident_from_path(&name), arguments: arguments.into_iter().map(tir_type).collect() },
    }
}

use super::{scheme::Scheme, tir::{Type, TypeVarId}};

/// Encode a type scheme in the stable, dependency-free `.tmi` format.
pub fn encode_scheme(scheme: &Scheme) -> String {
    let quantifiers = scheme
        .quantifier
        .iter()
        .map(|id| id.index().to_string())
        .collect::<Vec<_>>()
        .join(",");
    format!("{}|{}", quantifiers, encode_type(&scheme.body))
}

fn encode_type(ty: &Type) -> String {
    match ty {
        Type::Var(id) => format!("v{}", id.index()),
        Type::Generic(id) => format!("g{}", id.index()),
        Type::Tuple(items) => format!("t[{}]", items.iter().map(encode_type).collect::<Vec<_>>().join(",")),
        Type::Arrow { parameter, result } => {
            format!("f[{},{}]", encode_type(parameter), encode_type(result))
        }
        Type::Apply { ident, arguments } => {
            let name = ident.as_name().unwrap_or("_");
            format!("a{}:{}[{}]", name.len(), name, arguments.iter().map(encode_type).collect::<Vec<_>>().join(","))
        }
    }
}

/// Decode a scheme produced by [`encode_scheme`].
pub fn decode_scheme(encoded: &str) -> Result<Scheme, String> {
    let (quantifiers, body) = encoded.split_once('|').ok_or("missing scheme separator")?;
    let quantifier = if quantifiers.is_empty() {
        Vec::new()
    } else {
        quantifiers
            .split(',')
            .map(|value| value.parse::<usize>().map(TypeVarId::from_index).map_err(|_| "invalid quantifier".to_string()))
            .collect::<Result<Vec<_>, _>>()?
    };
    Ok(Scheme { quantifier, body: decode_type(body)? })
}

fn decode_type(encoded: &str) -> Result<Type, String> {
    let tag = encoded.chars().next().ok_or("empty type")?;
    match tag {
        'v' | 'g' => {
            let id = encoded[1..].parse::<usize>().map_err(|_| "invalid type variable".to_string())?;
            Ok(if tag == 'v' { Type::Var(TypeVarId::from_index(id)) } else { Type::Generic(TypeVarId::from_index(id)) })
        }
        'a' => {
            let colon = encoded.find(':').ok_or("invalid application")?;
            let len = encoded[1..colon].parse::<usize>().map_err(|_| "invalid name length".to_string())?;
            let name_start = colon + 1;
            let name_end = name_start + len;
            let name = encoded.get(name_start..name_end).ok_or("truncated name")?;
            let args = decode_list(encoded.get(name_end..).ok_or("missing arguments")?)?;
            Ok(Type::Apply { ident: Ident::from_string(name), arguments: args })
        }
        't' | 'f' => {
            let items = decode_list(encoded.get(1..).ok_or("missing type list")?)?;
            if tag == 't' { Ok(Type::Tuple(items)) }
            else if items.len() == 2 { Ok(Type::Arrow { parameter: Box::new(items[0].clone()), result: Box::new(items[1].clone()) }) }
            else { Err("function type must have two elements".into()) }
        }
        _ => Err("unknown type tag".into()),
    }
}

fn decode_list(encoded: &str) -> Result<Vec<Type>, String> {
    if !encoded.starts_with('[') || !encoded.ends_with(']') { return Err("invalid type list".into()); }
    let inner = &encoded[1..encoded.len() - 1];
    if inner.is_empty() { return Ok(Vec::new()); }
    let mut parts = Vec::new();
    let mut start = 0;
    let mut depth = 0;
    for (index, byte) in inner.bytes().enumerate() {
        match byte {
            b'[' => depth += 1,
            b']' => depth -= 1,
            b',' if depth == 0 => {
                parts.push(&inner[start..index]);
                start = index + 1;
            }
            _ => {}
        }
    }
    parts.push(&inner[start..]);
    parts.into_iter().map(decode_type).collect()
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleInterface {
    pub values: Vec<ValueDescription>,
    pub constructors: Vec<ConstructorDescription>,
    pub record_fields: Vec<RecordFieldDescription>,
    pub types: Vec<TypeDescription>,
    pub modules: Vec<ModuleDescription>,
}

impl ModuleInterface {
    pub fn empty() -> Self {
        Self {
            values: Vec::new(),
            constructors: Vec::new(),
            record_fields: Vec::new(),
            types: Vec::new(),
            modules: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDescription {
    pub name: Ident,
    pub scheme: Scheme,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDescription {
    pub name: Ident,
    pub scheme: Scheme,
    pub owner: Ident,
    pub tag: u32,
    pub result: Type,
    pub arguments: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordFieldDescription {
    pub name: Ident,
    pub owner: Type,
    pub field: Type,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDescription {
    pub name: Ident,
    pub params: Vec<ast::TypeVar>,
    pub body: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDescription {
    pub name: Ident,
    pub interface: Box<ModuleInterface>,
}
