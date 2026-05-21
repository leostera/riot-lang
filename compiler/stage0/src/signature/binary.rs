use std::io::{Cursor, Read};

use miette::{IntoDiagnostic, bail};

use super::{
    ConstructorName, FieldName, MAGIC, ModuleName, Rsig, RsigDependency, RsigExport, RsigExternal,
    RsigFunction, RsigRecordField, RsigType, RsigTypeDecl, RsigTypeDeclKind, RsigTypeScheme,
    RsigVariantConstructor, TypeName, TypeParamName, TypeVarName, VERSION,
};

pub(super) fn encode_rsig(rsig: &Rsig) -> Vec<u8> {
    let mut bytes = Vec::new();
    bytes.extend_from_slice(MAGIC);
    put_u16(&mut bytes, VERSION);
    put_string(&mut bytes, rsig.module.as_str());
    put_u64(&mut bytes, rsig.module_fingerprint);
    put_u32(&mut bytes, rsig.dependencies.len() as u32);
    for dependency in &rsig.dependencies {
        put_string(&mut bytes, dependency.module.as_str());
        put_u64(&mut bytes, dependency.fingerprint);
    }
    put_u32(&mut bytes, rsig.types.len() as u32);
    for type_ in &rsig.types {
        put_string(&mut bytes, type_.name.as_str());
        put_u32(&mut bytes, type_.params.len() as u32);
        for param in &type_.params {
            put_string(&mut bytes, param.as_str());
        }
        match &type_.body {
            RsigTypeDeclKind::Abstract => {
                bytes.push(2);
            }
            RsigTypeDeclKind::Variant { constructors } => {
                bytes.push(0);
                put_u32(&mut bytes, constructors.len() as u32);
                for constructor in constructors {
                    put_string(&mut bytes, constructor.name.as_str());
                    put_types(&mut bytes, &constructor.payload);
                }
            }
            RsigTypeDeclKind::Record { fields } => {
                bytes.push(1);
                put_u32(&mut bytes, fields.len() as u32);
                for field in fields {
                    put_string(&mut bytes, field.name.as_str());
                    put_type(&mut bytes, &field.type_);
                }
            }
        }
        put_u64(&mut bytes, type_.fingerprint);
    }
    put_u32(&mut bytes, rsig.exports.len() as u32);
    for export in &rsig.exports {
        match export {
            RsigExport::Function(function) => {
                bytes.push(0);
                put_string(&mut bytes, &function.name);
                put_types(&mut bytes, &function.params);
                put_type(&mut bytes, &function.result);
                put_scheme(&mut bytes, &function.scheme);
                put_string(&mut bytes, &function.symbol);
                put_u64(&mut bytes, function.fingerprint);
            }
            RsigExport::External(external) => {
                bytes.push(1);
                put_string(&mut bytes, &external.name);
                put_types(&mut bytes, &external.params);
                put_type(&mut bytes, &external.result);
                put_scheme(&mut bytes, &external.scheme);
                put_string(&mut bytes, &external.abi);
                put_u64(&mut bytes, external.fingerprint);
            }
        }
    }
    bytes
}

pub(super) fn decode_rsig(bytes: &[u8]) -> miette::Result<Rsig> {
    let mut cursor = Cursor::new(bytes);
    let mut magic = [0_u8; 8];
    cursor.read_exact(&mut magic).into_diagnostic()?;
    if &magic != MAGIC {
        bail!("not a Riot signature artifact");
    }
    let version = get_u16(&mut cursor)?;
    if version != VERSION {
        bail!("unsupported rsig version {version}");
    }
    let module = ModuleName::new(get_string(&mut cursor)?);
    let module_fingerprint = get_u64(&mut cursor)?;
    let dependency_count = get_u32(&mut cursor)?;
    let mut dependencies = Vec::new();
    for _ in 0..dependency_count {
        dependencies.push(RsigDependency {
            module: ModuleName::new(get_string(&mut cursor)?),
            fingerprint: get_u64(&mut cursor)?,
        });
    }
    let type_count = get_u32(&mut cursor)?;
    let mut types = Vec::new();
    for _ in 0..type_count {
        let name = TypeName::new(get_string(&mut cursor)?);
        let param_count = get_u32(&mut cursor)?;
        let mut params = Vec::new();
        for _ in 0..param_count {
            params.push(TypeParamName::new(get_string(&mut cursor)?));
        }
        let mut tag = [0_u8; 1];
        cursor.read_exact(&mut tag).into_diagnostic()?;
        let body = match tag[0] {
            0 => {
                let constructor_count = get_u32(&mut cursor)?;
                let mut constructors = Vec::new();
                for _ in 0..constructor_count {
                    constructors.push(RsigVariantConstructor {
                        name: ConstructorName::new(get_string(&mut cursor)?),
                        payload: get_types(&mut cursor)?,
                    });
                }
                RsigTypeDeclKind::Variant { constructors }
            }
            1 => {
                let field_count = get_u32(&mut cursor)?;
                let mut fields = Vec::new();
                for _ in 0..field_count {
                    fields.push(RsigRecordField {
                        name: FieldName::new(get_string(&mut cursor)?),
                        type_: get_type(&mut cursor)?,
                    });
                }
                RsigTypeDeclKind::Record { fields }
            }
            2 => RsigTypeDeclKind::Abstract,
            other => bail!("unsupported rsig type declaration tag {other}"),
        };
        let fingerprint = get_u64(&mut cursor)?;
        types.push(RsigTypeDecl {
            name,
            params,
            body,
            fingerprint,
        });
    }
    let count = get_u32(&mut cursor)?;
    let mut exports = Vec::new();
    for _ in 0..count {
        let mut tag = [0_u8; 1];
        cursor.read_exact(&mut tag).into_diagnostic()?;
        let name = get_string(&mut cursor)?;
        let params = get_types(&mut cursor)?;
        let result = get_type(&mut cursor)?;
        let scheme = get_scheme(&mut cursor)?;
        let payload = get_string(&mut cursor)?;
        let fingerprint = get_u64(&mut cursor)?;
        match tag[0] {
            0 => exports.push(RsigExport::Function(RsigFunction {
                name,
                params,
                result,
                scheme,
                symbol: payload,
                fingerprint,
            })),
            1 => exports.push(RsigExport::External(RsigExternal {
                name,
                params,
                result,
                scheme,
                abi: payload,
                fingerprint,
            })),
            tag => bail!("unknown rsig export tag {tag}"),
        }
    }
    Ok(Rsig {
        module,
        dependencies,
        types,
        exports,
        module_fingerprint,
    })
}

fn put_types(bytes: &mut Vec<u8>, types: &[RsigType]) {
    put_u32(bytes, types.len() as u32);
    for type_ in types {
        put_type(bytes, type_);
    }
}

fn get_types(cursor: &mut Cursor<&[u8]>) -> miette::Result<Vec<RsigType>> {
    let count = get_u32(cursor)?;
    let mut types = Vec::new();
    for _ in 0..count {
        types.push(get_type(cursor)?);
    }
    Ok(types)
}

fn put_scheme(bytes: &mut Vec<u8>, scheme: &RsigTypeScheme) {
    put_u32(bytes, scheme.quantifiers.len() as u32);
    for quantifier in &scheme.quantifiers {
        put_string(bytes, quantifier.as_str());
    }
    put_type(bytes, &scheme.body);
}

fn get_scheme(cursor: &mut Cursor<&[u8]>) -> miette::Result<RsigTypeScheme> {
    let count = get_u32(cursor)?;
    let mut quantifiers = Vec::new();
    for _ in 0..count {
        quantifiers.push(TypeVarName::new(get_string(cursor)?));
    }
    let body = get_type(cursor)?;
    Ok(RsigTypeScheme { quantifiers, body })
}

fn put_type(bytes: &mut Vec<u8>, type_: &RsigType) {
    match type_ {
        RsigType::Bool => bytes.push(0),
        RsigType::Char => bytes.push(1),
        RsigType::F64 => bytes.push(2),
        RsigType::I64 => bytes.push(3),
        RsigType::ActorId(message) => {
            bytes.push(4);
            put_type(bytes, message);
        }
        RsigType::String => bytes.push(5),
        RsigType::Unit => bytes.push(6),
        RsigType::Var(name) => {
            bytes.push(7);
            put_string(bytes, name.as_str());
        }
        RsigType::Arrow { parameter, result } => {
            bytes.push(12);
            put_type(bytes, parameter);
            put_type(bytes, result);
        }
        RsigType::Tuple(items) => {
            bytes.push(8);
            put_types(bytes, items);
        }
        RsigType::List(item) => {
            bytes.push(9);
            put_type(bytes, item);
        }
        RsigType::Record(name) => {
            bytes.push(10);
            put_string(bytes, name.as_str());
        }
        RsigType::Variant(name) => {
            bytes.push(13);
            put_string(bytes, name.as_str());
        }
        RsigType::VariantApp { name, args } => {
            bytes.push(14);
            put_string(bytes, name.as_str());
            put_types(bytes, args);
        }
        RsigType::Unknown => bytes.push(11),
        RsigType::I32 => bytes.push(15),
    }
}

fn get_type(cursor: &mut Cursor<&[u8]>) -> miette::Result<RsigType> {
    let mut tag = [0_u8; 1];
    cursor.read_exact(&mut tag).into_diagnostic()?;
    Ok(match tag[0] {
        0 => RsigType::Bool,
        1 => RsigType::Char,
        2 => RsigType::F64,
        3 => RsigType::I64,
        4 => RsigType::ActorId(Box::new(get_type(cursor)?)),
        5 => RsigType::String,
        6 => RsigType::Unit,
        7 => RsigType::Var(TypeVarName::new(get_string(cursor)?)),
        8 => RsigType::Tuple(get_types(cursor)?),
        9 => RsigType::List(Box::new(get_type(cursor)?)),
        10 => RsigType::Record(TypeName::new(get_string(cursor)?)),
        11 => RsigType::Unknown,
        12 => RsigType::Arrow {
            parameter: Box::new(get_type(cursor)?),
            result: Box::new(get_type(cursor)?),
        },
        13 => RsigType::Variant(TypeName::new(get_string(cursor)?)),
        14 => RsigType::VariantApp {
            name: TypeName::new(get_string(cursor)?),
            args: get_types(cursor)?,
        },
        15 => RsigType::I32,
        tag => bail!("unknown type tag {tag}"),
    })
}

fn put_string(bytes: &mut Vec<u8>, value: &str) {
    put_u32(bytes, value.len() as u32);
    bytes.extend_from_slice(value.as_bytes());
}

fn get_string(cursor: &mut Cursor<&[u8]>) -> miette::Result<String> {
    let len = get_u32(cursor)? as usize;
    let mut bytes = vec![0_u8; len];
    cursor.read_exact(&mut bytes).into_diagnostic()?;
    String::from_utf8(bytes).into_diagnostic()
}

fn put_u16(bytes: &mut Vec<u8>, value: u16) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn get_u16(cursor: &mut Cursor<&[u8]>) -> miette::Result<u16> {
    let mut bytes = [0_u8; 2];
    cursor.read_exact(&mut bytes).into_diagnostic()?;
    Ok(u16::from_le_bytes(bytes))
}

fn put_u32(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn get_u32(cursor: &mut Cursor<&[u8]>) -> miette::Result<u32> {
    let mut bytes = [0_u8; 4];
    cursor.read_exact(&mut bytes).into_diagnostic()?;
    Ok(u32::from_le_bytes(bytes))
}

fn put_u64(bytes: &mut Vec<u8>, value: u64) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn get_u64(cursor: &mut Cursor<&[u8]>) -> miette::Result<u64> {
    let mut bytes = [0_u8; 8];
    cursor.read_exact(&mut bytes).into_diagnostic()?;
    Ok(u64::from_le_bytes(bytes))
}
