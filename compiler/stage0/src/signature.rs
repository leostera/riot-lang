use std::collections::BTreeMap;
use std::io::{Cursor, Read};

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr, bail};

const MAGIC: &[u8; 8] = b"RIOTRSIG";
const VERSION: u16 = 3;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Rsig {
    pub(crate) module: String,
    pub(crate) exports: Vec<RsigExport>,
    pub(crate) module_fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RsigExport {
    Function(RsigFunction),
    External(RsigExternal),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RsigFunction {
    pub(crate) name: String,
    pub(crate) params: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) scheme: RsigTypeScheme,
    pub(crate) symbol: String,
    pub(crate) fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RsigExternal {
    pub(crate) name: String,
    pub(crate) params: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) scheme: RsigTypeScheme,
    pub(crate) abi: String,
    pub(crate) fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RsigType {
    Bool,
    Char,
    F64,
    I64,
    ActorId(Box<RsigType>),
    String,
    Unit,
    Var(String),
    Arrow {
        parameter: Box<RsigType>,
        result: Box<RsigType>,
    },
    Tuple(Vec<RsigType>),
    List(Box<RsigType>),
    Record(String),
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RsigTypeScheme {
    pub(crate) quantifiers: Vec<String>,
    pub(crate) body: RsigType,
}

impl RsigTypeScheme {
    pub(crate) fn from_signature(params: &[RsigType], result: &RsigType) -> Self {
        let body = source_function_type(params.to_vec(), result.clone());
        let mut quantifiers = Vec::new();
        collect_type_vars(&body, &mut quantifiers);
        Self { quantifiers, body }
    }

    pub(crate) fn monomorphic(body: RsigType) -> Self {
        Self {
            quantifiers: Vec::new(),
            body,
        }
    }

    pub(crate) fn canonical(&self) -> String {
        if self.quantifiers.is_empty() {
            self.body.canonical()
        } else {
            format!(
                "forall {}. {}",
                self.quantifiers.join(" "),
                self.body.canonical()
            )
        }
    }
}

fn source_function_type(params: Vec<RsigType>, result: RsigType) -> RsigType {
    if params.is_empty() {
        RsigType::Arrow {
            parameter: Box::new(RsigType::Unit),
            result: Box::new(result),
        }
    } else {
        params
            .into_iter()
            .rev()
            .fold(result, |result, parameter| RsigType::Arrow {
                parameter: Box::new(parameter),
                result: Box::new(result),
            })
    }
}

fn collect_type_vars(type_: &RsigType, vars: &mut Vec<String>) {
    match type_ {
        RsigType::ActorId(message) | RsigType::List(message) => collect_type_vars(message, vars),
        RsigType::Arrow { parameter, result } => {
            collect_type_vars(parameter, vars);
            collect_type_vars(result, vars);
        }
        RsigType::Tuple(items) => {
            for item in items {
                collect_type_vars(item, vars);
            }
        }
        RsigType::Var(name) if !vars.contains(name) => vars.push(name.clone()),
        RsigType::Var(_)
        | RsigType::Bool
        | RsigType::Char
        | RsigType::F64
        | RsigType::I64
        | RsigType::Record(_)
        | RsigType::String
        | RsigType::Unit
        | RsigType::Unknown => {}
    }
}

impl Rsig {
    pub(crate) fn new(module: String, mut exports: Vec<RsigExport>) -> Self {
        for export in &mut exports {
            export.canonicalize_type_vars();
        }
        exports.sort_by(|lhs, rhs| lhs.name().cmp(rhs.name()).then(lhs.kind().cmp(rhs.kind())));
        for export in &mut exports {
            export.set_fingerprint(stable_hash(&export.canonical_without_fingerprint()));
        }
        let module_fingerprint = stable_hash(
            &exports
                .iter()
                .map(RsigExport::canonical_with_fingerprint)
                .collect::<Vec<_>>()
                .join("\n"),
        );
        Self {
            module,
            exports,
            module_fingerprint,
        }
    }

    pub(crate) fn canonical_text(&self) -> String {
        let mut text = String::new();
        text.push_str(&format!("module {}\n", self.module));
        text.push_str(&format!("fingerprint {:016x}\n", self.module_fingerprint));
        for export in &self.exports {
            text.push_str(&export.canonical_with_fingerprint());
            text.push('\n');
        }
        text
    }

    pub(crate) fn find(&self, name: &str) -> Option<&RsigExport> {
        self.exports.iter().find(|export| export.name() == name)
    }
}

impl RsigExport {
    pub(crate) fn name(&self) -> &str {
        match self {
            RsigExport::Function(function) => &function.name,
            RsigExport::External(external) => &external.name,
        }
    }

    fn kind(&self) -> &'static str {
        match self {
            RsigExport::Function(_) => "fn",
            RsigExport::External(_) => "external",
        }
    }

    fn canonical_without_fingerprint(&self) -> String {
        match self {
            RsigExport::Function(function) => format!(
                "fn {}({}) -> {} : {} = {}",
                function.name,
                render_types(&function.params),
                function.result.canonical(),
                function.scheme.canonical(),
                function.symbol
            ),
            RsigExport::External(external) => format!(
                "external {}({}) -> {} : {} = {}",
                external.name,
                render_types(&external.params),
                external.result.canonical(),
                external.scheme.canonical(),
                external.abi
            ),
        }
    }

    fn canonical_with_fingerprint(&self) -> String {
        match self {
            RsigExport::Function(function) => {
                format!(
                    "{:016x} {}",
                    function.fingerprint,
                    self.canonical_without_fingerprint()
                )
            }
            RsigExport::External(external) => {
                format!(
                    "{:016x} {}",
                    external.fingerprint,
                    self.canonical_without_fingerprint()
                )
            }
        }
    }

    fn set_fingerprint(&mut self, fingerprint: u64) {
        match self {
            RsigExport::Function(function) => function.fingerprint = fingerprint,
            RsigExport::External(external) => external.fingerprint = fingerprint,
        }
    }

    fn canonicalize_type_vars(&mut self) {
        let mut vars = TypeVarCanonicalizer::default();
        match self {
            RsigExport::Function(function) => {
                for param in &mut function.params {
                    vars.canonicalize(param);
                }
                vars.canonicalize(&mut function.result);
                vars.canonicalize_scheme(&mut function.scheme);
            }
            RsigExport::External(external) => {
                for param in &mut external.params {
                    vars.canonicalize(param);
                }
                vars.canonicalize(&mut external.result);
                vars.canonicalize_scheme(&mut external.scheme);
            }
        }
    }
}

#[derive(Default)]
struct TypeVarCanonicalizer {
    names: BTreeMap<String, String>,
}

impl TypeVarCanonicalizer {
    fn canonicalize_scheme(&mut self, scheme: &mut RsigTypeScheme) {
        self.canonicalize(&mut scheme.body);
        let mut quantifiers = Vec::new();
        collect_type_vars(&scheme.body, &mut quantifiers);
        scheme.quantifiers = quantifiers;
    }

    fn canonicalize(&mut self, type_: &mut RsigType) {
        match type_ {
            RsigType::ActorId(message) | RsigType::List(message) => self.canonicalize(message),
            RsigType::Arrow { parameter, result } => {
                self.canonicalize(parameter);
                self.canonicalize(result);
            }
            RsigType::Tuple(items) => {
                for item in items {
                    self.canonicalize(item);
                }
            }
            RsigType::Var(name) => {
                let next = canonical_type_var_name(self.names.len());
                let canonical = self.names.entry(name.clone()).or_insert(next);
                *name = canonical.clone();
            }
            RsigType::Bool
            | RsigType::Char
            | RsigType::F64
            | RsigType::I64
            | RsigType::Record(_)
            | RsigType::String
            | RsigType::Unit
            | RsigType::Unknown => {}
        }
    }
}

fn canonical_type_var_name(index: usize) -> String {
    if index < 26 {
        let name = char::from(b'a' + index as u8);
        format!("'{name}")
    } else {
        format!("'t{}", index - 26)
    }
}

impl RsigType {
    pub(crate) fn canonical(&self) -> String {
        match self {
            RsigType::Bool => "bool".to_owned(),
            RsigType::Char => "char".to_owned(),
            RsigType::F64 => "f64".to_owned(),
            RsigType::I64 => "i64".to_owned(),
            RsigType::ActorId(message) => format!("actor_id<{}>", message.canonical()),
            RsigType::String => "string".to_owned(),
            RsigType::Unit => "unit".to_owned(),
            RsigType::Var(name) => name.clone(),
            RsigType::Arrow { parameter, result } => {
                format!("({} -> {})", parameter.canonical(), result.canonical())
            }
            RsigType::Tuple(items) => format!("({})", render_types(items)),
            RsigType::List(item) => format!("{} list", item.canonical()),
            RsigType::Record(name) => name.clone(),
            RsigType::Unknown => "_".to_owned(),
        }
    }
}

fn render_types(types: &[RsigType]) -> String {
    types
        .iter()
        .map(RsigType::canonical)
        .collect::<Vec<_>>()
        .join(", ")
}

pub(crate) fn write_rsig(path: &Utf8Path, rsig: &Rsig) -> miette::Result<()> {
    std::fs::write(path.as_std_path(), encode_rsig(rsig))
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to write {}", path))
}

pub(crate) fn read_rsig(path: &Utf8Path) -> miette::Result<Rsig> {
    let bytes = std::fs::read(path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read {}", path))?;
    decode_rsig(&bytes).wrap_err_with(|| format!("failed to decode {}", path))
}

pub(crate) fn resolve_rsig(
    module: &str,
    source_dir: Option<&Utf8Path>,
    sig_dirs: &[Utf8PathBuf],
) -> miette::Result<(Utf8PathBuf, Rsig)> {
    let filename = format!("{module}.rsig");
    let mut candidates = Vec::new();
    if let Some(source_dir) = source_dir {
        candidates.push(source_dir.join(&filename));
    }
    candidates.extend(sig_dirs.iter().map(|dir| dir.join(&filename)));

    for candidate in &candidates {
        if candidate.exists() {
            let rsig = read_rsig(candidate)?;
            if rsig.module != module {
                bail!(
                    "signature {} declares module {}, but `use {}` requested {}",
                    candidate,
                    rsig.module,
                    module,
                    module
                );
            }
            return Ok((candidate.clone(), rsig));
        }
    }

    bail!(
        "missing signature for module {module}\nsearched:\n{}",
        candidates
            .iter()
            .map(|path| format!("  - {path}"))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

pub(crate) fn parse_type_signature(text: &str) -> (Vec<RsigType>, RsigType) {
    let mut parts = split_top_level_arrows(text);
    if parts.len() < 2 {
        return (Vec::new(), parse_type(text));
    }
    let result = parse_type(parts.pop().unwrap_or("_"));
    let params = parts.into_iter().map(parse_type).collect();
    (params, result)
}

pub(crate) fn parse_type(text: &str) -> RsigType {
    let text = text.trim();
    if let Some(inner) = strip_wrapping_parens(text)
        && split_top_level_arrows(inner).len() > 1
    {
        return parse_type(inner);
    }
    let mut arrow_parts = split_top_level_arrows(text);
    if arrow_parts.len() > 1 {
        let result = parse_type(arrow_parts.pop().unwrap_or("_"));
        return arrow_parts
            .into_iter()
            .rev()
            .fold(result, |result, parameter| RsigType::Arrow {
                parameter: Box::new(parse_type(parameter)),
                result: Box::new(result),
            });
    }
    if let Some(item) = text.strip_suffix(" list") {
        return RsigType::List(Box::new(parse_type(item)));
    }
    match text {
        "bool" => RsigType::Bool,
        "char" => RsigType::Char,
        "f64" | "float" => RsigType::F64,
        "i64" | "int" => RsigType::I64,
        "string" => RsigType::String,
        "unit" => RsigType::Unit,
        "_" | "" => RsigType::Unknown,
        _ if text.starts_with('\'') => RsigType::Var(text.to_owned()),
        _ if text.starts_with("actor_id<") && text.ends_with('>') => {
            let inner = &text[9..text.len() - 1];
            RsigType::ActorId(Box::new(parse_type(inner)))
        }
        _ if text.starts_with('(') && text.ends_with(')') && text.contains(',') => {
            let inner = &text[1..text.len() - 1];
            RsigType::Tuple(
                split_top_level_commas(inner)
                    .into_iter()
                    .map(parse_type)
                    .collect(),
            )
        }
        _ => RsigType::Record(text.to_owned()),
    }
}

fn strip_wrapping_parens(text: &str) -> Option<&str> {
    let inner = text.strip_prefix('(')?.strip_suffix(')')?;
    let mut depth = 0_i32;
    for (index, ch) in text.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 && index + ch.len_utf8() < text.len() {
                    return None;
                }
            }
            _ => {}
        }
    }
    Some(inner)
}

pub(crate) fn stable_hash(text: &str) -> u64 {
    const OFFSET: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x100000001b3;
    let mut hash = OFFSET;
    for byte in text.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(PRIME);
    }
    hash
}

fn split_top_level_arrows(text: &str) -> Vec<&str> {
    let bytes = text.as_bytes();
    let mut depth = 0_i32;
    let mut start = 0;
    let mut parts = Vec::new();
    let mut index = 0;
    while index < bytes.len() {
        match bytes[index] {
            b'(' | b'<' => depth += 1,
            b')' => depth -= 1,
            b'>' if index == 0 || bytes[index - 1] != b'-' => depth -= 1,
            b'-' if depth == 0 && bytes.get(index + 1) == Some(&b'>') => {
                parts.push(text[start..index].trim());
                index += 2;
                start = index;
                continue;
            }
            _ => {}
        }
        index += 1;
    }
    parts.push(text[start..].trim());
    parts
}

fn split_top_level_commas(text: &str) -> Vec<&str> {
    let mut depth = 0_i32;
    let mut start = 0;
    let mut parts = Vec::new();
    for (index, ch) in text.char_indices() {
        match ch {
            '(' | '<' => depth += 1,
            ')' | '>' => depth -= 1,
            ',' if depth == 0 => {
                parts.push(text[start..index].trim());
                start = index + ch.len_utf8();
            }
            _ => {}
        }
    }
    parts.push(text[start..].trim());
    parts
}

fn encode_rsig(rsig: &Rsig) -> Vec<u8> {
    let mut bytes = Vec::new();
    bytes.extend_from_slice(MAGIC);
    put_u16(&mut bytes, VERSION);
    put_string(&mut bytes, &rsig.module);
    put_u64(&mut bytes, rsig.module_fingerprint);
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

fn decode_rsig(bytes: &[u8]) -> miette::Result<Rsig> {
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
    let module = get_string(&mut cursor)?;
    let module_fingerprint = get_u64(&mut cursor)?;
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
        put_string(bytes, quantifier);
    }
    put_type(bytes, &scheme.body);
}

fn get_scheme(cursor: &mut Cursor<&[u8]>) -> miette::Result<RsigTypeScheme> {
    let count = get_u32(cursor)?;
    let mut quantifiers = Vec::new();
    for _ in 0..count {
        quantifiers.push(get_string(cursor)?);
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
            put_string(bytes, name);
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
            put_string(bytes, name);
        }
        RsigType::Unknown => bytes.push(11),
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
        7 => RsigType::Var(get_string(cursor)?),
        8 => RsigType::Tuple(get_types(cursor)?),
        9 => RsigType::List(Box::new(get_type(cursor)?)),
        10 => RsigType::Record(get_string(cursor)?),
        11 => RsigType::Unknown,
        12 => RsigType::Arrow {
            parameter: Box::new(get_type(cursor)?),
            result: Box::new(get_type(cursor)?),
        },
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

pub(crate) type ImportedSignatures = BTreeMap<String, Rsig>;

#[cfg(test)]
mod tests {
    use super::{
        Rsig, RsigExport, RsigFunction, RsigType, RsigTypeScheme, decode_rsig, encode_rsig,
        parse_type_signature,
    };

    #[test]
    fn parses_parenthesized_arrow_parameter_types() {
        let (params, result) = parse_type_signature("(i64 -> i64) -> i64 -> i64");

        assert_eq!(
            params,
            vec![
                RsigType::Arrow {
                    parameter: Box::new(RsigType::I64),
                    result: Box::new(RsigType::I64),
                },
                RsigType::I64,
            ]
        );
        assert_eq!(result, RsigType::I64);
    }

    #[test]
    fn binary_rsig_roundtrips_arrow_types() {
        let rsig = Rsig::new(
            "Apply".to_owned(),
            vec![RsigExport::Function(RsigFunction {
                name: "apply_i64".to_owned(),
                params: vec![
                    RsigType::Arrow {
                        parameter: Box::new(RsigType::I64),
                        result: Box::new(RsigType::I64),
                    },
                    RsigType::I64,
                ],
                result: RsigType::I64,
                scheme: RsigTypeScheme::from_signature(
                    &[
                        RsigType::Arrow {
                            parameter: Box::new(RsigType::I64),
                            result: Box::new(RsigType::I64),
                        },
                        RsigType::I64,
                    ],
                    &RsigType::I64,
                ),
                symbol: "riot_mod_Apply_apply_i64".to_owned(),
                fingerprint: 0,
            })],
        );

        let decoded = decode_rsig(&encode_rsig(&rsig)).unwrap();

        assert_eq!(decoded, rsig);
        assert!(decoded.canonical_text().contains("(i64 -> i64)"));
    }

    #[test]
    fn rsig_canonicalizes_type_variables_before_fingerprinting() {
        let first = Rsig::new(
            "Id".to_owned(),
            vec![RsigExport::Function(RsigFunction {
                name: "id".to_owned(),
                params: vec![RsigType::Var("'t17".to_owned())],
                result: RsigType::Var("'t17".to_owned()),
                scheme: RsigTypeScheme::from_signature(
                    &[RsigType::Var("'t17".to_owned())],
                    &RsigType::Var("'t17".to_owned()),
                ),
                symbol: "riot_mod_Id_id".to_owned(),
                fingerprint: 0,
            })],
        );
        let second = Rsig::new(
            "Id".to_owned(),
            vec![RsigExport::Function(RsigFunction {
                name: "id".to_owned(),
                params: vec![RsigType::Var("'source_name".to_owned())],
                result: RsigType::Var("'source_name".to_owned()),
                scheme: RsigTypeScheme::from_signature(
                    &[RsigType::Var("'source_name".to_owned())],
                    &RsigType::Var("'source_name".to_owned()),
                ),
                symbol: "riot_mod_Id_id".to_owned(),
                fingerprint: 0,
            })],
        );

        assert_eq!(first, second);
        assert!(first.canonical_text().contains("fn id('a) -> 'a"));
        assert_eq!(decode_rsig(&encode_rsig(&first)).unwrap(), first);
    }
}
