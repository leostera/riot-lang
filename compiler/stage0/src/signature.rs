use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::io::{Cursor, Read};

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr, bail};

const MAGIC: &[u8; 8] = b"RIOTRSIG";
const VERSION: u16 = 9;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Rsig {
    pub(crate) module: ModuleName,
    pub(crate) dependencies: Vec<RsigDependency>,
    pub(crate) types: Vec<RsigTypeDecl>,
    pub(crate) exports: Vec<RsigExport>,
    pub(crate) module_fingerprint: u64,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ModuleName(String);

impl ModuleName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for ModuleName {
    fn from(value: String) -> Self {
        ModuleName::new(value)
    }
}

impl From<&str> for ModuleName {
    fn from(value: &str) -> Self {
        ModuleName::new(value)
    }
}

impl AsRef<str> for ModuleName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for ModuleName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

impl fmt::Debug for ModuleName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(formatter)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TypeName(String);

impl TypeName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for TypeName {
    fn from(value: String) -> Self {
        TypeName::new(value)
    }
}

impl From<&str> for TypeName {
    fn from(value: &str) -> Self {
        TypeName::new(value)
    }
}

impl AsRef<str> for TypeName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ConstructorName(String);

impl ConstructorName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for ConstructorName {
    fn from(value: String) -> Self {
        ConstructorName::new(value)
    }
}

impl From<&str> for ConstructorName {
    fn from(value: &str) -> Self {
        ConstructorName::new(value)
    }
}

impl AsRef<str> for ConstructorName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for ConstructorName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct FieldName(String);

impl FieldName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for FieldName {
    fn from(value: String) -> Self {
        FieldName::new(value)
    }
}

impl From<&str> for FieldName {
    fn from(value: &str) -> Self {
        FieldName::new(value)
    }
}

impl AsRef<str> for FieldName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for FieldName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RsigDependency {
    pub(crate) module: ModuleName,
    pub(crate) fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RsigTypeDecl {
    pub(crate) name: TypeName,
    pub(crate) params: Vec<TypeParamName>,
    pub(crate) body: RsigTypeDeclKind,
    pub(crate) fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RsigTypeDeclKind {
    Abstract,
    Variant {
        constructors: Vec<RsigVariantConstructor>,
    },
    Record {
        fields: Vec<RsigRecordField>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RsigVariantConstructor {
    pub(crate) name: ConstructorName,
    pub(crate) payload: Vec<RsigType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeParamName(String);

impl TypeParamName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for TypeParamName {
    fn from(value: String) -> Self {
        TypeParamName::new(value)
    }
}

impl From<&str> for TypeParamName {
    fn from(value: &str) -> Self {
        TypeParamName::new(value)
    }
}

impl AsRef<str> for TypeParamName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for TypeParamName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RsigRecordField {
    pub(crate) name: FieldName,
    pub(crate) type_: RsigType,
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
    I32,
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
    Record(TypeName),
    Variant(TypeName),
    VariantApp {
        name: TypeName,
        args: Vec<RsigType>,
    },
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
        RsigType::VariantApp { args, .. } => {
            for arg in args {
                collect_type_vars(arg, vars);
            }
        }
        RsigType::Var(name) if !vars.contains(name) => vars.push(name.clone()),
        RsigType::Var(_)
        | RsigType::Bool
        | RsigType::Char
        | RsigType::F64
        | RsigType::I32
        | RsigType::I64
        | RsigType::Record(_)
        | RsigType::String
        | RsigType::Unit
        | RsigType::Variant(_)
        | RsigType::Unknown => {}
    }
}

impl Rsig {
    pub(crate) fn with_dependencies(
        module: impl Into<ModuleName>,
        mut dependencies: Vec<RsigDependency>,
        mut types: Vec<RsigTypeDecl>,
        mut exports: Vec<RsigExport>,
    ) -> Self {
        let module = module.into();
        dependencies.sort_by(|lhs, rhs| lhs.module.cmp(&rhs.module));
        types.sort_by(|lhs, rhs| lhs.name.as_str().cmp(rhs.name.as_str()));
        for type_ in &mut types {
            type_.fingerprint = stable_hash(&type_.canonical_without_fingerprint());
        }
        for export in &mut exports {
            export.canonicalize_type_vars();
        }
        exports.sort_by(|lhs, rhs| lhs.name().cmp(rhs.name()).then(lhs.kind().cmp(rhs.kind())));
        for export in &mut exports {
            export.set_fingerprint(stable_hash(&export.canonical_without_fingerprint()));
        }
        let dependency_text = dependencies
            .iter()
            .map(RsigDependency::canonical)
            .collect::<Vec<_>>()
            .join("\n");
        let export_text = exports
            .iter()
            .map(RsigExport::canonical_with_fingerprint)
            .collect::<Vec<_>>()
            .join("\n");
        let type_text = types
            .iter()
            .map(RsigTypeDecl::canonical_with_fingerprint)
            .collect::<Vec<_>>()
            .join("\n");
        let module_fingerprint = stable_hash(
            &[
                dependency_text.as_str(),
                type_text.as_str(),
                export_text.as_str(),
            ]
            .into_iter()
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>()
            .join("\n"),
        );
        Self {
            module,
            dependencies,
            types,
            exports,
            module_fingerprint,
        }
    }

    pub(crate) fn canonical_text(&self) -> String {
        let mut text = String::new();
        text.push_str(&format!("module {}\n", self.module));
        text.push_str(&format!("fingerprint {:016x}\n", self.module_fingerprint));
        for dependency in &self.dependencies {
            text.push_str(&dependency.canonical());
            text.push('\n');
        }
        for type_ in &self.types {
            text.push_str(&type_.canonical_with_fingerprint());
            text.push('\n');
        }
        for export in &self.exports {
            text.push_str(&export.canonical_with_fingerprint());
            text.push('\n');
        }
        text
    }

    pub(crate) fn find(&self, name: &str) -> Option<&RsigExport> {
        self.exports.iter().find(|export| export.name() == name)
    }

    pub(crate) fn find_constructor(&self, name: &str) -> Option<&RsigTypeDecl> {
        self.types.iter().find(|type_| {
            let RsigTypeDeclKind::Variant { constructors } = &type_.body else {
                return false;
            };
            constructors
                .iter()
                .any(|constructor| constructor.name.as_str() == name)
        })
    }
}

impl RsigDependency {
    fn canonical(&self) -> String {
        format!("depends {} {:016x}", self.module, self.fingerprint)
    }
}

impl RsigTypeDecl {
    fn canonical_header(&self) -> String {
        if self.params.is_empty() {
            format!("type {}", self.name)
        } else {
            let params = self
                .params
                .iter()
                .map(TypeParamName::as_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("type {}<{}>", self.name, params)
        }
    }

    fn canonical_without_fingerprint(&self) -> String {
        match &self.body {
            RsigTypeDeclKind::Abstract => self.canonical_header(),
            RsigTypeDeclKind::Variant { constructors } => {
                let constructors = constructors
                    .iter()
                    .map(RsigVariantConstructor::canonical)
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!("{} = {}", self.canonical_header(), constructors)
            }
            RsigTypeDeclKind::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|field| format!("{}: {}", field.name, field.type_.canonical()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} = {{ {} }}", self.canonical_header(), fields)
            }
        }
    }

    fn canonical_with_fingerprint(&self) -> String {
        format!(
            "{:016x} {}",
            self.fingerprint,
            self.canonical_without_fingerprint()
        )
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

impl RsigVariantConstructor {
    fn canonical(&self) -> String {
        if self.payload.is_empty() {
            self.name.to_string()
        } else {
            format!("{}({})", self.name, render_types(&self.payload))
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
            RsigType::VariantApp { args, .. } => {
                for arg in args {
                    self.canonicalize(arg);
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
            | RsigType::I32
            | RsigType::I64
            | RsigType::Record(_)
            | RsigType::Variant(_)
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
            RsigType::I32 => "i32".to_owned(),
            RsigType::I64 => "i64".to_owned(),
            RsigType::ActorId(message) => format!("actor_id<{}>", message.canonical()),
            RsigType::String => "String".to_owned(),
            RsigType::Unit => "unit".to_owned(),
            RsigType::Var(name) => name.clone(),
            RsigType::Arrow { parameter, result } => {
                format!("({} -> {})", parameter.canonical(), result.canonical())
            }
            RsigType::Tuple(items) => format!("({})", render_types(items)),
            RsigType::List(item) => format!("List<{}>", item.canonical()),
            RsigType::Record(name) => name.to_string(),
            RsigType::Variant(name) => name.to_string(),
            RsigType::VariantApp { name, args } => {
                format!("{}<{}>", name, render_types(args))
            }
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
            if rsig.module.as_str() != module {
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

pub(crate) fn parse_type_signature_with_variants(
    text: &str,
    variants: &BTreeSet<TypeName>,
) -> (Vec<RsigType>, RsigType) {
    let mut parts = split_top_level_arrows(text);
    if parts.len() < 2 {
        return (Vec::new(), parse_type_with_variants(text, variants));
    }
    let result = parse_type_with_variants(parts.pop().unwrap_or("_"), variants);
    let params = parts
        .into_iter()
        .map(|part| parse_type_with_variants(part, variants))
        .collect();
    (params, result)
}

pub(crate) fn parse_type(text: &str) -> RsigType {
    let text = text.trim();
    if text == "()" {
        return RsigType::Unit;
    }
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
    if let Some((name, args)) = parse_named_type_app(text)
        && name == "List"
    {
        let args = split_top_level_commas(args);
        if let [item] = args.as_slice() {
            return RsigType::List(Box::new(parse_type(item)));
        }
    }
    match text {
        "bool" => RsigType::Bool,
        "char" => RsigType::Char,
        "f64" | "float" => RsigType::F64,
        "i32" => RsigType::I32,
        "i64" | "int" => RsigType::I64,
        "String" => RsigType::String,
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
        _ => RsigType::Record(TypeName::new(text)),
    }
}

pub(crate) fn parse_type_with_variants(text: &str, variants: &BTreeSet<TypeName>) -> RsigType {
    let text = text.trim();
    if let Some((name, args)) = parse_named_type_app(text)
        && name == "List"
    {
        let args = split_top_level_commas(args);
        if let [item] = args.as_slice() {
            return RsigType::List(Box::new(parse_type_with_variants(item, variants)));
        }
    }
    if let Some((name, args)) = parse_named_type_app(text) {
        let name = TypeName::new(name);
        let args = split_top_level_commas(args)
            .into_iter()
            .map(|arg| parse_type_with_variants(arg, variants))
            .collect::<Vec<_>>();
        if variants.contains(&name) {
            return RsigType::VariantApp { name, args };
        }
    }
    resolve_declared_variants(parse_type(text), variants)
}

fn resolve_declared_variants(type_: RsigType, variants: &BTreeSet<TypeName>) -> RsigType {
    match type_ {
        RsigType::ActorId(message) => {
            RsigType::ActorId(Box::new(resolve_declared_variants(*message, variants)))
        }
        RsigType::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(resolve_declared_variants(*parameter, variants)),
            result: Box::new(resolve_declared_variants(*result, variants)),
        },
        RsigType::List(element) => {
            RsigType::List(Box::new(resolve_declared_variants(*element, variants)))
        }
        RsigType::Tuple(items) => RsigType::Tuple(
            items
                .into_iter()
                .map(|item| resolve_declared_variants(item, variants))
                .collect(),
        ),
        RsigType::VariantApp { name, args } => RsigType::VariantApp {
            name,
            args: args
                .into_iter()
                .map(|arg| resolve_declared_variants(arg, variants))
                .collect(),
        },
        RsigType::Record(name) => {
            if variants.contains(&name) {
                RsigType::Variant(name)
            } else {
                RsigType::Record(name)
            }
        }
        other => other,
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

fn parse_named_type_app(text: &str) -> Option<(&str, &str)> {
    let (name, rest) = text.split_once('<')?;
    let args = rest.strip_suffix('>')?;
    let name = name.trim();
    if name.is_empty() || name == "actor_id" {
        return None;
    }
    Some((name, args))
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
        7 => RsigType::Var(get_string(cursor)?),
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

pub(crate) type ImportedSignatures = BTreeMap<ModuleName, Rsig>;

#[cfg(test)]
mod tests {
    use super::{
        ConstructorName, FieldName, Rsig, RsigDependency, RsigExport, RsigFunction,
        RsigRecordField, RsigType, RsigTypeDecl, RsigTypeDeclKind, RsigTypeScheme,
        RsigVariantConstructor, TypeName, TypeParamName, decode_rsig, encode_rsig,
        parse_type_signature_with_variants,
    };
    use std::collections::BTreeSet;

    #[test]
    fn parses_parenthesized_arrow_parameter_types() {
        let (params, result) =
            parse_type_signature_with_variants("(i64 -> i64) -> i64 -> i64", &BTreeSet::new());

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
    fn parses_declared_variants_in_type_signatures() {
        let declared = BTreeSet::from([TypeName::new("color"), TypeName::new("Palette.color")]);

        let (params, result) =
            parse_type_signature_with_variants("color -> List<Palette.color>", &declared);

        assert_eq!(params, vec![RsigType::Variant(TypeName::new("color"))]);
        assert_eq!(
            result,
            RsigType::List(Box::new(RsigType::Variant(TypeName::new("Palette.color"))))
        );
    }

    #[test]
    fn binary_rsig_roundtrips_arrow_types() {
        let rsig = Rsig::with_dependencies(
            "Apply".to_owned(),
            Vec::new(),
            Vec::new(),
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
        let first = Rsig::with_dependencies(
            "Id".to_owned(),
            Vec::new(),
            Vec::new(),
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
        let second = Rsig::with_dependencies(
            "Id".to_owned(),
            Vec::new(),
            Vec::new(),
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

    #[test]
    fn binary_rsig_roundtrips_dependency_fingerprints() {
        let rsig = Rsig::with_dependencies(
            "UsesMath".to_owned(),
            vec![RsigDependency {
                module: "Math".into(),
                fingerprint: 0xfeed_f00d,
            }],
            Vec::new(),
            Vec::new(),
        );

        let decoded = decode_rsig(&encode_rsig(&rsig)).unwrap();

        assert_eq!(decoded.dependencies, rsig.dependencies);
        assert!(
            decoded
                .canonical_text()
                .contains("depends Math 00000000feedf00d")
        );
    }

    #[test]
    fn binary_rsig_roundtrips_record_type_names() {
        let point = RsigType::Record(TypeName::new("point"));
        let rsig = Rsig::with_dependencies(
            "Geometry".to_owned(),
            Vec::new(),
            Vec::new(),
            vec![RsigExport::Function(RsigFunction {
                name: "origin".to_owned(),
                params: Vec::new(),
                result: point.clone(),
                scheme: RsigTypeScheme::from_signature(&[], &point),
                symbol: "riot_mod_Geometry_origin".to_owned(),
                fingerprint: 0,
            })],
        );

        let decoded = decode_rsig(&encode_rsig(&rsig)).unwrap();

        assert_eq!(decoded, rsig);
        assert!(decoded.canonical_text().contains("fn origin() -> point"));
    }

    #[test]
    fn binary_rsig_roundtrips_record_type_declarations() {
        let rsig = Rsig::with_dependencies(
            "Geometry".to_owned(),
            Vec::new(),
            vec![RsigTypeDecl {
                name: TypeName::new("point"),
                params: Vec::new(),
                body: RsigTypeDeclKind::Record {
                    fields: vec![
                        RsigRecordField {
                            name: FieldName::new("x"),
                            type_: RsigType::I64,
                        },
                        RsigRecordField {
                            name: FieldName::new("y"),
                            type_: RsigType::I64,
                        },
                    ],
                },
                fingerprint: 0,
            }],
            Vec::new(),
        );

        let decoded = decode_rsig(&encode_rsig(&rsig)).unwrap();

        assert_eq!(decoded, rsig);
        assert!(
            decoded
                .canonical_text()
                .contains("type point = { x: i64, y: i64 }")
        );
    }

    #[test]
    fn binary_rsig_roundtrips_variant_type_declarations() {
        let rsig = Rsig::with_dependencies(
            "Colors".to_owned(),
            Vec::new(),
            vec![RsigTypeDecl {
                name: TypeName::new("color"),
                params: Vec::new(),
                body: RsigTypeDeclKind::Variant {
                    constructors: vec![
                        RsigVariantConstructor {
                            name: ConstructorName::new("Red"),
                            payload: Vec::new(),
                        },
                        RsigVariantConstructor {
                            name: ConstructorName::new("Green"),
                            payload: Vec::new(),
                        },
                    ],
                },
                fingerprint: 0,
            }],
            Vec::new(),
        );

        let decoded = decode_rsig(&encode_rsig(&rsig)).unwrap();

        assert_eq!(decoded, rsig);
        assert!(
            decoded
                .canonical_text()
                .contains("type color = Red | Green")
        );
        assert!(decoded.find_constructor("Red").is_some());
    }

    #[test]
    fn binary_rsig_roundtrips_variant_constructor_payloads() {
        let rsig = Rsig::with_dependencies(
            "Options".to_owned(),
            Vec::new(),
            vec![RsigTypeDecl {
                name: TypeName::new("option_i64"),
                params: Vec::new(),
                body: RsigTypeDeclKind::Variant {
                    constructors: vec![
                        RsigVariantConstructor {
                            name: ConstructorName::new("Some"),
                            payload: vec![RsigType::I64],
                        },
                        RsigVariantConstructor {
                            name: ConstructorName::new("Pair"),
                            payload: vec![RsigType::I64, RsigType::String],
                        },
                        RsigVariantConstructor {
                            name: ConstructorName::new("None"),
                            payload: Vec::new(),
                        },
                    ],
                },
                fingerprint: 0,
            }],
            Vec::new(),
        );

        let decoded = decode_rsig(&encode_rsig(&rsig)).unwrap();

        assert_eq!(decoded, rsig);
        assert!(
            decoded
                .canonical_text()
                .contains("type option_i64 = Some(i64) | Pair(i64, String) | None")
        );
    }

    #[test]
    fn binary_rsig_roundtrips_abstract_generic_types() {
        let rsig = Rsig::with_dependencies(
            "Prelude".to_owned(),
            Vec::new(),
            vec![RsigTypeDecl {
                name: TypeName::new("option"),
                params: vec![TypeParamName::new("'value")],
                body: RsigTypeDeclKind::Abstract,
                fingerprint: 0,
            }],
            Vec::new(),
        );

        let decoded = decode_rsig(&encode_rsig(&rsig)).unwrap();

        assert_eq!(decoded, rsig);
        assert!(decoded.canonical_text().contains("type option<'value>"));
    }
}
