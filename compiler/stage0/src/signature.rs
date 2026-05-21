use std::collections::BTreeMap;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr, bail};

use crate::fingerprint::SignatureFingerprinter;
#[cfg(test)]
pub(crate) use crate::signature_type_parser::RsigTypeParser;

mod binary;
mod names;

use binary::{decode_rsig, encode_rsig};

pub(crate) use names::{
    ConstructorName, FieldName, ModuleName, TypeName, TypeParamName, TypeVarName,
};

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
    Var(TypeVarName),
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
    pub(crate) quantifiers: Vec<TypeVarName>,
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
                self.quantifiers
                    .iter()
                    .map(TypeVarName::as_str)
                    .collect::<Vec<_>>()
                    .join(" "),
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

fn collect_type_vars(type_: &RsigType, vars: &mut Vec<TypeVarName>) {
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
        let fingerprinter = SignatureFingerprinter::new();
        dependencies.sort_by(|lhs, rhs| lhs.module.cmp(&rhs.module));
        types.sort_by(|lhs, rhs| lhs.name.as_str().cmp(rhs.name.as_str()));
        for type_ in &mut types {
            type_.fingerprint = fingerprinter.stable_text(&type_.canonical_without_fingerprint());
        }
        for export in &mut exports {
            export.canonicalize_type_vars();
        }
        exports.sort_by(|lhs, rhs| lhs.name().cmp(rhs.name()).then(lhs.kind().cmp(rhs.kind())));
        for export in &mut exports {
            export.set_fingerprint(
                fingerprinter.stable_text(&export.canonical_without_fingerprint()),
            );
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
        let module_fingerprint = fingerprinter.stable_text(
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
    names: BTreeMap<TypeVarName, TypeVarName>,
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

fn canonical_type_var_name(index: usize) -> TypeVarName {
    if index < 26 {
        let name = char::from(b'a' + index as u8);
        TypeVarName::new(format!("'{name}"))
    } else {
        TypeVarName::new(format!("'t{}", index - 26))
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
            RsigType::Var(name) => name.to_string(),
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

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct RsigStore;

impl RsigStore {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn write(&self, path: &Utf8Path, rsig: &Rsig) -> miette::Result<()> {
        write_rsig(path, rsig)
    }

    pub(crate) fn read(&self, path: &Utf8Path) -> miette::Result<Rsig> {
        read_rsig(path)
    }

    pub(crate) fn resolve(
        &self,
        module: &str,
        source_dir: Option<&Utf8Path>,
        sig_dirs: &[Utf8PathBuf],
    ) -> miette::Result<(Utf8PathBuf, Rsig)> {
        resolve_rsig(module, source_dir, sig_dirs)
    }
}

fn write_rsig(path: &Utf8Path, rsig: &Rsig) -> miette::Result<()> {
    std::fs::write(path.as_std_path(), encode_rsig(rsig))
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to write {}", path))
}

fn read_rsig(path: &Utf8Path) -> miette::Result<Rsig> {
    let bytes = std::fs::read(path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read {}", path))?;
    decode_rsig(&bytes).wrap_err_with(|| format!("failed to decode {}", path))
}

fn resolve_rsig(
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
            let rsig = RsigStore::new().read(candidate)?;
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

pub(crate) type ImportedSignatures = BTreeMap<ModuleName, Rsig>;

#[cfg(test)]
mod tests {
    use super::{
        ConstructorName, FieldName, Rsig, RsigDependency, RsigExport, RsigFunction,
        RsigRecordField, RsigType, RsigTypeDecl, RsigTypeDeclKind, RsigTypeParser, RsigTypeScheme,
        RsigVariantConstructor, TypeName, TypeParamName, TypeVarName, decode_rsig, encode_rsig,
    };
    use std::collections::BTreeSet;

    #[test]
    fn parses_parenthesized_arrow_parameter_types() {
        let (params, result) = RsigTypeParser::new()
            .parse_signature_with_variants("(i64 -> i64) -> i64 -> i64", &BTreeSet::new());

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

        let (params, result) = RsigTypeParser::new()
            .parse_signature_with_variants("color -> List<Palette.color>", &declared);

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
                params: vec![RsigType::Var(TypeVarName::new("'t17"))],
                result: RsigType::Var(TypeVarName::new("'t17")),
                scheme: RsigTypeScheme::from_signature(
                    &[RsigType::Var(TypeVarName::new("'t17"))],
                    &RsigType::Var(TypeVarName::new("'t17")),
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
                params: vec![RsigType::Var(TypeVarName::new("'source_name"))],
                result: RsigType::Var(TypeVarName::new("'source_name")),
                scheme: RsigTypeScheme::from_signature(
                    &[RsigType::Var(TypeVarName::new("'source_name"))],
                    &RsigType::Var(TypeVarName::new("'source_name")),
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
