use std::collections::BTreeMap;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr, bail};

#[cfg(test)]
pub(crate) use crate::signature_type_parser::RsigTypeParser;

mod binary;
mod canonical;
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

impl Rsig {
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

impl RsigExport {
    pub(crate) fn name(&self) -> &str {
        match self {
            RsigExport::Function(function) => &function.name,
            RsigExport::External(external) => &external.name,
        }
    }
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
