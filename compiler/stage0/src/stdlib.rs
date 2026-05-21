use std::collections::BTreeSet;
use std::sync::OnceLock;

use camino::Utf8Path;
use miette::bail;

use crate::ast::{AstDecl, AstProgram, AstTypeBody};
use crate::parser::SourceParser;
use crate::signature::{
    ConstructorName, FieldName, ModuleName, Rsig, RsigExport, RsigExternal, RsigRecordField,
    RsigTypeDecl, RsigTypeDeclKind, RsigTypeScheme, RsigVariantConstructor, TypeName,
    TypeParamName,
};
use crate::type_lowerer::RsigTypeLowerer;

const STD_SOURCE: &str = include_str!("../../std/std.ml");
const PRELUDE_SOURCE: &str = include_str!("../../std/prelude.ml");
const ACTOR_SOURCE: &str = include_str!("../../std/actor.ml");
const LIST_SOURCE: &str = include_str!("../../std/list.ml");
const STRING_SOURCE: &str = include_str!("../../std/string.ml");
const OPTION_SOURCE: &str = include_str!("../../std/option.ml");
const RESULT_SOURCE: &str = include_str!("../../std/result.ml");
static PRELUDE_TYPE_NAMES: OnceLock<BTreeSet<TypeName>> = OnceLock::new();

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Stdlib;

impl Stdlib {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn prelude_signature(&self) -> miette::Result<Rsig> {
        prelude_signature()
    }

    pub(crate) fn signature(&self, module: &str) -> miette::Result<Option<Rsig>> {
        std_signature(module)
    }

    pub(crate) fn contains_signature(&self, rsig: &Rsig) -> bool {
        is_std_signature(rsig)
    }

    pub(crate) fn is_prelude_type_name(&self, type_name: &TypeName) -> bool {
        prelude_type_names().contains(type_name)
    }

    pub(crate) fn prelude_member_name<'a>(path: &'a [String]) -> Option<&'a str> {
        match path {
            [name] => Some(name.as_str()),
            [std, prelude, name] if std == "Std" && prelude == "Prelude" => Some(name.as_str()),
            _ => None,
        }
    }
}

fn prelude_signature() -> miette::Result<Rsig> {
    std_signature("Prelude")?.ok_or_else(|| miette::miette!("compiler/std/prelude.ml is missing"))
}

fn prelude_type_names() -> &'static BTreeSet<TypeName> {
    PRELUDE_TYPE_NAMES.get_or_init(|| {
        prelude_signature()
            .expect("compiler/std/prelude.ml must parse")
            .types
            .into_iter()
            .map(|type_| type_.name)
            .collect()
    })
}

fn std_signature(module: &str) -> miette::Result<Option<Rsig>> {
    let Some((module, path, source)) = std_source(module) else {
        return Ok(None);
    };
    let ast = SourceParser::new().parse(Utf8Path::new(path), source)?;
    module_signature(module, ast)
}

fn is_std_signature(rsig: &Rsig) -> bool {
    std_signature(rsig.module.as_str())
        .ok()
        .flatten()
        .is_some_and(|std| std.module_fingerprint == rsig.module_fingerprint)
}

fn std_source(module: &str) -> Option<(ModuleName, &'static str, &'static str)> {
    Some(match module {
        "Std" => (ModuleName::new("Std"), "compiler/std/std.ml", STD_SOURCE),
        "Prelude" => (
            ModuleName::new("Prelude"),
            "compiler/std/prelude.ml",
            PRELUDE_SOURCE,
        ),
        "Actor" => (
            ModuleName::new("Actor"),
            "compiler/std/actor.ml",
            ACTOR_SOURCE,
        ),
        "List" => (ModuleName::new("List"), "compiler/std/list.ml", LIST_SOURCE),
        "String" => (
            ModuleName::new("String"),
            "compiler/std/string.ml",
            STRING_SOURCE,
        ),
        "Option" => (
            ModuleName::new("Option"),
            "compiler/std/option.ml",
            OPTION_SOURCE,
        ),
        "Result" => (
            ModuleName::new("Result"),
            "compiler/std/result.ml",
            RESULT_SOURCE,
        ),
        _ => return None,
    })
}

fn module_signature(module: ModuleName, ast: AstProgram) -> miette::Result<Option<Rsig>> {
    let declared_variants = declared_variants(&ast);
    let mut types = Vec::new();
    let mut exports = Vec::new();
    for decl in ast.decls {
        match decl {
            AstDecl::Use(_) | AstDecl::Module(_) => {}
            AstDecl::Include(include) => {
                let Some(included) = std_signature(&include.name)? else {
                    bail!(
                        "compiler/std include {} does not name a std module",
                        include.name
                    );
                };
                types.extend(included.types);
                exports.extend(included.exports);
            }
            AstDecl::Type(type_) => {
                types.push(RsigTypeDecl {
                    name: TypeName::new(type_.name),
                    params: type_
                        .params
                        .into_iter()
                        .map(|param| TypeParamName::new(param.name))
                        .collect(),
                    body: match type_.body {
                        AstTypeBody::Abstract => RsigTypeDeclKind::Abstract,
                        AstTypeBody::Variant { constructors } => RsigTypeDeclKind::Variant {
                            constructors: constructors
                                .into_iter()
                                .map(|constructor| RsigVariantConstructor {
                                    name: ConstructorName::new(constructor.name),
                                    payload: constructor
                                        .payload
                                        .into_iter()
                                        .map(|payload| {
                                            RsigTypeLowerer::new()
                                                .lower(&payload.syntax, &declared_variants)
                                        })
                                        .collect(),
                                })
                                .collect(),
                        },
                        AstTypeBody::Record { fields } => RsigTypeDeclKind::Record {
                            fields: fields
                                .into_iter()
                                .map(|field| RsigRecordField {
                                    name: FieldName::new(field.name),
                                    type_: RsigTypeLowerer::new()
                                        .lower(&field.type_annotation.syntax, &declared_variants),
                                })
                                .collect(),
                        },
                    },
                    fingerprint: 0,
                });
            }
            AstDecl::External(external) => {
                let (params, result) = RsigTypeLowerer::new()
                    .lower_signature(&external.type_annotation.syntax, &declared_variants);
                let scheme = RsigTypeScheme::from_signature(&params, &result);
                exports.push(RsigExport::External(RsigExternal {
                    name: external.name,
                    params,
                    result,
                    scheme,
                    abi: external.abi,
                    fingerprint: 0,
                }));
            }
            AstDecl::Function(_) => {
                bail!("compiler/std modules currently support declarations, not functions")
            }
        }
    }
    Ok(Some(Rsig::with_dependencies(
        module,
        Vec::new(),
        types,
        exports,
    )))
}

fn declared_variants(ast: &AstProgram) -> BTreeSet<TypeName> {
    let mut names = BTreeSet::from([
        TypeName::new("List"),
        TypeName::new("Option"),
        TypeName::new("Result"),
    ]);
    names.extend(ast.decls.iter().filter_map(|decl| {
        let AstDecl::Type(type_) = decl else {
            return None;
        };
        matches!(type_.body, AstTypeBody::Variant { .. }).then(|| TypeName::new(type_.name.clone()))
    }));
    names
}

#[cfg(test)]
mod tests {
    use super::Stdlib;

    #[test]
    fn prelude_source_loads_as_binary_signature_data() {
        let rsig = Stdlib::new().prelude_signature().unwrap();
        let names = rsig
            .exports
            .iter()
            .map(|export| export.name().to_owned())
            .collect::<Vec<_>>();

        assert_eq!(rsig.module.as_str(), "Prelude");
        assert!(names.contains(&"dbg".to_owned()));
        assert!(names.contains(&"(+)".to_owned()));
        assert!(names.contains(&"send".to_owned()));
        assert!(names.contains(&"string_concat".to_owned()));
        assert!(
            rsig.canonical_text()
                .contains("type List<'value> = Nil | Cons('value, List<'value>)")
        );
        assert!(
            rsig.canonical_text()
                .contains("type Option<'value> = Some('value) | None")
        );
        assert!(rsig.canonical_text().contains("type Never"));
    }

    #[test]
    fn std_index_source_loads_prelude_exports() {
        let rsig = Stdlib::new().signature("Std").unwrap().unwrap();

        assert_eq!(rsig.module.as_str(), "Std");
        assert!(rsig.find("dbg").is_some());
        assert!(rsig.find("send").is_some());
    }

    #[test]
    fn std_modules_load_as_signatures() {
        let stdlib = Stdlib::new();
        for module in ["Actor", "List", "String", "Option", "Result"] {
            let rsig = stdlib.signature(module).unwrap().unwrap();
            assert_eq!(rsig.module.as_str(), module);
        }
        assert!(
            stdlib
                .signature("List")
                .unwrap()
                .unwrap()
                .find("len")
                .is_some()
        );
        assert!(
            stdlib
                .signature("Option")
                .unwrap()
                .unwrap()
                .find("is_some")
                .is_some()
        );
        assert!(
            stdlib
                .signature("Result")
                .unwrap()
                .unwrap()
                .canonical_text()
                .contains("external unwrap_or(Result<'a, 'b>, 'a) -> 'a")
        );
    }

    #[test]
    fn prelude_type_names_come_from_std_source() {
        let stdlib = Stdlib::new();

        assert!(stdlib.is_prelude_type_name(&crate::signature::TypeName::new("List")));
        assert!(stdlib.is_prelude_type_name(&crate::signature::TypeName::new("Option")));
        assert!(!stdlib.is_prelude_type_name(&crate::signature::TypeName::new("Matches")));
    }

    #[test]
    fn prelude_member_name_accepts_local_and_std_prelude_paths() {
        assert_eq!(
            Stdlib::prelude_member_name(&["dbg".to_owned()]),
            Some("dbg")
        );
        assert_eq!(
            Stdlib::prelude_member_name(&[
                "Std".to_owned(),
                "Prelude".to_owned(),
                "(+)".to_owned()
            ]),
            Some("(+)")
        );
        assert_eq!(
            Stdlib::prelude_member_name(&["Std".to_owned(), "List".to_owned(), "len".to_owned()]),
            None
        );
    }
}
