use std::collections::BTreeMap;

use crate::signature::{ImportedSignatures, ModuleName, RsigExport, RsigType, TypeName};
use crate::stdlib::Stdlib;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CallableKind {
    Function,
    External,
    ImportedFunction { module: ModuleName },
    ImportedExternal { module: ModuleName },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallableSignature {
    pub(crate) params: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) kind: CallableKind,
}

pub(crate) struct CallableResolver<'a> {
    functions: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    imports: &'a ImportedSignatures,
}

impl<'a> CallableResolver<'a> {
    pub(crate) fn new(
        functions: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
        externals: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
        imports: &'a ImportedSignatures,
    ) -> Self {
        Self {
            functions,
            externals,
            imports,
        }
    }

    pub(crate) fn resolve(&self, callee: &[String]) -> Option<CallableSignature> {
        if let Some(name) = local_or_prelude_name(callee) {
            if let Some((params, result)) = self.externals.get(name) {
                return Some(CallableSignature {
                    params: params.clone(),
                    result: result.clone(),
                    kind: CallableKind::External,
                });
            }
            if let Some((params, result)) = self.functions.get(name) {
                return Some(CallableSignature {
                    params: params.clone(),
                    result: result.clone(),
                    kind: CallableKind::Function,
                });
            }
            return None;
        }

        let [module, name] = callee else {
            return None;
        };
        let module_name = ModuleName::new(module.clone());
        self.imports
            .get(module.as_str())
            .and_then(|rsig| rsig.find(name))
            .map(|export| match export {
                RsigExport::Function(function) => CallableSignature {
                    params: function
                        .params
                        .iter()
                        .map(|param| qualify_imported_type(module, param))
                        .collect(),
                    result: qualify_imported_type(module, &function.result),
                    kind: CallableKind::ImportedFunction {
                        module: module_name,
                    },
                },
                RsigExport::External(external) => CallableSignature {
                    params: external
                        .params
                        .iter()
                        .map(|param| qualify_imported_type(module, param))
                        .collect(),
                    result: qualify_imported_type(module, &external.result),
                    kind: CallableKind::ImportedExternal {
                        module: module_name,
                    },
                },
            })
    }
}

pub(crate) fn prelude_external_signatures() -> BTreeMap<String, (Vec<RsigType>, RsigType)> {
    crate::stdlib::Stdlib::new()
        .prelude_signature()
        .ok()
        .map(|rsig| {
            rsig.exports
                .into_iter()
                .filter_map(|export| {
                    let RsigExport::External(external) = export else {
                        return None;
                    };
                    Some((external.name, (external.params, external.result)))
                })
                .collect()
        })
        .unwrap_or_default()
}

fn local_or_prelude_name(callee: &[String]) -> Option<&str> {
    match callee {
        [name] => Some(name.as_str()),
        [std, prelude, name] if std == "Std" && prelude == "Prelude" => Some(name.as_str()),
        _ => None,
    }
}

fn qualify_imported_type(module: &str, type_: &RsigType) -> RsigType {
    match type_ {
        RsigType::ActorId(message) => {
            RsigType::ActorId(Box::new(qualify_imported_type(module, message)))
        }
        RsigType::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(qualify_imported_type(module, parameter)),
            result: Box::new(qualify_imported_type(module, result)),
        },
        RsigType::List(item) => RsigType::List(Box::new(qualify_imported_type(module, item))),
        RsigType::Tuple(items) => RsigType::Tuple(
            items
                .iter()
                .map(|item| qualify_imported_type(module, item))
                .collect(),
        ),
        RsigType::Record(name) => {
            RsigType::Record(TypeName::new(format!("{module}.{}", name.as_str())))
        }
        RsigType::Variant(name) => {
            if is_prelude_type_name(name) {
                RsigType::Variant(name.clone())
            } else {
                RsigType::Variant(TypeName::new(format!("{module}.{}", name.as_str())))
            }
        }
        RsigType::VariantApp { name, args } => {
            let name = if is_prelude_type_name(name) {
                name.clone()
            } else {
                TypeName::new(format!("{module}.{}", name.as_str()))
            };
            RsigType::VariantApp {
                name,
                args: args
                    .iter()
                    .map(|arg| qualify_imported_type(module, arg))
                    .collect(),
            }
        }
        other => other.clone(),
    }
}

fn is_prelude_type_name(type_name: &TypeName) -> bool {
    Stdlib::new().is_prelude_type_name(type_name)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::signature::{ImportedSignatures, RsigType};

    use super::{CallableKind, CallableResolver, prelude_external_signatures};

    #[test]
    fn resolves_prelude_externals_from_std_source() {
        let functions = BTreeMap::new();
        let externals = prelude_external_signatures();
        let imports = ImportedSignatures::new();
        let resolver = CallableResolver::new(&functions, &externals, &imports);

        let signature = resolver
            .resolve(&["Std".to_owned(), "Prelude".to_owned(), "send".to_owned()])
            .expect("prelude send should resolve");

        assert_eq!(signature.kind, CallableKind::External);
        assert_eq!(signature.result, RsigType::Unit);
        assert_eq!(signature.params.len(), 2);
    }

    #[test]
    fn user_externals_override_prelude_signatures() {
        let functions = BTreeMap::new();
        let mut externals = prelude_external_signatures();
        externals.insert("dbg".to_owned(), (vec![RsigType::String], RsigType::String));
        let imports = ImportedSignatures::new();
        let resolver = CallableResolver::new(&functions, &externals, &imports);

        let signature = resolver
            .resolve(&["dbg".to_owned()])
            .expect("dbg should resolve");

        assert_eq!(signature.params, vec![RsigType::String]);
        assert_eq!(signature.result, RsigType::String);
    }
}
