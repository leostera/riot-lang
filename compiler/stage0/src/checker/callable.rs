use std::collections::BTreeMap;

use crate::imported_types::qualify_imported_type;
use crate::signature::{ImportedSignatures, ModuleName, RsigExport, RsigType};
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
    pub(crate) abi: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternalSignature {
    pub(crate) params: Vec<RsigType>,
    pub(crate) result: RsigType,
    pub(crate) abi: String,
}

impl ExternalSignature {
    pub(crate) fn new(params: Vec<RsigType>, result: RsigType, abi: impl Into<String>) -> Self {
        Self {
            params,
            result,
            abi: abi.into(),
        }
    }
}

pub(crate) struct CallableResolver<'a> {
    functions: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: &'a BTreeMap<String, ExternalSignature>,
    imports: &'a ImportedSignatures,
}

impl<'a> CallableResolver<'a> {
    pub(crate) fn new(
        functions: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
        externals: &'a BTreeMap<String, ExternalSignature>,
        imports: &'a ImportedSignatures,
    ) -> Self {
        Self {
            functions,
            externals,
            imports,
        }
    }

    pub(crate) fn resolve(&self, callee: &[String]) -> Option<CallableSignature> {
        if let Some(name) = Stdlib::prelude_member_name(callee) {
            if let Some(external) = self.externals.get(name) {
                return Some(CallableSignature {
                    params: external.params.clone(),
                    result: external.result.clone(),
                    kind: CallableKind::External,
                    abi: Some(external.abi.clone()),
                });
            }
            if let Some((params, result)) = self.functions.get(name) {
                return Some(CallableSignature {
                    params: params.clone(),
                    result: result.clone(),
                    kind: CallableKind::Function,
                    abi: None,
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
                    abi: None,
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
                    abi: Some(external.abi.clone()),
                },
            })
    }
}

pub(crate) fn prelude_external_signatures() -> BTreeMap<String, ExternalSignature> {
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
                    Some((
                        external.name,
                        ExternalSignature::new(external.params, external.result, external.abi),
                    ))
                })
                .collect()
        })
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::signature::{ImportedSignatures, RsigType};

    use super::{CallableKind, CallableResolver, ExternalSignature, prelude_external_signatures};

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
        assert_eq!(signature.abi.as_deref(), Some("riot_rt_send_value"));
    }

    #[test]
    fn user_externals_override_prelude_signatures() {
        let functions = BTreeMap::new();
        let mut externals = prelude_external_signatures();
        externals.insert(
            "dbg".to_owned(),
            ExternalSignature::new(vec![RsigType::String], RsigType::String, "user_dbg"),
        );
        let imports = ImportedSignatures::new();
        let resolver = CallableResolver::new(&functions, &externals, &imports);

        let signature = resolver
            .resolve(&["dbg".to_owned()])
            .expect("dbg should resolve");

        assert_eq!(signature.params, vec![RsigType::String]);
        assert_eq!(signature.result, RsigType::String);
        assert_eq!(signature.abi.as_deref(), Some("user_dbg"));
    }
}
