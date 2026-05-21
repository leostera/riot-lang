use std::collections::BTreeMap;

use crate::imported_types::qualify_imported_type;
use crate::signature::{
    AbiSymbol, FunctionSignature, FunctionTable, ImportedSignatures, ModuleName, RsigExport,
    RsigType,
};
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
    pub(crate) function: FunctionSignature,
    pub(crate) kind: CallableKind,
    pub(crate) abi: Option<AbiSymbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternalSignature {
    pub(crate) function: FunctionSignature,
    pub(crate) abi: AbiSymbol,
}

impl ExternalSignature {
    pub(crate) fn new(params: Vec<RsigType>, result: RsigType, abi: impl Into<AbiSymbol>) -> Self {
        Self {
            function: FunctionSignature::new(params, result),
            abi: abi.into(),
        }
    }
}

impl CallableSignature {
    pub(crate) fn is_output_operation(&self) -> bool {
        matches!(
            self.abi.as_deref(),
            Some("riot_rt_dbg_value" | "riot_rt_println" | "riot_prim_println")
        )
    }

    pub(crate) fn is_actor_operation(&self) -> bool {
        matches!(
            self.abi.as_deref(),
            Some("riot_rt_send_value" | "riot_rt_monitor" | "riot_rt_link")
        )
    }

    pub(crate) fn is_static_list_get(&self) -> bool {
        self.abi.as_deref() == Some("riot_rt_value_list_get")
    }
}

pub(crate) struct CallableResolver<'a> {
    functions: &'a FunctionTable,
    externals: &'a BTreeMap<String, ExternalSignature>,
    imports: &'a ImportedSignatures,
}

impl<'a> CallableResolver<'a> {
    pub(crate) fn new(
        functions: &'a FunctionTable,
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
                    function: external.function.clone(),
                    kind: CallableKind::External,
                    abi: Some(external.abi.clone()),
                });
            }
            if let Some(function) = self.functions.get(name) {
                return Some(CallableSignature {
                    function: function.clone(),
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
                    function: FunctionSignature::new(
                        function
                            .params
                            .iter()
                            .map(|param| qualify_imported_type(module, param))
                            .collect(),
                        qualify_imported_type(module, &function.result),
                    ),
                    kind: CallableKind::ImportedFunction {
                        module: module_name,
                    },
                    abi: None,
                },
                RsigExport::External(external) => CallableSignature {
                    function: FunctionSignature::new(
                        external
                            .params
                            .iter()
                            .map(|param| qualify_imported_type(module, param))
                            .collect(),
                        qualify_imported_type(module, &external.result),
                    ),
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
                        ExternalSignature {
                            function: FunctionSignature::new(external.params, external.result),
                            abi: external.abi,
                        },
                    ))
                })
                .collect()
        })
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use crate::signature::{FunctionTable, ImportedSignatures, RsigType};

    use super::{CallableKind, CallableResolver, ExternalSignature, prelude_external_signatures};

    #[test]
    fn resolves_prelude_externals_from_std_source() {
        let functions = FunctionTable::new();
        let externals = prelude_external_signatures();
        let imports = ImportedSignatures::new();
        let resolver = CallableResolver::new(&functions, &externals, &imports);

        let signature = resolver
            .resolve(&["Std".to_owned(), "Prelude".to_owned(), "send".to_owned()])
            .expect("prelude send should resolve");

        assert_eq!(signature.kind, CallableKind::External);
        assert_eq!(signature.function.result, RsigType::Unit);
        assert_eq!(signature.function.params.len(), 2);
        assert_eq!(signature.abi.as_deref(), Some("riot_rt_send_value"));
        assert!(signature.is_actor_operation());
        assert!(!signature.is_output_operation());
    }

    #[test]
    fn user_externals_override_prelude_signatures() {
        let functions = FunctionTable::new();
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

        assert_eq!(signature.function.params, vec![RsigType::String]);
        assert_eq!(signature.function.result, RsigType::String);
        assert_eq!(signature.abi.as_deref(), Some("user_dbg"));
        assert!(!signature.is_output_operation());
    }
}
