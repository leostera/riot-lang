use crate::signature::{
    Rsig, RsigDependency, RsigExport, RsigExternal, RsigFunction, RsigRecordField, RsigTypeDecl,
    RsigTypeDeclKind, RsigTypeScheme, RsigVariantConstructor,
};

use super::tyir::{TypedProgram, TypedTypeBody};

pub(super) fn signature_for(program: &TypedProgram) -> Rsig {
    let types = program
        .types
        .iter()
        .map(|type_| RsigTypeDecl {
            name: type_.name.clone(),
            params: type_.params.clone(),
            body: match &type_.body {
                TypedTypeBody::Abstract => RsigTypeDeclKind::Abstract,
                TypedTypeBody::Variant { constructors } => RsigTypeDeclKind::Variant {
                    constructors: constructors
                        .iter()
                        .map(|constructor| RsigVariantConstructor {
                            name: constructor.name.clone(),
                            payload: constructor.payload.clone(),
                        })
                        .collect(),
                },
                TypedTypeBody::Record { fields } => RsigTypeDeclKind::Record {
                    fields: fields
                        .iter()
                        .map(|field| RsigRecordField {
                            name: field.name.clone(),
                            type_: field.type_.clone(),
                        })
                        .collect(),
                },
            },
            fingerprint: 0,
        })
        .collect::<Vec<_>>();
    let mut exports = Vec::new();
    for function in &program.functions {
        if function.name == "main" {
            continue;
        }
        exports.push(RsigExport::Function(RsigFunction {
            name: function.name.clone(),
            params: function
                .params
                .iter()
                .map(|param| param.type_.clone())
                .collect(),
            result: function.result.clone(),
            scheme: RsigTypeScheme::from_signature(
                &function
                    .params
                    .iter()
                    .map(|param| param.type_.clone())
                    .collect::<Vec<_>>(),
                &function.result,
            ),
            symbol: function.symbol.clone(),
            fingerprint: 0,
        }));
    }
    for external in &program.externals {
        exports.push(RsigExport::External(RsigExternal {
            name: external.name.clone(),
            params: external.params.clone(),
            result: external.result.clone(),
            scheme: RsigTypeScheme::from_signature(&external.params, &external.result),
            abi: external.abi.clone(),
            fingerprint: 0,
        }));
    }
    let dependencies = program
        .uses
        .iter()
        .map(|use_| RsigDependency {
            module: use_.name.clone(),
            fingerprint: use_.fingerprint,
        })
        .collect();
    Rsig::with_dependencies(program.module_name.clone(), dependencies, types, exports)
}
