use std::collections::BTreeSet;

use crate::ast::{AstPath, AstTypeExpr};
use crate::signature::{RsigType, TypeName};

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct RsigTypeLowerer;

impl RsigTypeLowerer {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn lower(&self, type_: &AstTypeExpr, variants: &BTreeSet<TypeName>) -> RsigType {
        lower_type_expr(type_, variants)
    }

    pub(crate) fn lower_signature(
        &self,
        type_: &AstTypeExpr,
        variants: &BTreeSet<TypeName>,
    ) -> (Vec<RsigType>, RsigType) {
        split_signature_type(self.lower(type_, variants))
    }
}

fn split_signature_type(type_: RsigType) -> (Vec<RsigType>, RsigType) {
    let mut params = Vec::new();
    let mut result = type_;
    while let RsigType::Arrow {
        parameter,
        result: next,
    } = result
    {
        params.push(*parameter);
        result = *next;
    }
    (params, result)
}

fn lower_type_expr(type_: &AstTypeExpr, variants: &BTreeSet<TypeName>) -> RsigType {
    match type_ {
        AstTypeExpr::Wildcard { .. } => RsigType::Unknown,
        AstTypeExpr::Var { name, .. } => RsigType::Var(name.clone()),
        AstTypeExpr::Path { path, .. } => lower_type_path(&path.segments.join("."), variants),
        AstTypeExpr::Apply {
            constructor, args, ..
        } => lower_type_app(constructor, args, variants),
        AstTypeExpr::Tuple { items, .. } => RsigType::Tuple(
            items
                .iter()
                .map(|item| lower_type_expr(item, variants))
                .collect(),
        ),
        AstTypeExpr::Arrow {
            parameter, result, ..
        } => RsigType::Arrow {
            parameter: Box::new(lower_type_expr(parameter, variants)),
            result: Box::new(lower_type_expr(result, variants)),
        },
    }
}

fn lower_type_path(path: &str, variants: &BTreeSet<TypeName>) -> RsigType {
    match path {
        "bool" => RsigType::Bool,
        "char" => RsigType::Char,
        "f64" | "float" => RsigType::F64,
        "i32" => RsigType::I32,
        "i64" | "int" => RsigType::I64,
        "String" => RsigType::String,
        "unit" => RsigType::Unit,
        "_" | "" => RsigType::Unknown,
        _ => {
            let name = TypeName::new(path);
            if variants.contains(&name) {
                RsigType::Variant(name)
            } else {
                RsigType::Record(name)
            }
        }
    }
}

fn lower_type_app(
    constructor: &AstPath,
    args: &[AstTypeExpr],
    variants: &BTreeSet<TypeName>,
) -> RsigType {
    let name = constructor.segments.join(".");
    let args = args
        .iter()
        .map(|arg| lower_type_expr(arg, variants))
        .collect::<Vec<_>>();
    match (name.as_str(), args.as_slice()) {
        ("actor_id", [message]) => RsigType::ActorId(Box::new(message.clone())),
        ("List", [item]) => RsigType::List(Box::new(item.clone())),
        _ => {
            let name = TypeName::new(name);
            if variants.contains(&name) {
                RsigType::VariantApp { name, args }
            } else {
                RsigType::Record(name)
            }
        }
    }
}
