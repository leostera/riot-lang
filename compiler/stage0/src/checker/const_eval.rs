use std::collections::HashMap;

use crate::ast::AstExpr;

use super::types::PrimitiveType;

#[derive(Debug, Clone)]
pub(super) enum ConstValue {
    Bool(bool),
    Char(char),
    Float(String),
    Int(i64),
    String(String),
    Unit,
    Tuple(Vec<ConstValue>),
    List(Vec<ConstValue>),
    Record {
        path: String,
        fields: Vec<(String, ConstValue)>,
    },
}

impl ConstValue {
    pub(super) fn to_print_string(&self) -> String {
        match self {
            ConstValue::Bool(true) => "true".to_owned(),
            ConstValue::Bool(false) => "false".to_owned(),
            ConstValue::Char(value) => value.to_string(),
            ConstValue::Float(value) => value.clone(),
            ConstValue::Int(value) => value.to_string(),
            ConstValue::String(value) => value.clone(),
            ConstValue::Unit => "()".to_owned(),
            ConstValue::Tuple(items) => {
                let rendered = items
                    .iter()
                    .map(ConstValue::to_print_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({rendered})")
            }
            ConstValue::List(items) => {
                let rendered = items
                    .iter()
                    .map(ConstValue::to_print_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{rendered}]")
            }
            ConstValue::Record { path, fields } => {
                let rendered = fields
                    .iter()
                    .map(|(name, value)| format!("{name}: {}", value.to_print_string()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{path} {{ {rendered} }}")
            }
        }
    }

    pub(super) fn type_name(&self) -> String {
        match self {
            ConstValue::Bool(_) => "bool".to_owned(),
            ConstValue::Char(_) => "char".to_owned(),
            ConstValue::Float(_) => "f64".to_owned(),
            ConstValue::Int(_) => "i64".to_owned(),
            ConstValue::String(_) => "string".to_owned(),
            ConstValue::Unit => "unit".to_owned(),
            ConstValue::Tuple(items) => {
                let rendered = items
                    .iter()
                    .map(ConstValue::type_name)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({rendered})")
            }
            ConstValue::List(items) => {
                let element = items
                    .first()
                    .map(ConstValue::type_name)
                    .unwrap_or_else(|| "_".to_owned());
                format!("{element} list")
            }
            ConstValue::Record { path, fields: _ } => path.clone(),
        }
    }

    pub(super) fn matches_primitive(&self, type_: PrimitiveType) -> bool {
        match (self, type_) {
            (ConstValue::Bool(_), PrimitiveType::Bool)
            | (ConstValue::Char(_), PrimitiveType::Char)
            | (ConstValue::String(_), PrimitiveType::String)
            | (ConstValue::Unit, PrimitiveType::Unit)
            | (
                ConstValue::Float(_),
                PrimitiveType::F16 | PrimitiveType::F32 | PrimitiveType::F64,
            ) => true,
            (ConstValue::Int(value), PrimitiveType::Byte | PrimitiveType::U8) => {
                u8::try_from(*value).is_ok()
            }
            (ConstValue::Int(value), PrimitiveType::I8) => i8::try_from(*value).is_ok(),
            (ConstValue::Int(value), PrimitiveType::I16) => i16::try_from(*value).is_ok(),
            (ConstValue::Int(value), PrimitiveType::I32) => i32::try_from(*value).is_ok(),
            (ConstValue::Int(_), PrimitiveType::I64) => true,
            (ConstValue::Int(_), PrimitiveType::I128) => true,
            (ConstValue::Int(value), PrimitiveType::ISize) => isize::try_from(*value).is_ok(),
            (ConstValue::Int(value), PrimitiveType::U16) => u16::try_from(*value).is_ok(),
            (ConstValue::Int(value), PrimitiveType::U32) => u32::try_from(*value).is_ok(),
            (ConstValue::Int(value), PrimitiveType::U64) => u64::try_from(*value).is_ok(),
            (ConstValue::Int(value), PrimitiveType::U128) => u128::try_from(*value).is_ok(),
            (ConstValue::Int(value), PrimitiveType::USize) => usize::try_from(*value).is_ok(),
            _ => false,
        }
    }
}

pub(super) fn resolve_const_value(
    expr: &AstExpr,
    bindings: &HashMap<String, ConstValue>,
) -> Option<ConstValue> {
    match expr {
        AstExpr::Add { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs + rhs)),
            _ => None,
        },
        AstExpr::Sub { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs - rhs)),
            _ => None,
        },
        AstExpr::Mul { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs * rhs)),
            _ => None,
        },
        AstExpr::Div { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Int(_), ConstValue::Int(0)) => None,
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs / rhs)),
            _ => None,
        },
        AstExpr::Mod { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Int(_), ConstValue::Int(0)) => None,
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs % rhs)),
            _ => None,
        },
        AstExpr::Neg { expr, span: _ } => match resolve_const_value(expr, bindings)? {
            ConstValue::Float(value) => Some(ConstValue::Float(format!("-{value}"))),
            ConstValue::Int(value) => Some(ConstValue::Int(-value)),
            _ => None,
        },
        AstExpr::Eq { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(ConstValue::Bool(lhs == rhs)),
            (ConstValue::Char(lhs), ConstValue::Char(rhs)) => Some(ConstValue::Bool(lhs == rhs)),
            (ConstValue::Float(lhs), ConstValue::Float(rhs)) => Some(ConstValue::Bool(lhs == rhs)),
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Bool(lhs == rhs)),
            (ConstValue::String(lhs), ConstValue::String(rhs)) => {
                Some(ConstValue::Bool(lhs == rhs))
            }
            _ => None,
        },
        AstExpr::Lt { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Bool(lhs < rhs)),
            _ => None,
        },
        AstExpr::And { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(ConstValue::Bool(lhs && rhs)),
            _ => None,
        },
        AstExpr::Or { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings)?,
            resolve_const_value(rhs, bindings)?,
        ) {
            (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(ConstValue::Bool(lhs || rhs)),
            _ => None,
        },
        AstExpr::Not { expr, span: _ } => match resolve_const_value(expr, bindings)? {
            ConstValue::Bool(value) => Some(ConstValue::Bool(!value)),
            _ => None,
        },
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            span: _,
        } => match resolve_const_value(condition, bindings)? {
            ConstValue::Bool(true) => resolve_const_value(then_branch, bindings),
            ConstValue::Bool(false) => resolve_const_value(else_branch, bindings),
            _ => None,
        },
        AstExpr::Bool { value, span: _ } => Some(ConstValue::Bool(*value)),
        AstExpr::Char { value, span: _ } => Some(ConstValue::Char(*value)),
        AstExpr::Unit { span: _ } => Some(ConstValue::Unit),
        AstExpr::Tuple { items, span: _ } => items
            .iter()
            .map(|item| resolve_const_value(item, bindings))
            .collect::<Option<Vec<_>>>()
            .map(ConstValue::Tuple),
        AstExpr::List { items, span: _ } => items
            .iter()
            .map(|item| resolve_const_value(item, bindings))
            .collect::<Option<Vec<_>>>()
            .map(ConstValue::List),
        AstExpr::Record {
            path,
            fields,
            span: _,
        } => fields
            .iter()
            .map(|(name, value)| Some((name.clone(), resolve_const_value(value, bindings)?)))
            .collect::<Option<Vec<_>>>()
            .map(|fields| ConstValue::Record {
                path: path.segments.join("."),
                fields,
            }),
        AstExpr::Float { value, span: _ } => Some(ConstValue::Float(value.clone())),
        AstExpr::Int { value, span: _ } => Some(ConstValue::Int(*value)),
        AstExpr::String { value, span: _ } => Some(ConstValue::String(value.clone())),
        AstExpr::Path { path, span: _ } if path.segments.len() == 1 => {
            bindings.get(&path.segments[0]).cloned()
        }
        AstExpr::Path { .. } | AstExpr::Call { .. } => None,
    }
}
