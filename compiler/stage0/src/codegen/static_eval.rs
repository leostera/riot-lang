use std::collections::HashMap;

use crate::ir::{RirBlock, RirExpr, RirFunction, RirStmt};

#[derive(Debug, Clone)]
pub(crate) enum StaticValue {
    Bool(bool),
    Char(char),
    Float(String),
    Int(i64),
    List(Vec<StaticValue>),
    Record {
        path: String,
        fields: Vec<(String, StaticValue)>,
    },
    String(String),
    Tuple(Vec<StaticValue>),
    Unit,
}

impl StaticValue {
    pub(crate) fn to_print_string(&self) -> String {
        match self {
            StaticValue::Bool(true) => "true".to_owned(),
            StaticValue::Bool(false) => "false".to_owned(),
            StaticValue::Char(value) => value.to_string(),
            StaticValue::Float(value) => value.clone(),
            StaticValue::Int(value) => value.to_string(),
            StaticValue::List(items) => {
                let rendered = items
                    .iter()
                    .map(StaticValue::to_print_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{rendered}]")
            }
            StaticValue::Record { path, fields } => {
                let rendered = fields
                    .iter()
                    .map(|(name, value)| format!("{name}: {}", value.to_print_string()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{path} {{ {rendered} }}")
            }
            StaticValue::String(value) => value.clone(),
            StaticValue::Tuple(items) => {
                let rendered = items
                    .iter()
                    .map(StaticValue::to_print_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({rendered})")
            }
            StaticValue::Unit => "()".to_owned(),
        }
    }

    pub(crate) fn as_int(&self) -> Option<i64> {
        match self {
            StaticValue::Int(value) => Some(*value),
            _ => None,
        }
    }

    pub(crate) fn as_bool(&self) -> Option<bool> {
        match self {
            StaticValue::Bool(value) => Some(*value),
            _ => None,
        }
    }
}

pub(crate) fn eval_expr(
    expr: &RirExpr,
    bindings: &HashMap<String, StaticValue>,
    functions: &HashMap<&str, &RirFunction>,
    depth: usize,
) -> Option<StaticValue> {
    if depth > 64 {
        return None;
    }
    match expr {
        RirExpr::Add(lhs, rhs) => Some(StaticValue::Int(
            eval_expr(lhs, bindings, functions, depth)?.as_int()?
                + eval_expr(rhs, bindings, functions, depth)?.as_int()?,
        )),
        RirExpr::Sub(lhs, rhs) => Some(StaticValue::Int(
            eval_expr(lhs, bindings, functions, depth)?.as_int()?
                - eval_expr(rhs, bindings, functions, depth)?.as_int()?,
        )),
        RirExpr::Mul(lhs, rhs) => Some(StaticValue::Int(
            eval_expr(lhs, bindings, functions, depth)?.as_int()?
                * eval_expr(rhs, bindings, functions, depth)?.as_int()?,
        )),
        RirExpr::Div(lhs, rhs) => {
            let rhs = eval_expr(rhs, bindings, functions, depth)?.as_int()?;
            if rhs == 0 {
                return None;
            }
            Some(StaticValue::Int(
                eval_expr(lhs, bindings, functions, depth)?.as_int()? / rhs,
            ))
        }
        RirExpr::Mod(lhs, rhs) => {
            let rhs = eval_expr(rhs, bindings, functions, depth)?.as_int()?;
            if rhs == 0 {
                return None;
            }
            Some(StaticValue::Int(
                eval_expr(lhs, bindings, functions, depth)?.as_int()? % rhs,
            ))
        }
        RirExpr::Neg(value) => match eval_expr(value, bindings, functions, depth)? {
            StaticValue::Float(value) => Some(StaticValue::Float(format!("-{value}"))),
            StaticValue::Int(value) => Some(StaticValue::Int(-value)),
            _ => None,
        },
        RirExpr::Eq(lhs, rhs) => {
            let lhs = eval_expr(lhs, bindings, functions, depth)?.to_print_string();
            let rhs = eval_expr(rhs, bindings, functions, depth)?.to_print_string();
            Some(StaticValue::Bool(lhs == rhs))
        }
        RirExpr::Lt(lhs, rhs) => Some(StaticValue::Bool(
            eval_expr(lhs, bindings, functions, depth)?.as_int()?
                < eval_expr(rhs, bindings, functions, depth)?.as_int()?,
        )),
        RirExpr::And(lhs, rhs) => Some(StaticValue::Bool(
            eval_expr(lhs, bindings, functions, depth)?.as_bool()?
                && eval_expr(rhs, bindings, functions, depth)?.as_bool()?,
        )),
        RirExpr::Or(lhs, rhs) => Some(StaticValue::Bool(
            eval_expr(lhs, bindings, functions, depth)?.as_bool()?
                || eval_expr(rhs, bindings, functions, depth)?.as_bool()?,
        )),
        RirExpr::Not(value) => Some(StaticValue::Bool(
            !eval_expr(value, bindings, functions, depth)?.as_bool()?,
        )),
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            if eval_expr(condition, bindings, functions, depth)?.as_bool()? {
                eval_expr(then_branch, bindings, functions, depth)
            } else {
                eval_expr(else_branch, bindings, functions, depth)
            }
        }
        RirExpr::Bool(value) => Some(StaticValue::Bool(*value)),
        RirExpr::Call { callee, args } => {
            let [name] = callee.as_slice() else {
                return None;
            };
            eval_call(name, args, bindings, functions, depth)
        }
        RirExpr::Unit => Some(StaticValue::Unit),
        RirExpr::Tuple(items) => items
            .iter()
            .map(|item| eval_expr(item, bindings, functions, depth))
            .collect::<Option<Vec<_>>>()
            .map(StaticValue::Tuple),
        RirExpr::List(items) => items
            .iter()
            .map(|item| eval_expr(item, bindings, functions, depth))
            .collect::<Option<Vec<_>>>()
            .map(StaticValue::List),
        RirExpr::Record { path, fields } => fields
            .iter()
            .map(|(name, value)| {
                Some((name.clone(), eval_expr(value, bindings, functions, depth)?))
            })
            .collect::<Option<Vec<_>>>()
            .map(|fields| StaticValue::Record {
                path: path.join("."),
                fields,
            }),
        RirExpr::Field { .. } | RirExpr::TupleIndex { .. } => None,
        RirExpr::Char(value) => Some(StaticValue::Char(*value)),
        RirExpr::Float(value) => Some(StaticValue::Float(value.clone())),
        RirExpr::Int(value) => Some(StaticValue::Int(*value)),
        RirExpr::Path(path) => resolve_path(path, bindings),
        RirExpr::String(value) => Some(StaticValue::String(value.clone())),
        RirExpr::Apply { .. }
        | RirExpr::Lambda { .. }
        | RirExpr::Spawn { .. }
        | RirExpr::Receive { .. } => None,
    }
}

pub(crate) fn eval_call(
    name: &str,
    args: &[RirExpr],
    bindings: &HashMap<String, StaticValue>,
    functions: &HashMap<&str, &RirFunction>,
    depth: usize,
) -> Option<StaticValue> {
    let function = functions.get(name)?;
    if function.params.len() != args.len() {
        return None;
    }
    let mut call_bindings = HashMap::new();
    for (param, arg) in function.params.iter().zip(args) {
        call_bindings.insert(
            param.as_str().to_owned(),
            eval_expr(arg, bindings, functions, depth + 1)?,
        );
    }
    eval_block(&function.body, &mut call_bindings, functions, depth + 1)
}

fn eval_block(
    block: &RirBlock,
    bindings: &mut HashMap<String, StaticValue>,
    functions: &HashMap<&str, &RirFunction>,
    depth: usize,
) -> Option<StaticValue> {
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                let value = eval_expr(value, bindings, functions, depth)?;
                bindings.insert(name.clone(), value);
            }
            RirStmt::Expr(_) => return None,
        }
    }
    block
        .tail
        .as_ref()
        .map(|tail| eval_expr(tail, bindings, functions, depth))
        .unwrap_or(Some(StaticValue::Unit))
}

pub(crate) fn resolve_path(
    path: &[String],
    bindings: &HashMap<String, StaticValue>,
) -> Option<StaticValue> {
    let (head, tail) = path.split_first()?;
    let mut value = bindings.get(head)?.clone();
    for segment in tail {
        let StaticValue::Record { fields, .. } = value else {
            return None;
        };
        value = fields
            .into_iter()
            .find_map(|(name, value)| (name == *segment).then_some(value))?;
    }
    Some(value)
}
