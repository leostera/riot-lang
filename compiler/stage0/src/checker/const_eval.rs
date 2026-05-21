use std::collections::HashMap;

use crate::ast::{AstBlock, AstExpr, AstPattern, AstStmt};

#[derive(Debug, Clone)]
pub(super) struct ConstFunction {
    pub(super) params: Vec<String>,
    pub(super) body: AstBlock,
}

#[derive(Debug, Clone, Default)]
pub(super) struct ConstFunctionTable {
    functions: HashMap<String, ConstFunction>,
}

impl ConstFunctionTable {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn insert(&mut self, name: impl Into<String>, function: ConstFunction) {
        self.functions.insert(name.into(), function);
    }

    fn get(&self, name: &str) -> Option<&ConstFunction> {
        self.functions.get(name)
    }

    pub(super) fn names(&self) -> impl Iterator<Item = &String> {
        self.functions.keys()
    }
}

#[derive(Debug, Clone, PartialEq)]
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

pub(super) fn resolve_const_value(
    expr: &AstExpr,
    bindings: &HashMap<String, ConstValue>,
    functions: &ConstFunctionTable,
) -> Option<ConstValue> {
    match expr {
        AstExpr::Add { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
        ) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs + rhs)),
            _ => None,
        },
        AstExpr::Sub { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
        ) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs - rhs)),
            _ => None,
        },
        AstExpr::Mul { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
        ) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs * rhs)),
            _ => None,
        },
        AstExpr::Div { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
        ) {
            (ConstValue::Int(_), ConstValue::Int(0)) => None,
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs / rhs)),
            _ => None,
        },
        AstExpr::Mod { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
        ) {
            (ConstValue::Int(_), ConstValue::Int(0)) => None,
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs % rhs)),
            _ => None,
        },
        AstExpr::Neg { expr, span: _ } => match resolve_const_value(expr, bindings, functions)? {
            ConstValue::Float(value) => Some(ConstValue::Float(format!("-{value}"))),
            ConstValue::Int(value) => Some(ConstValue::Int(-value)),
            _ => None,
        },
        AstExpr::Eq { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
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
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
        ) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Bool(lhs < rhs)),
            _ => None,
        },
        AstExpr::And { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
        ) {
            (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(ConstValue::Bool(lhs && rhs)),
            _ => None,
        },
        AstExpr::Or { lhs, rhs, span: _ } => match (
            resolve_const_value(lhs, bindings, functions)?,
            resolve_const_value(rhs, bindings, functions)?,
        ) {
            (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(ConstValue::Bool(lhs || rhs)),
            _ => None,
        },
        AstExpr::Not { expr, span: _ } => match resolve_const_value(expr, bindings, functions)? {
            ConstValue::Bool(value) => Some(ConstValue::Bool(!value)),
            _ => None,
        },
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            span: _,
        } => match resolve_const_value(condition, bindings, functions)? {
            ConstValue::Bool(true) => resolve_const_value(then_branch, bindings, functions),
            ConstValue::Bool(false) => resolve_const_value(else_branch, bindings, functions),
            _ => None,
        },
        AstExpr::Match {
            scrutinee,
            arms,
            span: _,
        } => {
            let scrutinee = resolve_const_value(scrutinee, bindings, functions)?;
            for arm in arms {
                if const_pattern_matches(&arm.pattern, &scrutinee) {
                    let mut arm_bindings = bindings.clone();
                    bind_const_pattern(&arm.pattern, &scrutinee, &mut arm_bindings);
                    return resolve_const_value(&arm.body, &arm_bindings, functions);
                }
            }
            None
        }
        AstExpr::Block { block, span: _ } => resolve_const_block(block, bindings, functions),
        AstExpr::Bool { value, span: _ } => Some(ConstValue::Bool(*value)),
        AstExpr::Char { value, span: _ } => Some(ConstValue::Char(*value)),
        AstExpr::Unit { span: _ } => Some(ConstValue::Unit),
        AstExpr::Tuple { items, span: _ } => items
            .iter()
            .map(|item| resolve_const_value(item, bindings, functions))
            .collect::<Option<Vec<_>>>()
            .map(ConstValue::Tuple),
        AstExpr::List { items, span: _ } => items
            .iter()
            .map(|item| resolve_const_value(item, bindings, functions))
            .collect::<Option<Vec<_>>>()
            .map(ConstValue::List),
        AstExpr::Record {
            path,
            fields,
            span: _,
        } => fields
            .iter()
            .map(|(name, value)| {
                Some((
                    name.clone(),
                    resolve_const_value(value, bindings, functions)?,
                ))
            })
            .collect::<Option<Vec<_>>>()
            .map(|fields| ConstValue::Record {
                path: path.segments.join("."),
                fields,
            }),
        AstExpr::Float { value, span: _ } => Some(ConstValue::Float(value.clone())),
        AstExpr::Int { value, span: _ } => Some(ConstValue::Int(*value)),
        AstExpr::String { value, span: _ } => Some(ConstValue::String(value.clone())),
        AstExpr::Path { path, span: _ } => resolve_path_value(path.segments.as_slice(), bindings),
        AstExpr::Field { base, field, .. } => match resolve_const_value(base, bindings, functions)?
        {
            ConstValue::Record { fields, .. } => fields
                .into_iter()
                .find_map(|(name, value)| (name == *field).then_some(value)),
            _ => None,
        },
        AstExpr::TupleIndex { base, index, .. } => {
            match resolve_const_value(base, bindings, functions)? {
                ConstValue::Tuple(items) => items.get(*index).cloned(),
                _ => None,
            }
        }
        AstExpr::Call {
            callee,
            args,
            span: _,
        } => resolve_call_value(
            callee.segments.as_slice(),
            args.as_slice(),
            bindings,
            functions,
        ),
        AstExpr::Apply { .. }
        | AstExpr::Lambda { .. }
        | AstExpr::Spawn { .. }
        | AstExpr::Receive { .. } => None,
    }
}

fn resolve_const_block(
    block: &AstBlock,
    bindings: &HashMap<String, ConstValue>,
    functions: &ConstFunctionTable,
) -> Option<ConstValue> {
    let mut bindings = bindings.clone();
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let { name, value, .. } => {
                let value = resolve_const_value(value, &bindings, functions)?;
                bindings.insert(name.clone(), value);
            }
            AstStmt::Expr(expr) => {
                resolve_const_value(expr, &bindings, functions)?;
            }
        }
    }
    if let Some(tail) = &block.tail {
        resolve_const_value(tail, &bindings, functions)
    } else {
        Some(ConstValue::Unit)
    }
}

fn resolve_path_value(
    segments: &[String],
    bindings: &HashMap<String, ConstValue>,
) -> Option<ConstValue> {
    let (head, tail) = segments.split_first()?;
    let mut value = bindings.get(head)?.clone();

    for segment in tail {
        let ConstValue::Record { path: _, fields } = value else {
            return None;
        };
        value = fields
            .into_iter()
            .find_map(|(name, value)| (name == *segment).then_some(value))?;
    }

    Some(value)
}

fn const_pattern_matches(pattern: &AstPattern, value: &ConstValue) -> bool {
    match pattern {
        AstPattern::Wildcard { .. } | AstPattern::Bind { .. } => true,
        AstPattern::Constructor { .. } => false,
        AstPattern::Tuple { items, .. } => {
            let ConstValue::Tuple(values) = value else {
                return false;
            };
            items.len() == values.len()
                && items
                    .iter()
                    .zip(values)
                    .all(|(pattern, value)| const_pattern_matches(pattern, value))
        }
        AstPattern::List { prefix, tail, .. } => {
            let ConstValue::List(values) = value else {
                return false;
            };
            if tail.is_none() && prefix.len() != values.len() {
                return false;
            }
            if tail.is_some() && prefix.len() > values.len() {
                return false;
            }
            prefix
                .iter()
                .zip(values)
                .all(|(pattern, value)| const_pattern_matches(pattern, value))
                && tail.as_ref().is_none_or(|tail| {
                    const_pattern_matches(tail, &ConstValue::List(values[prefix.len()..].to_vec()))
                })
        }
        AstPattern::Record { path, fields, .. } => {
            let ConstValue::Record {
                path: actual_path,
                fields: values,
            } = value
            else {
                return false;
            };
            actual_path == &path.segments.join(".")
                && fields.iter().all(|(field, pattern)| {
                    values
                        .iter()
                        .find_map(|(name, value)| (name == field).then_some(value))
                        .is_some_and(|value| const_pattern_matches(pattern, value))
                })
        }
        AstPattern::Unit { .. } => matches!(value, ConstValue::Unit),
        AstPattern::Bool {
            value: expected, ..
        } => {
            matches!(value, ConstValue::Bool(actual) if actual == expected)
        }
        AstPattern::Int {
            value: expected, ..
        } => {
            matches!(value, ConstValue::Int(actual) if actual == expected)
        }
        AstPattern::String {
            value: expected, ..
        } => {
            matches!(value, ConstValue::String(actual) if actual == expected)
        }
    }
}

fn bind_const_pattern(
    pattern: &AstPattern,
    value: &ConstValue,
    bindings: &mut HashMap<String, ConstValue>,
) {
    match pattern {
        AstPattern::Bind { name, .. } => {
            bindings.insert(name.clone(), value.clone());
        }
        AstPattern::Tuple { items, .. } => {
            let ConstValue::Tuple(values) = value else {
                return;
            };
            for (pattern, value) in items.iter().zip(values) {
                bind_const_pattern(pattern, value, bindings);
            }
        }
        AstPattern::List { prefix, tail, .. } => {
            let ConstValue::List(values) = value else {
                return;
            };
            for (pattern, value) in prefix.iter().zip(values) {
                bind_const_pattern(pattern, value, bindings);
            }
            if let Some(tail) = tail {
                bind_const_pattern(
                    tail,
                    &ConstValue::List(values[prefix.len()..].to_vec()),
                    bindings,
                );
            }
        }
        AstPattern::Record { fields, .. } => {
            let ConstValue::Record {
                path: _,
                fields: values,
            } = value
            else {
                return;
            };
            for (field, pattern) in fields {
                if let Some(value) = values
                    .iter()
                    .find_map(|(name, value)| (name == field).then_some(value))
                {
                    bind_const_pattern(pattern, value, bindings);
                }
            }
        }
        AstPattern::Wildcard { .. }
        | AstPattern::Constructor { .. }
        | AstPattern::Unit { .. }
        | AstPattern::Bool { .. }
        | AstPattern::Int { .. }
        | AstPattern::String { .. } => {}
    }
}

fn resolve_call_value(
    callee: &[String],
    args: &[AstExpr],
    bindings: &HashMap<String, ConstValue>,
    functions: &ConstFunctionTable,
) -> Option<ConstValue> {
    let [name] = callee else {
        return None;
    };
    let function = functions.get(name)?;
    if function.params.len() != args.len() || !function.body.statements.is_empty() {
        return None;
    }

    let mut call_bindings = HashMap::new();
    for (param, arg) in function.params.iter().zip(args) {
        let value = resolve_const_value(arg, bindings, functions)?;
        call_bindings.insert(param.clone(), value);
    }

    let tail = function.body.tail.as_ref()?;
    resolve_const_value(tail, &call_bindings, functions)
}
