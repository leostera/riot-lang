use std::collections::HashMap;

use crate::lambda::ir::{RirBlock, RirExpr, RirFunction, RirPattern, RirStmt};
use crate::signature::{ConstructorName, TypeName};

#[derive(Debug, Clone, PartialEq)]
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
    Variant {
        type_name: TypeName,
        constructor: ConstructorName,
        payload: Box<StaticValue>,
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
            StaticValue::Variant {
                constructor,
                payload,
                ..
            } if matches!(payload.as_ref(), StaticValue::Unit) => constructor.to_string(),
            StaticValue::Variant {
                constructor,
                payload,
                ..
            } if matches!(payload.as_ref(), StaticValue::Tuple(_)) => {
                format!("{}{}", constructor, payload.to_print_string())
            }
            StaticValue::Variant {
                constructor,
                payload,
                ..
            } => format!("{}({})", constructor, payload.to_print_string()),
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct StaticEvaluator<'a> {
    functions: &'a HashMap<&'a str, &'a RirFunction>,
}

impl<'a> StaticEvaluator<'a> {
    pub(crate) fn new(functions: &'a HashMap<&'a str, &'a RirFunction>) -> Self {
        Self { functions }
    }

    pub(crate) fn eval_expr(
        &self,
        expr: &RirExpr,
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        eval_expr(expr, bindings, self.functions, 0)
    }

    pub(crate) fn eval_call(
        &self,
        name: &str,
        args: &[RirExpr],
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        eval_call(name, args, bindings, self.functions, 0)
    }

    pub(crate) fn resolve_path(
        &self,
        path: &[String],
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        resolve_path(path, bindings)
    }
}

fn eval_expr(
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
        RirExpr::Match { scrutinee, arms } => {
            let scrutinee = eval_expr(scrutinee, bindings, functions, depth)?;
            for arm in arms {
                if static_pattern_matches(&arm.pattern, &scrutinee) {
                    let mut arm_bindings = bindings.clone();
                    bind_static_pattern(&arm.pattern, scrutinee.clone(), &mut arm_bindings);
                    return eval_expr(&arm.body, &arm_bindings, functions, depth);
                }
            }
            None
        }
        RirExpr::Block(block) => {
            let mut block_bindings = bindings.clone();
            eval_block(block, &mut block_bindings, functions, depth)
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
        RirExpr::Variant {
            type_name,
            constructor,
            payload,
        } => Some(StaticValue::Variant {
            type_name: type_name.clone(),
            constructor: constructor.clone(),
            payload: Box::new(match payload.as_slice() {
                [] => StaticValue::Unit,
                [value] => eval_expr(value, bindings, functions, depth)?,
                values => StaticValue::Tuple(
                    values
                        .iter()
                        .map(|value| eval_expr(value, bindings, functions, depth))
                        .collect::<Option<Vec<_>>>()?,
                ),
            }),
        }),
        RirExpr::Field { .. } | RirExpr::TupleIndex { .. } => None,
        RirExpr::Char(value) => Some(StaticValue::Char(*value)),
        RirExpr::Float(value) => Some(StaticValue::Float(value.clone())),
        RirExpr::Int(value) => Some(StaticValue::Int(*value)),
        RirExpr::Path(path) => resolve_path(path.as_slice(), bindings),
        RirExpr::String(value) => Some(StaticValue::String(value.clone())),
        RirExpr::Apply { .. }
        | RirExpr::Lambda { .. }
        | RirExpr::Spawn { .. }
        | RirExpr::Receive { .. } => None,
    }
}

fn static_pattern_matches(pattern: &RirPattern, value: &StaticValue) -> bool {
    match pattern {
        RirPattern::Wildcard | RirPattern::Bind { .. } => true,
        RirPattern::Unit => matches!(value, StaticValue::Unit),
        RirPattern::Bool(expected) => {
            matches!(value, StaticValue::Bool(actual) if actual == expected)
        }
        RirPattern::Int(expected) => {
            matches!(value, StaticValue::Int(actual) if actual == expected)
        }
        RirPattern::String(expected) => {
            matches!(value, StaticValue::String(actual) if actual == expected)
        }
        RirPattern::Constructor {
            type_name,
            constructor,
            payload,
        } => {
            let StaticValue::Variant {
                type_name: actual_type,
                constructor: actual_constructor,
                payload: actual_payload,
            } = value
            else {
                return false;
            };
            actual_type == type_name
                && actual_constructor == constructor
                && static_payload_patterns_match(payload, actual_payload)
        }
        RirPattern::Tuple(patterns) => {
            let StaticValue::Tuple(items) = value else {
                return false;
            };
            patterns.len() == items.len()
                && patterns
                    .iter()
                    .zip(items)
                    .all(|(pattern, item)| static_pattern_matches(pattern, item))
        }
        RirPattern::List { prefix, tail } => {
            let StaticValue::List(items) = value else {
                return false;
            };
            if tail.is_none() && prefix.len() != items.len() {
                return false;
            }
            if tail.is_some() && prefix.len() > items.len() {
                return false;
            }
            prefix
                .iter()
                .zip(items)
                .all(|(pattern, item)| static_pattern_matches(pattern, item))
                && tail.as_ref().is_none_or(|tail| {
                    static_pattern_matches(tail, &StaticValue::List(items[prefix.len()..].to_vec()))
                })
        }
        RirPattern::Record { type_name, fields } => {
            let StaticValue::Record {
                path,
                fields: values,
            } = value
            else {
                return false;
            };
            path == type_name.as_str()
                && fields.iter().all(|(field, pattern)| {
                    values
                        .iter()
                        .find_map(|(name, value)| (name == field).then_some(value))
                        .is_some_and(|value| static_pattern_matches(pattern, value))
                })
        }
    }
}

fn static_payload_patterns_match(patterns: &[RirPattern], payload: &StaticValue) -> bool {
    match patterns {
        [] => matches!(payload, StaticValue::Unit),
        [pattern] => static_pattern_matches(pattern, payload),
        patterns => {
            let StaticValue::Tuple(items) = payload else {
                return false;
            };
            patterns.len() == items.len()
                && patterns
                    .iter()
                    .zip(items)
                    .all(|(pattern, item)| static_pattern_matches(pattern, item))
        }
    }
}

fn bind_static_pattern(
    pattern: &RirPattern,
    value: StaticValue,
    bindings: &mut HashMap<String, StaticValue>,
) {
    match pattern {
        RirPattern::Bind { binding, .. } => {
            bindings.insert(binding.as_str().to_owned(), value);
        }
        RirPattern::Constructor { payload, .. } => match payload.as_slice() {
            [] => {}
            [pattern] => {
                let StaticValue::Variant {
                    payload: actual_payload,
                    ..
                } = value
                else {
                    return;
                };
                bind_static_pattern(pattern, *actual_payload, bindings);
            }
            patterns => {
                let StaticValue::Variant {
                    payload: actual_payload,
                    ..
                } = value
                else {
                    return;
                };
                let StaticValue::Tuple(items) = *actual_payload else {
                    return;
                };
                for (pattern, item) in patterns.iter().zip(items) {
                    bind_static_pattern(pattern, item, bindings);
                }
            }
        },
        RirPattern::Tuple(patterns) => {
            let StaticValue::Tuple(items) = value else {
                return;
            };
            for (pattern, item) in patterns.iter().zip(items) {
                bind_static_pattern(pattern, item, bindings);
            }
        }
        RirPattern::List { prefix, tail } => {
            let StaticValue::List(items) = value else {
                return;
            };
            for (pattern, item) in prefix.iter().zip(&items) {
                bind_static_pattern(pattern, item.clone(), bindings);
            }
            if let Some(tail) = tail {
                bind_static_pattern(
                    tail,
                    StaticValue::List(items[prefix.len()..].to_vec()),
                    bindings,
                );
            }
        }
        RirPattern::Record { fields, .. } => {
            let StaticValue::Record {
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
                    bind_static_pattern(pattern, value.clone(), bindings);
                }
            }
        }
        RirPattern::Wildcard
        | RirPattern::Unit
        | RirPattern::Bool(_)
        | RirPattern::Int(_)
        | RirPattern::String(_) => {}
    }
}

fn eval_call(
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
                bindings.insert(name.as_str().to_owned(), value);
            }
            RirStmt::Expr(expr) => {
                eval_expr(expr, bindings, functions, depth)?;
            }
        }
    }
    block
        .tail
        .as_ref()
        .map(|tail| eval_expr(tail, bindings, functions, depth))
        .unwrap_or(Some(StaticValue::Unit))
}

fn resolve_path(path: &[String], bindings: &HashMap<String, StaticValue>) -> Option<StaticValue> {
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
