use std::collections::{BTreeMap, HashMap};

use crate::lambda::ir::{
    LambdaBlock, LambdaExpr, LambdaExternal, LambdaFunction, LambdaPattern, LambdaStmt,
};
use crate::signature::{ConstructorName, TypeName};
use crate::stdlib::Stdlib;

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
    functions: &'a HashMap<&'a str, &'a LambdaFunction>,
    externals: &'a BTreeMap<String, LambdaExternal>,
}

impl<'a> StaticEvaluator<'a> {
    pub(crate) fn new(
        functions: &'a HashMap<&'a str, &'a LambdaFunction>,
        externals: &'a BTreeMap<String, LambdaExternal>,
    ) -> Self {
        Self {
            functions,
            externals,
        }
    }

    pub(crate) fn eval_expr(
        &self,
        expr: &LambdaExpr,
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        eval_expr(expr, bindings, self.functions, self.externals, 0)
    }

    pub(crate) fn eval_call(
        &self,
        name: &str,
        args: &[LambdaExpr],
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        eval_call(name, args, bindings, self.functions, self.externals, 0)
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
    expr: &LambdaExpr,
    bindings: &HashMap<String, StaticValue>,
    functions: &HashMap<&str, &LambdaFunction>,
    externals: &BTreeMap<String, LambdaExternal>,
    depth: usize,
) -> Option<StaticValue> {
    if depth > 64 {
        return None;
    }
    match expr {
        LambdaExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            if eval_expr(condition, bindings, functions, externals, depth)?.as_bool()? {
                eval_expr(then_branch, bindings, functions, externals, depth)
            } else {
                eval_expr(else_branch, bindings, functions, externals, depth)
            }
        }
        LambdaExpr::Match { scrutinee, arms } => {
            let scrutinee = eval_expr(scrutinee, bindings, functions, externals, depth)?;
            for arm in arms {
                if static_pattern_matches(&arm.pattern, &scrutinee) {
                    let mut arm_bindings = bindings.clone();
                    bind_static_pattern(&arm.pattern, scrutinee.clone(), &mut arm_bindings);
                    return eval_expr(&arm.body, &arm_bindings, functions, externals, depth);
                }
            }
            None
        }
        LambdaExpr::Block(block) => {
            let mut block_bindings = bindings.clone();
            eval_block(block, &mut block_bindings, functions, externals, depth)
        }
        LambdaExpr::Bool(value) => Some(StaticValue::Bool(*value)),
        LambdaExpr::Call { callee, args, .. } => {
            let name = Stdlib::prelude_member_name(callee)?;
            eval_call(name, args, bindings, functions, externals, depth)
        }
        LambdaExpr::Unit => Some(StaticValue::Unit),
        LambdaExpr::Tuple(items) => items
            .iter()
            .map(|item| eval_expr(item, bindings, functions, externals, depth))
            .collect::<Option<Vec<_>>>()
            .map(StaticValue::Tuple),
        LambdaExpr::List(items) => items
            .iter()
            .map(|item| eval_expr(item, bindings, functions, externals, depth))
            .collect::<Option<Vec<_>>>()
            .map(StaticValue::List),
        LambdaExpr::Record { path, fields } => fields
            .iter()
            .map(|(name, value)| {
                Some((
                    name.clone(),
                    eval_expr(value, bindings, functions, externals, depth)?,
                ))
            })
            .collect::<Option<Vec<_>>>()
            .map(|fields| StaticValue::Record {
                path: path.join("."),
                fields,
            }),
        LambdaExpr::Variant {
            type_name,
            constructor,
            payload,
        } => Some(StaticValue::Variant {
            type_name: type_name.clone(),
            constructor: constructor.clone(),
            payload: Box::new(match payload.as_slice() {
                [] => StaticValue::Unit,
                [value] => eval_expr(value, bindings, functions, externals, depth)?,
                values => StaticValue::Tuple(
                    values
                        .iter()
                        .map(|value| eval_expr(value, bindings, functions, externals, depth))
                        .collect::<Option<Vec<_>>>()?,
                ),
            }),
        }),
        LambdaExpr::Field { .. } | LambdaExpr::TupleIndex { .. } => None,
        LambdaExpr::Char(value) => Some(StaticValue::Char(*value)),
        LambdaExpr::Float(value) => Some(StaticValue::Float(value.clone())),
        LambdaExpr::Int(value) => Some(StaticValue::Int(*value)),
        LambdaExpr::Path(path) => resolve_path(path.as_slice(), bindings),
        LambdaExpr::Local(binding) => bindings.get(binding.as_str()).cloned(),
        LambdaExpr::String(value) => Some(StaticValue::String(value.clone())),
        LambdaExpr::Apply { .. }
        | LambdaExpr::Lambda { .. }
        | LambdaExpr::Spawn { .. }
        | LambdaExpr::Receive { .. } => None,
    }
}

fn static_pattern_matches(pattern: &LambdaPattern, value: &StaticValue) -> bool {
    match pattern {
        LambdaPattern::Wildcard | LambdaPattern::Bind { .. } => true,
        LambdaPattern::Unit => matches!(value, StaticValue::Unit),
        LambdaPattern::Bool(expected) => {
            matches!(value, StaticValue::Bool(actual) if actual == expected)
        }
        LambdaPattern::Int(expected) => {
            matches!(value, StaticValue::Int(actual) if actual == expected)
        }
        LambdaPattern::String(expected) => {
            matches!(value, StaticValue::String(actual) if actual == expected)
        }
        LambdaPattern::Constructor {
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
        LambdaPattern::Tuple(patterns) => {
            let StaticValue::Tuple(items) = value else {
                return false;
            };
            patterns.len() == items.len()
                && patterns
                    .iter()
                    .zip(items)
                    .all(|(pattern, item)| static_pattern_matches(pattern, item))
        }
        LambdaPattern::List { prefix, tail } => {
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
        LambdaPattern::Record { type_name, fields } => {
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

fn static_payload_patterns_match(patterns: &[LambdaPattern], payload: &StaticValue) -> bool {
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
    pattern: &LambdaPattern,
    value: StaticValue,
    bindings: &mut HashMap<String, StaticValue>,
) {
    match pattern {
        LambdaPattern::Bind { binding, .. } => {
            bindings.insert(binding.as_str().to_owned(), value);
        }
        LambdaPattern::Constructor { payload, .. } => match payload.as_slice() {
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
        LambdaPattern::Tuple(patterns) => {
            let StaticValue::Tuple(items) = value else {
                return;
            };
            for (pattern, item) in patterns.iter().zip(items) {
                bind_static_pattern(pattern, item, bindings);
            }
        }
        LambdaPattern::List { prefix, tail } => {
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
        LambdaPattern::Record { fields, .. } => {
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
        LambdaPattern::Wildcard
        | LambdaPattern::Unit
        | LambdaPattern::Bool(_)
        | LambdaPattern::Int(_)
        | LambdaPattern::String(_) => {}
    }
}

fn eval_call(
    name: &str,
    args: &[LambdaExpr],
    bindings: &HashMap<String, StaticValue>,
    functions: &HashMap<&str, &LambdaFunction>,
    externals: &BTreeMap<String, LambdaExternal>,
    depth: usize,
) -> Option<StaticValue> {
    if let Some(external) = externals.get(name) {
        return eval_external_call(&external.abi, args, bindings, functions, externals, depth);
    }

    let function = functions.get(name)?;
    if function.params.len() != args.len() {
        return None;
    }
    let mut call_bindings = HashMap::new();
    for (param, arg) in function.params.iter().zip(args) {
        call_bindings.insert(
            param.as_str().to_owned(),
            eval_expr(arg, bindings, functions, externals, depth + 1)?,
        );
    }
    eval_block(
        &function.body,
        &mut call_bindings,
        functions,
        externals,
        depth + 1,
    )
}

fn eval_external_call(
    abi: &str,
    args: &[LambdaExpr],
    bindings: &HashMap<String, StaticValue>,
    functions: &HashMap<&str, &LambdaFunction>,
    externals: &BTreeMap<String, LambdaExternal>,
    depth: usize,
) -> Option<StaticValue> {
    let eval_arg =
        |index: usize| eval_expr(args.get(index)?, bindings, functions, externals, depth + 1);
    match (abi, args.len()) {
        ("riot_rt_prim_add", 2) => {
            return Some(StaticValue::Int(
                eval_arg(0)?.as_int()? + eval_arg(1)?.as_int()?,
            ));
        }
        ("riot_rt_prim_sub", 2) => {
            return Some(StaticValue::Int(
                eval_arg(0)?.as_int()? - eval_arg(1)?.as_int()?,
            ));
        }
        ("riot_rt_prim_neg", 1) => {
            return match eval_arg(0)? {
                StaticValue::Float(value) => Some(StaticValue::Float(format!("-{value}"))),
                StaticValue::Int(value) => Some(StaticValue::Int(-value)),
                _ => None,
            };
        }
        ("riot_rt_prim_mul", 2) => {
            return Some(StaticValue::Int(
                eval_arg(0)?.as_int()? * eval_arg(1)?.as_int()?,
            ));
        }
        ("riot_rt_prim_div", 2) => {
            let rhs = eval_arg(1)?.as_int()?;
            if rhs == 0 {
                return None;
            }
            return Some(StaticValue::Int(eval_arg(0)?.as_int()? / rhs));
        }
        ("riot_rt_prim_mod", 2) => {
            let rhs = eval_arg(1)?.as_int()?;
            if rhs == 0 {
                return None;
            }
            return Some(StaticValue::Int(eval_arg(0)?.as_int()? % rhs));
        }
        ("riot_rt_prim_eq", 2) => {
            return Some(StaticValue::Bool(eval_arg(0)? == eval_arg(1)?));
        }
        ("riot_rt_prim_lt", 2) => {
            return Some(StaticValue::Bool(
                eval_arg(0)?.as_int()? < eval_arg(1)?.as_int()?,
            ));
        }
        ("riot_rt_prim_and", 2) => {
            return Some(StaticValue::Bool(
                eval_arg(0)?.as_bool()? && eval_arg(1)?.as_bool()?,
            ));
        }
        ("riot_rt_prim_or", 2) => {
            return Some(StaticValue::Bool(
                eval_arg(0)?.as_bool()? || eval_arg(1)?.as_bool()?,
            ));
        }
        ("riot_rt_prim_not", 1) => return Some(StaticValue::Bool(!eval_arg(0)?.as_bool()?)),
        _ => {}
    }
    None
}

fn eval_block(
    block: &LambdaBlock,
    bindings: &mut HashMap<String, StaticValue>,
    functions: &HashMap<&str, &LambdaFunction>,
    externals: &BTreeMap<String, LambdaExternal>,
    depth: usize,
) -> Option<StaticValue> {
    for stmt in &block.statements {
        match stmt {
            LambdaStmt::Let { name, value } => {
                let value = eval_expr(value, bindings, functions, externals, depth)?;
                bindings.insert(name.as_str().to_owned(), value);
            }
            LambdaStmt::Expr(expr) => {
                eval_expr(expr, bindings, functions, externals, depth)?;
            }
        }
    }
    block
        .tail
        .as_ref()
        .map(|tail| eval_expr(tail, bindings, functions, externals, depth))
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

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};

    use crate::lambda::ir::{LambdaExpr, LambdaExternal};
    use crate::signature::RsigType;

    use super::{StaticEvaluator, StaticValue};

    #[test]
    fn static_equality_is_structural_not_rendered_text() {
        let functions = HashMap::new();
        let externals = BTreeMap::from([(
            "(==)".to_owned(),
            LambdaExternal {
                name: "(==)".to_owned(),
                params: vec![RsigType::Var("'a".into()), RsigType::Var("'a".into())],
                result: RsigType::Bool,
                abi: "riot_rt_prim_eq".to_owned(),
            },
        )]);
        let evaluator = StaticEvaluator::new(&functions, &externals);

        let result = evaluator.eval_call(
            "(==)",
            &[LambdaExpr::String("1".to_owned()), LambdaExpr::Int(1)],
            &HashMap::new(),
        );

        assert_eq!(result, Some(StaticValue::Bool(false)));
    }
}
