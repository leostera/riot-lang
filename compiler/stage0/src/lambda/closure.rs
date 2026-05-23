use std::collections::BTreeSet;

use super::ir::{
    BindingKey, Capture, LambdaBlock, LambdaExpr, LambdaPattern, LambdaProgram, LambdaStmt,
};

pub(crate) fn closure_convert_program(mut program: LambdaProgram) -> LambdaProgram {
    for function in &mut program.functions {
        closure_convert_block(&mut function.body);
    }
    program
}

fn closure_convert_block(block: &mut LambdaBlock) {
    for stmt in &mut block.statements {
        match stmt {
            LambdaStmt::Let { value, .. } | LambdaStmt::Expr(value) => closure_convert_expr(value),
        }
    }
    if let Some(tail) = &mut block.tail {
        closure_convert_expr(tail);
    }
}

fn closure_convert_expr(expr: &mut LambdaExpr) {
    match expr {
        LambdaExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            closure_convert_expr(condition);
            closure_convert_expr(then_branch);
            closure_convert_expr(else_branch);
        }
        LambdaExpr::Match { scrutinee, arms } => {
            closure_convert_expr(scrutinee);
            for arm in arms {
                closure_convert_expr(&mut arm.body);
            }
        }
        LambdaExpr::Block(block) => closure_convert_block(block),
        LambdaExpr::Call { args, .. } | LambdaExpr::Tuple(args) | LambdaExpr::List(args) => {
            for arg in args {
                closure_convert_expr(arg);
            }
        }
        LambdaExpr::Apply { callee, args, .. } => {
            closure_convert_expr(callee);
            for arg in args {
                closure_convert_expr(arg);
            }
        }
        LambdaExpr::Lambda {
            params,
            captures,
            body,
            ..
        } => {
            closure_convert_block(body);
            let bound = params
                .iter()
                .map(|param| param.key().clone())
                .collect::<BTreeSet<_>>();
            let mut free = BTreeSet::new();
            collect_free_block(body, &bound, &mut free);
            *captures = free.into_iter().map(Capture::from_key).collect();
        }
        LambdaExpr::Record { fields, .. } => {
            for (_, value) in fields {
                closure_convert_expr(value);
            }
        }
        LambdaExpr::Field { base, .. } | LambdaExpr::TupleIndex { base, .. } => {
            closure_convert_expr(base);
        }
        LambdaExpr::Spawn { body, .. } => closure_convert_block(body),
        LambdaExpr::Receive { arms } => {
            for arm in arms {
                closure_convert_expr(&mut arm.body);
            }
        }
        LambdaExpr::Variant { payload, .. } => {
            for value in payload {
                closure_convert_expr(value);
            }
        }
        LambdaExpr::Bool(_)
        | LambdaExpr::Unit
        | LambdaExpr::Char(_)
        | LambdaExpr::Float(_)
        | LambdaExpr::Int(_)
        | LambdaExpr::Path(_)
        | LambdaExpr::Local(_)
        | LambdaExpr::String(_) => {}
    }
}

pub(crate) fn collect_free_expr(
    expr: &LambdaExpr,
    bound: &BTreeSet<BindingKey>,
    free: &mut BTreeSet<BindingKey>,
) {
    match expr {
        LambdaExpr::Local(binding) => {
            if !bound.contains(binding) {
                free.insert(binding.clone());
            }
        }
        LambdaExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_free_expr(condition, bound, free);
            collect_free_expr(then_branch, bound, free);
            collect_free_expr(else_branch, bound, free);
        }
        LambdaExpr::Match { scrutinee, arms } => {
            collect_free_expr(scrutinee, bound, free);
            for arm in arms {
                let mut nested = bound.clone();
                bind_pattern_names(&arm.pattern, &mut nested);
                collect_free_expr(&arm.body, &nested, free);
            }
        }
        LambdaExpr::Block(block) => collect_free_block(block, bound, free),
        LambdaExpr::Call { args, .. } | LambdaExpr::Tuple(args) | LambdaExpr::List(args) => {
            for arg in args {
                collect_free_expr(arg, bound, free);
            }
        }
        LambdaExpr::Apply { callee, args, .. } => {
            collect_free_expr(callee, bound, free);
            for arg in args {
                collect_free_expr(arg, bound, free);
            }
        }
        LambdaExpr::Lambda { params, body, .. } => {
            let mut nested = bound.clone();
            for param in params {
                nested.insert(param.key().clone());
            }
            collect_free_block(body, &nested, free);
        }
        LambdaExpr::Record { fields, .. } => {
            for (_, value) in fields {
                collect_free_expr(value, bound, free);
            }
        }
        LambdaExpr::Field { base, .. } | LambdaExpr::TupleIndex { base, .. } => {
            collect_free_expr(base, bound, free)
        }
        LambdaExpr::Spawn { body, .. } => collect_free_block(body, bound, free),
        LambdaExpr::Receive { arms } => {
            for arm in arms {
                let mut nested = bound.clone();
                bind_pattern_names(&arm.pattern, &mut nested);
                collect_free_expr(&arm.body, &nested, free);
            }
        }
        LambdaExpr::Variant { payload, .. } => {
            for value in payload {
                collect_free_expr(value, bound, free);
            }
        }
        LambdaExpr::Bool(_)
        | LambdaExpr::Unit
        | LambdaExpr::Char(_)
        | LambdaExpr::Float(_)
        | LambdaExpr::Int(_)
        | LambdaExpr::Path(_)
        | LambdaExpr::String(_) => {}
    }
}

pub(crate) fn collect_free_block(
    block: &LambdaBlock,
    outer_bound: &BTreeSet<BindingKey>,
    free: &mut BTreeSet<BindingKey>,
) {
    let mut bound = outer_bound.clone();
    for stmt in &block.statements {
        match stmt {
            LambdaStmt::Let { name, value } => {
                collect_free_expr(value, &bound, free);
                bound.insert(name.clone());
            }
            LambdaStmt::Expr(expr) => collect_free_expr(expr, &bound, free),
        }
    }
    if let Some(tail) = &block.tail {
        collect_free_expr(tail, &bound, free);
    }
}

pub(crate) fn bind_pattern_names(pattern: &LambdaPattern, bound: &mut BTreeSet<BindingKey>) {
    match pattern {
        LambdaPattern::Bind { binding, .. } => {
            bound.insert(binding.clone());
        }
        LambdaPattern::Constructor { payload, .. } | LambdaPattern::Tuple(payload) => {
            for pattern in payload {
                bind_pattern_names(pattern, bound);
            }
        }
        LambdaPattern::List { prefix, tail } => {
            for pattern in prefix {
                bind_pattern_names(pattern, bound);
            }
            if let Some(tail) = tail {
                bind_pattern_names(tail, bound);
            }
        }
        LambdaPattern::Record { fields, .. } => {
            for (_, pattern) in fields {
                bind_pattern_names(pattern, bound);
            }
        }
        LambdaPattern::Wildcard
        | LambdaPattern::Unit
        | LambdaPattern::Bool(_)
        | LambdaPattern::Int(_)
        | LambdaPattern::String(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::ir::{LambdaFunction, LambdaMatchArm, LambdaReceiveArm, Param};
    use crate::signature::{ConstructorName, ModuleName, RsigType, TypeName};

    fn bind(name: &str, id: usize) -> BindingKey {
        BindingKey::resolved(name, id)
    }

    fn binding_pattern(name: &str, id: usize) -> LambdaPattern {
        LambdaPattern::Bind {
            binding: bind(name, id),
            type_: RsigType::Unknown,
        }
    }

    #[test]
    fn free_variable_collection_treats_match_pattern_binders_as_bound() {
        let outer = bind("outer", 0);
        let inner = bind("inner", 1);
        let expr = LambdaExpr::Match {
            scrutinee: Box::new(LambdaExpr::Local(outer.clone())),
            arms: vec![LambdaMatchArm {
                pattern: binding_pattern("inner", 1),
                body: LambdaExpr::Local(inner),
            }],
        };

        let mut free = BTreeSet::new();
        collect_free_expr(&expr, &BTreeSet::new(), &mut free);

        assert_eq!(free, BTreeSet::from([outer]));
    }

    #[test]
    fn free_variable_collection_treats_receive_pattern_binders_as_bound() {
        let msg = bind("msg", 0);
        let expr = LambdaExpr::Receive {
            arms: vec![LambdaReceiveArm {
                pattern: binding_pattern("msg", 0),
                body: LambdaExpr::Local(msg),
            }],
        };

        let mut free = BTreeSet::new();
        collect_free_expr(&expr, &BTreeSet::new(), &mut free);

        assert!(free.is_empty());
    }

    #[test]
    fn free_variable_collection_treats_nested_pattern_binders_as_bound() {
        let outer = bind("outer", 0);
        let tuple_item = bind("tuple_item", 1);
        let list_head = bind("list_head", 2);
        let record_field = bind("record_field", 3);
        let constructor_payload = bind("constructor_payload", 4);
        let list_tail = bind("list_tail", 5);
        let expr = LambdaExpr::Match {
            scrutinee: Box::new(LambdaExpr::Local(outer.clone())),
            arms: vec![LambdaMatchArm {
                pattern: LambdaPattern::Tuple(vec![
                    binding_pattern("tuple_item", 1),
                    LambdaPattern::List {
                        prefix: vec![binding_pattern("list_head", 2)],
                        tail: Some(Box::new(binding_pattern("list_tail", 5))),
                    },
                    LambdaPattern::Record {
                        type_name: TypeName::new("entry"),
                        fields: vec![("value".to_owned(), binding_pattern("record_field", 3))],
                    },
                    LambdaPattern::Constructor {
                        type_name: TypeName::new("option"),
                        constructor: ConstructorName::new("Some"),
                        payload: vec![binding_pattern("constructor_payload", 4)],
                    },
                ]),
                body: LambdaExpr::Tuple(vec![
                    LambdaExpr::Local(tuple_item),
                    LambdaExpr::Local(list_head),
                    LambdaExpr::Local(record_field),
                    LambdaExpr::Local(constructor_payload),
                    LambdaExpr::Local(list_tail),
                    LambdaExpr::Local(outer.clone()),
                ]),
            }],
        };

        let mut free = BTreeSet::new();
        collect_free_expr(&expr, &BTreeSet::new(), &mut free);

        assert_eq!(free, BTreeSet::from([outer]));
    }

    #[test]
    fn free_variable_collection_treats_let_binders_as_bound_after_initializers() {
        let value = bind("value", 0);
        let missing = bind("missing", 1);
        let after_binding = LambdaBlock {
            statements: vec![LambdaStmt::Let {
                name: value.clone(),
                value: LambdaExpr::Local(missing.clone()),
            }],
            tail: Some(LambdaExpr::Local(value.clone())),
        };

        let mut free = BTreeSet::new();
        collect_free_block(&after_binding, &BTreeSet::new(), &mut free);
        assert_eq!(free, BTreeSet::from([missing]));

        let initializer_before_binding = LambdaBlock {
            statements: vec![LambdaStmt::Let {
                name: value.clone(),
                value: LambdaExpr::Local(value.clone()),
            }],
            tail: Some(LambdaExpr::Unit),
        };

        let mut free = BTreeSet::new();
        collect_free_block(&initializer_before_binding, &BTreeSet::new(), &mut free);
        assert_eq!(free, BTreeSet::from([value]));
    }

    #[test]
    fn closure_conversion_captures_match_binders_for_nested_lambdas() {
        let outer = bind("outer", 0);
        let inner = bind("inner", 1);
        let mut expr = LambdaExpr::Match {
            scrutinee: Box::new(LambdaExpr::Local(outer.clone())),
            arms: vec![LambdaMatchArm {
                pattern: binding_pattern("inner", 1),
                body: LambdaExpr::Lambda {
                    params: vec![Param::from_key(bind("ignored", 2))],
                    param_types: vec![RsigType::Unit],
                    captures: Vec::new(),
                    body: Box::new(LambdaBlock {
                        statements: Vec::new(),
                        tail: Some(LambdaExpr::Tuple(vec![
                            LambdaExpr::Local(inner.clone()),
                            LambdaExpr::Local(outer.clone()),
                        ])),
                    }),
                },
            }],
        };

        closure_convert_expr(&mut expr);
        let LambdaExpr::Match { arms, .. } = expr else {
            panic!("expected match");
        };
        let LambdaExpr::Lambda { captures, .. } = &arms[0].body else {
            panic!("expected nested lambda arm body");
        };
        let capture_set = captures.iter().cloned().collect::<BTreeSet<_>>();
        assert_eq!(
            capture_set,
            BTreeSet::from([Capture::from_key(outer), Capture::from_key(inner)])
        );
    }

    #[test]
    fn closure_conversion_captures_receive_binders_for_nested_lambdas() {
        let msg = bind("msg", 0);
        let outer = bind("outer", 1);
        let mut expr = LambdaExpr::Receive {
            arms: vec![LambdaReceiveArm {
                pattern: binding_pattern("msg", 0),
                body: LambdaExpr::Lambda {
                    params: vec![Param::from_key(bind("ignored", 2))],
                    param_types: vec![RsigType::Unit],
                    captures: Vec::new(),
                    body: Box::new(LambdaBlock {
                        statements: Vec::new(),
                        tail: Some(LambdaExpr::Tuple(vec![
                            LambdaExpr::Local(msg.clone()),
                            LambdaExpr::Local(outer.clone()),
                        ])),
                    }),
                },
            }],
        };

        closure_convert_expr(&mut expr);
        let LambdaExpr::Receive { arms } = expr else {
            panic!("expected receive");
        };
        let LambdaExpr::Lambda { captures, .. } = &arms[0].body else {
            panic!("expected nested lambda arm body");
        };
        let capture_set = captures.iter().cloned().collect::<BTreeSet<_>>();
        assert_eq!(
            capture_set,
            BTreeSet::from([Capture::from_key(msg), Capture::from_key(outer)])
        );
    }

    #[test]
    fn closure_conversion_captures_outer_values_but_not_match_binders() {
        let outer = bind("outer", 0);
        let inner = bind("inner", 1);
        let mut program = LambdaProgram {
            module_name: ModuleName::new("ClosurePatternTest"),
            uses: Vec::new(),
            externals: Vec::new(),
            functions: vec![LambdaFunction {
                name: "main".to_owned(),
                params: Vec::new(),
                param_types: Vec::new(),
                result: RsigType::Unit,
                symbol: "riot_mod_ClosurePatternTest_main".to_owned(),
                body: LambdaBlock {
                    statements: Vec::new(),
                    tail: Some(LambdaExpr::Lambda {
                        params: vec![Param::from_key(bind("ignored", 2))],
                        param_types: vec![RsigType::Unit],
                        captures: Vec::new(),
                        body: Box::new(LambdaBlock {
                            statements: Vec::new(),
                            tail: Some(LambdaExpr::Match {
                                scrutinee: Box::new(LambdaExpr::Local(outer.clone())),
                                arms: vec![LambdaMatchArm {
                                    pattern: binding_pattern("inner", 1),
                                    body: LambdaExpr::Tuple(vec![
                                        LambdaExpr::Local(inner),
                                        LambdaExpr::Local(outer.clone()),
                                    ]),
                                }],
                            }),
                        }),
                    }),
                },
            }],
        };

        program = closure_convert_program(program);
        let Some(LambdaExpr::Lambda { captures, .. }) = &program.functions[0].body.tail else {
            panic!("expected function tail lambda");
        };

        assert_eq!(captures.as_slice(), &[Capture::from_key(outer)]);
    }
}
