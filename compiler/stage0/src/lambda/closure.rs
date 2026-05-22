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
