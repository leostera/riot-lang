use std::collections::BTreeSet;

use super::ir::{Capture, RirBlock, RirExpr, RirPattern, RirProgram, RirStmt};

pub(crate) fn closure_convert_program(mut program: RirProgram) -> RirProgram {
    for function in &mut program.functions {
        closure_convert_block(&mut function.body);
    }
    program
}

fn closure_convert_block(block: &mut RirBlock) {
    for stmt in &mut block.statements {
        match stmt {
            RirStmt::Let { value, .. } | RirStmt::Expr(value) => closure_convert_expr(value),
        }
    }
    if let Some(tail) = &mut block.tail {
        closure_convert_expr(tail);
    }
}

fn closure_convert_expr(expr: &mut RirExpr) {
    match expr {
        RirExpr::Add(lhs, rhs)
        | RirExpr::Sub(lhs, rhs)
        | RirExpr::Mul(lhs, rhs)
        | RirExpr::Div(lhs, rhs)
        | RirExpr::Mod(lhs, rhs)
        | RirExpr::Eq(lhs, rhs)
        | RirExpr::Lt(lhs, rhs)
        | RirExpr::And(lhs, rhs)
        | RirExpr::Or(lhs, rhs) => {
            closure_convert_expr(lhs);
            closure_convert_expr(rhs);
        }
        RirExpr::Neg(value) | RirExpr::Not(value) => closure_convert_expr(value),
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            closure_convert_expr(condition);
            closure_convert_expr(then_branch);
            closure_convert_expr(else_branch);
        }
        RirExpr::Match { scrutinee, arms } => {
            closure_convert_expr(scrutinee);
            for arm in arms {
                closure_convert_expr(&mut arm.body);
            }
        }
        RirExpr::Block(block) => closure_convert_block(block),
        RirExpr::Call { args, .. } | RirExpr::Tuple(args) | RirExpr::List(args) => {
            for arg in args {
                closure_convert_expr(arg);
            }
        }
        RirExpr::Apply { callee, args, .. } => {
            closure_convert_expr(callee);
            for arg in args {
                closure_convert_expr(arg);
            }
        }
        RirExpr::Lambda {
            params,
            captures,
            body,
        } => {
            closure_convert_block(body);
            let bound = params
                .iter()
                .map(|param| param.as_str().to_owned())
                .collect::<BTreeSet<_>>();
            let mut free = BTreeSet::new();
            collect_free_block(body, &bound, &mut free);
            *captures = free.into_iter().map(Capture::new).collect();
        }
        RirExpr::Record { fields, .. } => {
            for (_, value) in fields {
                closure_convert_expr(value);
            }
        }
        RirExpr::Field { base, .. } | RirExpr::TupleIndex { base, .. } => {
            closure_convert_expr(base);
        }
        RirExpr::Spawn { body, .. } => closure_convert_block(body),
        RirExpr::Receive { arms } => {
            for arm in arms {
                closure_convert_expr(&mut arm.body);
            }
        }
        RirExpr::Variant { payload, .. } => {
            for value in payload {
                closure_convert_expr(value);
            }
        }
        RirExpr::Bool(_)
        | RirExpr::Unit
        | RirExpr::Char(_)
        | RirExpr::Float(_)
        | RirExpr::Int(_)
        | RirExpr::Path(_)
        | RirExpr::String(_) => {}
    }
}

pub(crate) fn collect_free_expr(
    expr: &RirExpr,
    bound: &BTreeSet<String>,
    free: &mut BTreeSet<String>,
) {
    match expr {
        RirExpr::Path(path) => {
            if let [name] = path.as_slice()
                && !bound.contains(name)
            {
                free.insert(name.clone());
            }
        }
        RirExpr::Add(lhs, rhs)
        | RirExpr::Sub(lhs, rhs)
        | RirExpr::Mul(lhs, rhs)
        | RirExpr::Div(lhs, rhs)
        | RirExpr::Mod(lhs, rhs)
        | RirExpr::Eq(lhs, rhs)
        | RirExpr::Lt(lhs, rhs)
        | RirExpr::And(lhs, rhs)
        | RirExpr::Or(lhs, rhs) => {
            collect_free_expr(lhs, bound, free);
            collect_free_expr(rhs, bound, free);
        }
        RirExpr::Neg(value) | RirExpr::Not(value) => collect_free_expr(value, bound, free),
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_free_expr(condition, bound, free);
            collect_free_expr(then_branch, bound, free);
            collect_free_expr(else_branch, bound, free);
        }
        RirExpr::Match { scrutinee, arms } => {
            collect_free_expr(scrutinee, bound, free);
            for arm in arms {
                let mut nested = bound.clone();
                bind_pattern_names(&arm.pattern, &mut nested);
                collect_free_expr(&arm.body, &nested, free);
            }
        }
        RirExpr::Block(block) => collect_free_block(block, bound, free),
        RirExpr::Call { args, .. } | RirExpr::Tuple(args) | RirExpr::List(args) => {
            for arg in args {
                collect_free_expr(arg, bound, free);
            }
        }
        RirExpr::Apply { callee, args, .. } => {
            collect_free_expr(callee, bound, free);
            for arg in args {
                collect_free_expr(arg, bound, free);
            }
        }
        RirExpr::Lambda { params, body, .. } => {
            let mut nested = bound.clone();
            for param in params {
                nested.insert(param.as_str().to_owned());
            }
            collect_free_block(body, &nested, free);
        }
        RirExpr::Record { fields, .. } => {
            for (_, value) in fields {
                collect_free_expr(value, bound, free);
            }
        }
        RirExpr::Field { base, .. } | RirExpr::TupleIndex { base, .. } => {
            collect_free_expr(base, bound, free)
        }
        RirExpr::Spawn { body, .. } => collect_free_block(body, bound, free),
        RirExpr::Receive { arms } => {
            for arm in arms {
                let mut nested = bound.clone();
                bind_pattern_names(&arm.pattern, &mut nested);
                collect_free_expr(&arm.body, &nested, free);
            }
        }
        RirExpr::Variant { payload, .. } => {
            for value in payload {
                collect_free_expr(value, bound, free);
            }
        }
        RirExpr::Bool(_)
        | RirExpr::Unit
        | RirExpr::Char(_)
        | RirExpr::Float(_)
        | RirExpr::Int(_)
        | RirExpr::String(_) => {}
    }
}

pub(crate) fn collect_free_block(
    block: &RirBlock,
    outer_bound: &BTreeSet<String>,
    free: &mut BTreeSet<String>,
) {
    let mut bound = outer_bound.clone();
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                collect_free_expr(value, &bound, free);
                bound.insert(name.as_str().to_owned());
            }
            RirStmt::Expr(expr) => collect_free_expr(expr, &bound, free),
        }
    }
    if let Some(tail) = &block.tail {
        collect_free_expr(tail, &bound, free);
    }
}

pub(crate) fn bind_pattern_names(pattern: &RirPattern, bound: &mut BTreeSet<String>) {
    match pattern {
        RirPattern::Bind { binding, .. } => {
            bound.insert(binding.as_str().to_owned());
        }
        RirPattern::Constructor { payload, .. } | RirPattern::Tuple(payload) => {
            for pattern in payload {
                bind_pattern_names(pattern, bound);
            }
        }
        RirPattern::List { prefix, tail } => {
            for pattern in prefix {
                bind_pattern_names(pattern, bound);
            }
            if let Some(tail) = tail {
                bind_pattern_names(tail, bound);
            }
        }
        RirPattern::Record { fields, .. } => {
            for (_, pattern) in fields {
                bind_pattern_names(pattern, bound);
            }
        }
        RirPattern::Wildcard
        | RirPattern::Unit
        | RirPattern::Bool(_)
        | RirPattern::Int(_)
        | RirPattern::String(_) => {}
    }
}
