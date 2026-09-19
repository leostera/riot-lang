//! Native SSA optimization passes.
//!
//! This module is intentionally small for now. It gives the native pipeline an
//! optimization hook (`native::lower(&lambda).optimize()`) without committing to
//! a large pass manager yet.

use std::collections::{HashMap, HashSet};

use crate::{backend::native::ssa, lambda, parser::ast::Literal};

pub fn optimize(module: &mut ssa::SsaProgram) {
    for function in &mut module.functions {
        const_fold(function);
        copy_prop(function);
        dce(function);
    }
}

fn const_fold(function: &mut ssa::Function) {
    let mut constants: HashMap<ssa::ValueId, ssa::Constant> = HashMap::new();
    for block in &mut function.blocks {
        for stmt in &mut block.stmts {
            let ssa::Stmt::Assign { dst, rhs, .. } = stmt;
            match rhs {
                ssa::Rvalue::Const(constant) => {
                    constants.insert(*dst, constant.clone());
                }
                ssa::Rvalue::Prim { op, args } if args.len() == 2 => {
                    let Some(lhs) = constants.get(&args[0]).and_then(int_constant) else {
                        continue;
                    };
                    let Some(rhs_value) = constants.get(&args[1]).and_then(int_constant) else {
                        continue;
                    };
                    let folded = match op {
                        lambda::ir::Primitive::Add { .. } => lhs.checked_add(rhs_value),
                        lambda::ir::Primitive::Sub { .. } => lhs.checked_sub(rhs_value),
                        lambda::ir::Primitive::Mul { .. } => lhs.checked_mul(rhs_value),
                        lambda::ir::Primitive::Div { .. } => {
                            (rhs_value != 0).then(|| lhs / rhs_value)
                        }
                        lambda::ir::Primitive::Rem { .. } => {
                            (rhs_value != 0).then(|| lhs % rhs_value)
                        }
                        lambda::ir::Primitive::Eq { .. }
                        | lambda::ir::Primitive::Ne { .. }
                        | lambda::ir::Primitive::Lt { .. }
                        | lambda::ir::Primitive::Lte { .. }
                        | lambda::ir::Primitive::Gt { .. }
                        | lambda::ir::Primitive::Gte { .. }
                        | lambda::ir::Primitive::Named(_) => None,
                    };
                    if let Some(value) = folded {
                        let constant = ssa::Constant::Literal(Literal::Int(value.to_string()));
                        *rhs = ssa::Rvalue::Const(constant.clone());
                        constants.insert(*dst, constant);
                    }
                }
                _ => {}
            }
        }
    }
}

fn int_constant(constant: &ssa::Constant) -> Option<i64> {
    match constant {
        ssa::Constant::Literal(Literal::Int(value)) => value.parse().ok(),
        _ => None,
    }
}

fn copy_prop(function: &mut ssa::Function) {
    let mut copies = HashMap::new();
    for block in &function.blocks {
        for stmt in &block.stmts {
            let ssa::Stmt::Assign { dst, rhs, .. } = stmt;
            if let ssa::Rvalue::Copy(src) = rhs {
                copies.insert(*dst, resolve_copy(*src, &copies));
            }
        }
    }

    if copies.is_empty() {
        return;
    }

    for block in &mut function.blocks {
        for stmt in &mut block.stmts {
            let ssa::Stmt::Assign { rhs, .. } = stmt;
            rewrite_rvalue_uses(rhs, &copies);
        }
        rewrite_terminator_uses(&mut block.terminator, &copies);
    }
}

fn resolve_copy(
    mut value: ssa::ValueId,
    copies: &HashMap<ssa::ValueId, ssa::ValueId>,
) -> ssa::ValueId {
    while let Some(next) = copies.get(&value) {
        value = *next;
    }
    value
}

fn rewrite_rvalue_uses(rvalue: &mut ssa::Rvalue, copies: &HashMap<ssa::ValueId, ssa::ValueId>) {
    match rvalue {
        ssa::Rvalue::Copy(value) => *value = resolve_copy(*value, copies),
        ssa::Rvalue::Prim { args, .. } | ssa::Rvalue::Call { args, .. } => {
            for arg in args {
                *arg = resolve_copy(*arg, copies);
            }
        }
        ssa::Rvalue::MakeBlock { fields, .. } => {
            for field in fields {
                *field = resolve_copy(*field, copies);
            }
        }
        ssa::Rvalue::GetField { block, .. } | ssa::Rvalue::GetTag { block } => {
            *block = resolve_copy(*block, copies)
        }
        ssa::Rvalue::RuntimeMatch { scrutinee, .. } => {
            *scrutinee = resolve_copy(*scrutinee, copies)
        }
        ssa::Rvalue::Const(_)
        | ssa::Rvalue::Global(_)
        | ssa::Rvalue::StringLiteral { .. }
        | ssa::Rvalue::FloatLiteral { .. } => {}
    }
}

fn rewrite_terminator_uses(
    terminator: &mut ssa::Terminator,
    copies: &HashMap<ssa::ValueId, ssa::ValueId>,
) {
    match terminator {
        ssa::Terminator::Return(Some(value)) => *value = resolve_copy(*value, copies),
        ssa::Terminator::Return(None) => {}
        ssa::Terminator::Jump { args, .. } => {
            for arg in args {
                *arg = resolve_copy(*arg, copies);
            }
        }
        ssa::Terminator::Branch {
            cond,
            then_args,
            else_args,
            ..
        } => {
            *cond = resolve_copy(*cond, copies);
            for arg in then_args.iter_mut().chain(else_args) {
                *arg = resolve_copy(*arg, copies);
            }
        }
        ssa::Terminator::Switch {
            discr,
            cases,
            default_args,
            ..
        } => {
            *discr = resolve_copy(*discr, copies);
            for (_, _, args) in cases {
                for arg in args {
                    *arg = resolve_copy(*arg, copies);
                }
            }
            for arg in default_args {
                *arg = resolve_copy(*arg, copies);
            }
        }
    }
}

fn dce(function: &mut ssa::Function) {
    let mut used = HashSet::new();
    for block in &function.blocks {
        for stmt in &block.stmts {
            let ssa::Stmt::Assign { rhs, .. } = stmt;
            collect_rvalue_uses(rhs, &mut used);
        }
        collect_terminator_uses(&block.terminator, &mut used);
    }

    for block in &mut function.blocks {
        block.stmts.retain(|stmt| match stmt {
            ssa::Stmt::Assign { dst, rhs, .. } => used.contains(dst) || !is_pure(rhs),
        });
    }
}

fn is_pure(rvalue: &ssa::Rvalue) -> bool {
    matches!(
        rvalue,
        ssa::Rvalue::Const(_)
            | ssa::Rvalue::Copy(_)
            | ssa::Rvalue::Prim { .. }
            | ssa::Rvalue::Global(_)
    )
}

fn collect_rvalue_uses(rvalue: &ssa::Rvalue, used: &mut HashSet<ssa::ValueId>) {
    match rvalue {
        ssa::Rvalue::Copy(value) => {
            used.insert(*value);
        }
        ssa::Rvalue::Prim { args, .. } | ssa::Rvalue::Call { args, .. } => {
            used.extend(args.iter().copied());
        }
        ssa::Rvalue::MakeBlock { fields, .. } => used.extend(fields.iter().copied()),
        ssa::Rvalue::GetField { block, .. } | ssa::Rvalue::GetTag { block } => {
            used.insert(*block);
        }
        ssa::Rvalue::RuntimeMatch { scrutinee, .. } => {
            used.insert(*scrutinee);
        }
        ssa::Rvalue::Const(_)
        | ssa::Rvalue::Global(_)
        | ssa::Rvalue::StringLiteral { .. }
        | ssa::Rvalue::FloatLiteral { .. } => {}
    }
}

fn collect_terminator_uses(terminator: &ssa::Terminator, used: &mut HashSet<ssa::ValueId>) {
    match terminator {
        ssa::Terminator::Return(Some(value)) => {
            used.insert(*value);
        }
        ssa::Terminator::Return(None) => {}
        ssa::Terminator::Jump { args, .. } => used.extend(args.iter().copied()),
        ssa::Terminator::Branch {
            cond,
            then_args,
            else_args,
            ..
        } => {
            used.insert(*cond);
            used.extend(then_args.iter().chain(else_args).copied());
        }
        ssa::Terminator::Switch {
            discr,
            cases,
            default_args,
            ..
        } => {
            used.insert(*discr);
            for (_, _, args) in cases {
                used.extend(args.iter().copied());
            }
            used.extend(default_args.iter().copied());
        }
    }
}
