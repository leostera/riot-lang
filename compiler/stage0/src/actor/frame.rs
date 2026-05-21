use std::collections::BTreeMap;

use crate::lambda::closure::{bind_pattern_names, collect_free_expr};
use crate::lambda::ir::{RirBlock, RirExpr, RirPattern, RirProgram, RirStmt};
use crate::signature::{ImportedSignatures, RsigExport, RsigType};

use super::air::{
    ActorFrameLayout, ActorFrameOp, ActorFrameSlot, ActorFrameSlotName, ActorFrameState,
    ActorIrActor, ActorSlotType, ActorStateNext,
};

pub(crate) struct ActorSlotTypeContext<'a> {
    functions: BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: BTreeMap<String, (Vec<RsigType>, RsigType)>,
    imports: &'a ImportedSignatures,
}

impl<'a> ActorSlotTypeContext<'a> {
    pub(crate) fn from_program(program: &RirProgram, imports: &'a ImportedSignatures) -> Self {
        Self {
            functions: function_type_map(program),
            externals: external_type_map(program),
            imports,
        }
    }
}

fn function_type_map(program: &RirProgram) -> BTreeMap<String, (Vec<RsigType>, RsigType)> {
    program
        .functions
        .iter()
        .map(|function| {
            (
                function.name.clone(),
                (function.param_types.clone(), function.result.clone()),
            )
        })
        .collect()
}

fn external_type_map(program: &RirProgram) -> BTreeMap<String, (Vec<RsigType>, RsigType)> {
    program
        .externals
        .iter()
        .map(|external| {
            (
                external.name.clone(),
                (external.params.clone(), external.result.clone()),
            )
        })
        .collect()
}

pub(crate) fn bind_pattern_actor_slot_types(
    pattern: &RirPattern,
    type_: Option<ActorSlotType>,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
) {
    match pattern {
        RirPattern::Bind {
            binding,
            type_: binding_type,
        } => {
            locals.insert(
                binding.as_str().to_owned(),
                ActorSlotType::from_rsig(binding_type).or(type_),
            );
        }
        RirPattern::Constructor { payload, .. } | RirPattern::Tuple(payload) => {
            for pattern in payload {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
        }
        RirPattern::List { prefix, tail } => {
            for pattern in prefix {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
            if let Some(tail) = tail {
                bind_pattern_actor_slot_types(tail, Some(ActorSlotType::Value), locals);
            }
        }
        RirPattern::Record { fields, .. } => {
            for (_, pattern) in fields {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
        }
        RirPattern::Wildcard
        | RirPattern::Unit
        | RirPattern::Bool(_)
        | RirPattern::Int(_)
        | RirPattern::String(_) => {}
    }
}

pub(crate) fn infer_actor_slot_type(
    expr: &RirExpr,
    locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext<'_>,
) -> Option<ActorSlotType> {
    match expr {
        RirExpr::Add(_, _)
        | RirExpr::Sub(_, _)
        | RirExpr::Mul(_, _)
        | RirExpr::Div(_, _)
        | RirExpr::Mod(_, _)
        | RirExpr::Neg(_)
        | RirExpr::Int(_) => Some(ActorSlotType::I64),
        RirExpr::Eq(_, _)
        | RirExpr::Lt(_, _)
        | RirExpr::And(_, _)
        | RirExpr::Or(_, _)
        | RirExpr::Not(_)
        | RirExpr::Bool(_) => Some(ActorSlotType::Bool),
        RirExpr::If {
            then_branch,
            else_branch,
            ..
        } => unify_actor_slot_type(
            infer_actor_slot_type(then_branch, locals, context),
            infer_actor_slot_type(else_branch, locals, context),
        ),
        RirExpr::Match { arms, .. } => arms
            .iter()
            .map(|arm| infer_actor_slot_type(&arm.body, locals, context))
            .fold(None, unify_actor_slot_type),
        RirExpr::Block(block) => infer_actor_block_slot_type(block, locals, context),
        RirExpr::Call { callee, .. } => match callee.as_slice() {
            [name] if name == "dbg" || name == "println" => None,
            [name] if name == "send" || name == "monitor" || name == "link" => None,
            [name] if name == "list_len" || name == "string_len" => Some(ActorSlotType::I64),
            [name] if name == "list_get" || name == "string_concat" => Some(ActorSlotType::Value),
            [name] => context
                .externals
                .get(name)
                .or_else(|| context.functions.get(name))
                .and_then(|(_, result)| ActorSlotType::from_rsig(result)),
            [module, name] => context
                .imports
                .get(module.as_str())
                .and_then(|rsig| rsig.find(name))
                .and_then(|export| match export {
                    RsigExport::Function(function) => ActorSlotType::from_rsig(&function.result),
                    RsigExport::External(external) => ActorSlotType::from_rsig(&external.result),
                }),
            _ => None,
        },
        RirExpr::Path(path) => path
            .first()
            .and_then(|name| locals.get(name))
            .copied()
            .flatten(),
        RirExpr::Spawn { .. } => Some(ActorSlotType::ActorId),
        RirExpr::Tuple(_)
        | RirExpr::List(_)
        | RirExpr::Lambda { .. }
        | RirExpr::Record { .. }
        | RirExpr::Variant { .. }
        | RirExpr::Field { .. }
        | RirExpr::TupleIndex { .. }
        | RirExpr::String(_) => Some(ActorSlotType::Value),
        RirExpr::Apply { result, .. } => ActorSlotType::from_rsig(result),
        RirExpr::Unit | RirExpr::Receive { .. } | RirExpr::Char(_) | RirExpr::Float(_) => None,
    }
}

fn infer_actor_block_slot_type(
    block: &RirBlock,
    outer_locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext<'_>,
) -> Option<ActorSlotType> {
    let mut locals = outer_locals.clone();
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                let type_ = infer_actor_slot_type(value, &locals, context);
                locals.insert(name.as_str().to_owned(), type_);
            }
            RirStmt::Expr(expr) => {
                infer_actor_slot_type(expr, &locals, context);
            }
        }
    }
    block
        .tail
        .as_ref()
        .and_then(|tail| infer_actor_slot_type(tail, &locals, context))
}

fn unify_actor_slot_type(
    lhs: Option<ActorSlotType>,
    rhs: Option<ActorSlotType>,
) -> Option<ActorSlotType> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
        (Some(value), None) | (None, Some(value)) => Some(value),
        _ => None,
    }
}

pub(crate) fn actor_frame_from_block(
    actor_id: usize,
    block: &RirBlock,
    outer_locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext<'_>,
) -> ActorIrActor {
    let ops = actor_ops(block);
    let mut bound = std::collections::BTreeSet::new();
    let mut free = std::collections::BTreeSet::new();
    for op in &ops {
        match &op {
            ActorFrameOp::Let { name, value } => {
                collect_free_expr(value, &bound, &mut free);
                bound.insert(name.as_str().to_owned());
            }
            ActorFrameOp::Expr(expr) => collect_free_expr(expr, &bound, &mut free),
            ActorFrameOp::Receive { arms } => {
                for arm in arms {
                    let mut receive_bound = bound.clone();
                    bind_pattern_names(&arm.pattern, &mut receive_bound);
                    collect_free_expr(&arm.body, &receive_bound, &mut free);
                }
            }
        }
    }

    let mut slots = Vec::new();
    for name in free {
        if let Some(Some(type_)) = outer_locals.get(&name) {
            slots.push(ActorFrameSlot {
                name: ActorFrameSlotName::new(name),
                type_: *type_,
                field_index: slots.len() as u32 + 1,
            });
        }
    }
    let captures = slots.clone();

    let mut local_types = slots
        .iter()
        .map(|slot| (slot.name.as_str().to_owned(), Some(slot.type_)))
        .collect::<BTreeMap<_, _>>();
    for op in &ops {
        if let ActorFrameOp::Let { name, value } = op {
            let type_ = infer_actor_slot_type(value, &local_types, context);
            if let Some(type_) = type_ {
                slots.push(ActorFrameSlot {
                    name: name.clone(),
                    type_,
                    field_index: slots.len() as u32 + 1,
                });
                local_types.insert(name.as_str().to_owned(), Some(type_));
            } else {
                local_types.insert(name.as_str().to_owned(), None);
            }
        }
    }

    let op_count = ops.len();
    let states = ops
        .into_iter()
        .enumerate()
        .map(|(index, op)| {
            let next = if index + 1 >= op_count {
                ActorStateNext::Done
            } else {
                ActorStateNext::State(index + 1)
            };
            ActorFrameState { op, next }
        })
        .collect::<Vec<_>>();

    ActorIrActor {
        id: actor_id,
        frame: ActorFrameLayout {
            size_bytes: (slots.len() + 1) * 8,
            align: 8,
            slots,
            captures,
        },
        states,
    }
}

fn actor_ops(block: &RirBlock) -> Vec<ActorFrameOp> {
    let mut ops = Vec::new();
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => ops.push(ActorFrameOp::Let {
                name: ActorFrameSlotName::new(name.as_str()),
                value: value.clone(),
            }),
            RirStmt::Expr(RirExpr::Receive { arms }) => {
                ops.push(ActorFrameOp::Receive { arms: arms.clone() });
            }
            RirStmt::Expr(expr) => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    if let Some(expr) = &block.tail {
        match expr {
            RirExpr::Receive { arms } => {
                ops.push(ActorFrameOp::Receive { arms: arms.clone() });
            }
            expr => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    ops
}
