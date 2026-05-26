use std::collections::BTreeMap;

use crate::lambda::closure::{bind_pattern_names, collect_free_expr};
use crate::lambda::ir::{LambdaBlock, LambdaExpr, LambdaPattern, LambdaProgram, LambdaStmt};
use crate::signature::ImportedSignatures;

use super::air::{
    ActorFrameLayout, ActorFrameOp, ActorFrameSlot, ActorFrameSlotName, ActorFrameState,
    ActorIrActor, ActorSlotType, ActorStateNext,
};

pub(crate) struct ActorSlotTypeContext;

impl ActorSlotTypeContext {
    pub(crate) fn from_program(_program: &LambdaProgram, _imports: &ImportedSignatures) -> Self {
        Self
    }
}

pub(crate) fn bind_pattern_actor_slot_types(
    pattern: &LambdaPattern,
    type_: Option<ActorSlotType>,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
) {
    match pattern {
        LambdaPattern::Bind {
            binding,
            type_: binding_type,
        } => {
            locals.insert(
                binding.as_str().to_owned(),
                ActorSlotType::from_rsig(binding_type).or(type_),
            );
        }
        LambdaPattern::Constructor { payload, .. } | LambdaPattern::Tuple(payload) => {
            for pattern in payload {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
        }
        LambdaPattern::List { prefix, tail } => {
            for pattern in prefix {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
            if let Some(tail) = tail {
                bind_pattern_actor_slot_types(tail, Some(ActorSlotType::Value), locals);
            }
        }
        LambdaPattern::Record { fields, .. } => {
            for (_, pattern) in fields {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
        }
        LambdaPattern::Wildcard
        | LambdaPattern::Unit
        | LambdaPattern::Bool(_)
        | LambdaPattern::Int(_)
        | LambdaPattern::String(_) => {}
    }
}

pub(crate) fn infer_actor_slot_type(
    expr: &LambdaExpr,
    locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext,
) -> Option<ActorSlotType> {
    match expr {
        LambdaExpr::Int(_) => Some(ActorSlotType::I64),
        LambdaExpr::Bool(_) => Some(ActorSlotType::Bool),
        LambdaExpr::If {
            then_branch,
            else_branch,
            ..
        } => unify_actor_slot_type(
            infer_actor_slot_type(then_branch, locals, context),
            infer_actor_slot_type(else_branch, locals, context),
        ),
        LambdaExpr::While { .. } => None,
        LambdaExpr::Match { arms, .. } => arms
            .iter()
            .map(|arm| infer_actor_slot_type(&arm.body, locals, context))
            .fold(None, unify_actor_slot_type),
        LambdaExpr::Block(block) => infer_actor_block_slot_type(block, locals, context),
        LambdaExpr::Call { result, .. } => ActorSlotType::from_rsig(result),
        LambdaExpr::Path(path) => path
            .first()
            .and_then(|name| locals.get(name))
            .copied()
            .flatten(),
        LambdaExpr::Local(binding) => locals.get(binding.as_str()).copied().flatten(),
        LambdaExpr::Spawn { .. } => Some(ActorSlotType::ActorId),
        LambdaExpr::Tuple(_)
        | LambdaExpr::List(_)
        | LambdaExpr::Lambda { .. }
        | LambdaExpr::Record { .. }
        | LambdaExpr::Variant { .. }
        | LambdaExpr::Field { .. }
        | LambdaExpr::TupleIndex { .. }
        | LambdaExpr::String(_) => Some(ActorSlotType::Value),
        LambdaExpr::Apply { result, .. } => ActorSlotType::from_rsig(result),
        LambdaExpr::Unit
        | LambdaExpr::Receive { .. }
        | LambdaExpr::Char(_)
        | LambdaExpr::Float(_) => None,
    }
}

fn infer_actor_block_slot_type(
    block: &LambdaBlock,
    outer_locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext,
) -> Option<ActorSlotType> {
    let mut locals = outer_locals.clone();
    for stmt in &block.statements {
        match stmt {
            LambdaStmt::Let { name, value } => {
                let type_ = infer_actor_slot_type(value, &locals, context);
                locals.insert(name.as_str().to_owned(), type_);
            }
            LambdaStmt::Expr(expr) => {
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
    block: &LambdaBlock,
    outer_locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext,
) -> ActorIrActor {
    let ops = actor_ops(block);
    let mut bound = std::collections::BTreeSet::new();
    let mut free = std::collections::BTreeSet::new();
    for op in &ops {
        match &op {
            ActorFrameOp::Let { name, value } => {
                collect_free_expr(value, &bound, &mut free);
                bound.insert(name.clone());
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
        if let Some(Some(type_)) = outer_locals.get(name.as_str()) {
            slots.push(ActorFrameSlot {
                name: ActorFrameSlotName::new(name.as_str()),
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
                    name: ActorFrameSlotName::new(name.as_str()),
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

fn actor_ops(block: &LambdaBlock) -> Vec<ActorFrameOp> {
    let mut ops = Vec::new();
    for stmt in &block.statements {
        match stmt {
            LambdaStmt::Let { name, value } => ops.push(ActorFrameOp::Let {
                name: name.clone(),
                value: value.clone(),
            }),
            LambdaStmt::Expr(LambdaExpr::Receive { arms }) => {
                ops.push(ActorFrameOp::Receive { arms: arms.clone() });
            }
            LambdaStmt::Expr(expr) => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    if let Some(expr) = &block.tail {
        match expr {
            LambdaExpr::Receive { arms } => {
                ops.push(ActorFrameOp::Receive { arms: arms.clone() });
            }
            expr => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    ops
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::lambda::ir::{
        BindingKey, LambdaBlock, LambdaExpr, LambdaPattern, LambdaReceiveArm, LambdaStmt,
    };
    use crate::signature::{ConstructorName, RsigType, TypeName};

    use super::{
        ActorSlotType, ActorSlotTypeContext, actor_frame_from_block, bind_pattern_actor_slot_types,
    };

    #[test]
    fn generic_pattern_binders_keep_concrete_actor_slot_types() {
        let pattern = LambdaPattern::Constructor {
            type_name: TypeName::new("option"),
            constructor: ConstructorName::new("Some"),
            payload: vec![LambdaPattern::Bind {
                binding: BindingKey::resolved("value", 0),
                type_: RsigType::I64,
            }],
        };
        let mut locals = BTreeMap::new();

        bind_pattern_actor_slot_types(&pattern, Some(ActorSlotType::Value), &mut locals);

        assert_eq!(locals.get("value$0"), Some(&Some(ActorSlotType::I64)));
    }

    #[test]
    fn generic_record_pattern_binders_keep_boxed_actor_slot_types() {
        let pattern = LambdaPattern::Record {
            type_name: TypeName::new("box"),
            fields: vec![(
                "value".to_owned(),
                LambdaPattern::Bind {
                    binding: BindingKey::resolved("value", 0),
                    type_: RsigType::RecordApp {
                        name: TypeName::new("inner"),
                        args: vec![RsigType::String],
                    },
                },
            )],
        };
        let mut locals = BTreeMap::new();

        bind_pattern_actor_slot_types(&pattern, Some(ActorSlotType::Value), &mut locals);

        assert_eq!(locals.get("value$0"), Some(&Some(ActorSlotType::Value)));
    }

    #[test]
    fn actor_frame_receive_ops_capture_outer_values_but_not_pattern_binders() {
        let outer = BindingKey::resolved("outer", 0);
        let msg = BindingKey::resolved("msg", 1);
        let block = LambdaBlock {
            statements: Vec::new(),
            tail: Some(LambdaExpr::Receive {
                arms: vec![LambdaReceiveArm {
                    pattern: LambdaPattern::Bind {
                        binding: msg.clone(),
                        type_: RsigType::I64,
                    },
                    body: LambdaExpr::Tuple(vec![
                        LambdaExpr::Local(outer.clone()),
                        LambdaExpr::Local(msg),
                    ]),
                }],
            }),
        };
        let mut outer_locals = BTreeMap::new();
        outer_locals.insert(outer.as_str().to_owned(), Some(ActorSlotType::I64));

        let actor = actor_frame_from_block(7, &block, &outer_locals, &ActorSlotTypeContext);

        assert_eq!(actor.frame.captures.len(), 1);
        assert_eq!(actor.frame.captures[0].name.as_str(), "outer$0");
        assert_eq!(actor.frame.captures[0].type_, ActorSlotType::I64);
    }

    #[test]
    fn actor_frame_receive_ops_treat_nested_pattern_binders_as_arm_local() {
        let outer = BindingKey::resolved("outer", 0);
        let scalar = BindingKey::resolved("scalar", 1);
        let boxed = BindingKey::resolved("boxed", 2);
        let block = LambdaBlock {
            statements: Vec::new(),
            tail: Some(LambdaExpr::Receive {
                arms: vec![LambdaReceiveArm {
                    pattern: LambdaPattern::Tuple(vec![
                        LambdaPattern::Constructor {
                            type_name: TypeName::new("option"),
                            constructor: ConstructorName::new("Some"),
                            payload: vec![LambdaPattern::Bind {
                                binding: scalar.clone(),
                                type_: RsigType::I64,
                            }],
                        },
                        LambdaPattern::Record {
                            type_name: TypeName::new("box"),
                            fields: vec![(
                                "value".to_owned(),
                                LambdaPattern::Bind {
                                    binding: boxed.clone(),
                                    type_: RsigType::RecordApp {
                                        name: TypeName::new("payload"),
                                        args: vec![RsigType::String],
                                    },
                                },
                            )],
                        },
                    ]),
                    body: LambdaExpr::Tuple(vec![
                        LambdaExpr::Local(outer.clone()),
                        LambdaExpr::Local(scalar),
                        LambdaExpr::Local(boxed),
                    ]),
                }],
            }),
        };
        let mut outer_locals = BTreeMap::new();
        outer_locals.insert(outer.as_str().to_owned(), Some(ActorSlotType::I64));

        let actor = actor_frame_from_block(8, &block, &outer_locals, &ActorSlotTypeContext);

        assert_eq!(actor.frame.captures.len(), 1);
        assert_eq!(actor.frame.captures[0].name.as_str(), "outer$0");
        assert_eq!(actor.frame.captures[0].type_, ActorSlotType::I64);
    }

    #[test]
    fn actor_frame_receive_pattern_binders_shadow_outer_bindings() {
        let outer = BindingKey::resolved("value", 0);
        let pattern_value = BindingKey::resolved("value", 1);
        let block = LambdaBlock {
            statements: Vec::new(),
            tail: Some(LambdaExpr::Receive {
                arms: vec![LambdaReceiveArm {
                    pattern: LambdaPattern::Bind {
                        binding: pattern_value.clone(),
                        type_: RsigType::I64,
                    },
                    body: LambdaExpr::Local(pattern_value),
                }],
            }),
        };
        let mut outer_locals = BTreeMap::new();
        outer_locals.insert(outer.as_str().to_owned(), Some(ActorSlotType::Value));

        let actor = actor_frame_from_block(9, &block, &outer_locals, &ActorSlotTypeContext);

        assert!(actor.frame.captures.is_empty());
    }

    #[test]
    fn actor_frame_receive_ops_use_prior_let_slots_without_capturing_them() {
        let value = BindingKey::resolved("value", 0);
        let msg = BindingKey::resolved("msg", 1);
        let block = LambdaBlock {
            statements: vec![
                LambdaStmt::Let {
                    name: value.clone(),
                    value: LambdaExpr::Int(41),
                },
                LambdaStmt::Expr(LambdaExpr::Receive {
                    arms: vec![LambdaReceiveArm {
                        pattern: LambdaPattern::Bind {
                            binding: msg.clone(),
                            type_: RsigType::I64,
                        },
                        body: LambdaExpr::Tuple(vec![
                            LambdaExpr::Local(value.clone()),
                            LambdaExpr::Local(msg),
                        ]),
                    }],
                }),
            ],
            tail: None,
        };

        let actor = actor_frame_from_block(8, &block, &BTreeMap::new(), &ActorSlotTypeContext);

        assert!(actor.frame.captures.is_empty());
        assert_eq!(actor.frame.slots.len(), 1);
        assert_eq!(actor.frame.slots[0].name.as_str(), "value$0");
        assert_eq!(actor.frame.slots[0].type_, ActorSlotType::I64);
    }
}
