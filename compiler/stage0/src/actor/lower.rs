use std::collections::BTreeMap;

use crate::lambda::ir::{LambdaBlock, LambdaExpr, LambdaProgram, LambdaStmt};
use crate::signature::ImportedSignatures;

use super::air::{ActorIrActor, ActorIrProgram, ActorSlotType};
use super::frame::{
    ActorSlotTypeContext, actor_frame_from_block, bind_pattern_actor_slot_types,
    infer_actor_slot_type,
};

pub(crate) struct ActorIrLowerer<'a> {
    imports: &'a ImportedSignatures,
}

impl<'a> ActorIrLowerer<'a> {
    pub(crate) fn new(imports: &'a ImportedSignatures) -> Self {
        Self { imports }
    }

    pub(crate) fn lower(&self, program: &LambdaProgram) -> ActorIrProgram {
        let mut actors = Vec::new();
        let context = ActorSlotTypeContext::from_program(program, self.imports);
        for function in &program.functions {
            let mut locals = function
                .params
                .iter()
                .zip(&function.param_types)
                .map(|(name, type_)| (name.as_str().to_owned(), ActorSlotType::from_rsig(type_)))
                .collect::<BTreeMap<_, _>>();
            collect_actors_from_block(&function.body, &mut locals, &context, &mut actors);
        }
        ActorIrProgram { actors }
    }
}

pub(crate) struct StacklessActorLowerer<'a> {
    imports: &'a ImportedSignatures,
}

impl<'a> StacklessActorLowerer<'a> {
    pub(crate) fn new(imports: &'a ImportedSignatures) -> Self {
        Self { imports }
    }

    pub(crate) fn lower(&self, lambda_ir: &LambdaProgram) -> ActorIrProgram {
        ActorIrLowerer::new(self.imports).lower(lambda_ir)
    }
}

fn collect_actors_from_block(
    block: &LambdaBlock,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext<'_>,
    actors: &mut Vec<ActorIrActor>,
) {
    for stmt in &block.statements {
        match stmt {
            LambdaStmt::Let { name, value } => {
                collect_actors_from_expr(value, locals, context, actors);
                locals.insert(
                    name.as_str().to_owned(),
                    infer_actor_slot_type(value, locals, context),
                );
            }
            LambdaStmt::Expr(expr) => collect_actors_from_expr(expr, locals, context, actors),
        }
    }
    if let Some(tail) = &block.tail {
        collect_actors_from_expr(tail, locals, context, actors);
    }
}

fn collect_actors_from_expr(
    expr: &LambdaExpr,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext<'_>,
    actors: &mut Vec<ActorIrActor>,
) {
    match expr {
        LambdaExpr::Spawn { actor_id, body } => {
            let actor = actor_frame_from_block(*actor_id, body, locals, context);
            let mut actor_locals = actor
                .frame
                .slots
                .iter()
                .map(|slot| (slot.name.as_str().to_owned(), Some(slot.type_)))
                .collect::<BTreeMap<_, _>>();
            actors.push(actor);
            collect_actors_from_block(body, &mut actor_locals, context, actors);
        }
        LambdaExpr::Add(lhs, rhs)
        | LambdaExpr::Sub(lhs, rhs)
        | LambdaExpr::Mul(lhs, rhs)
        | LambdaExpr::Div(lhs, rhs)
        | LambdaExpr::Mod(lhs, rhs)
        | LambdaExpr::Eq(lhs, rhs)
        | LambdaExpr::Lt(lhs, rhs)
        | LambdaExpr::And(lhs, rhs)
        | LambdaExpr::Or(lhs, rhs) => {
            collect_actors_from_expr(lhs, locals, context, actors);
            collect_actors_from_expr(rhs, locals, context, actors);
        }
        LambdaExpr::Neg(value) | LambdaExpr::Not(value) => {
            collect_actors_from_expr(value, locals, context, actors);
        }
        LambdaExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_actors_from_expr(condition, locals, context, actors);
            collect_actors_from_expr(then_branch, locals, context, actors);
            collect_actors_from_expr(else_branch, locals, context, actors);
        }
        LambdaExpr::Match { scrutinee, arms } => {
            collect_actors_from_expr(scrutinee, locals, context, actors);
            let scrutinee_type = infer_actor_slot_type(scrutinee, locals, context);
            for arm in arms {
                let mut arm_locals = locals.clone();
                bind_pattern_actor_slot_types(&arm.pattern, scrutinee_type, &mut arm_locals);
                collect_actors_from_expr(&arm.body, &mut arm_locals, context, actors);
            }
        }
        LambdaExpr::Block(block) => {
            let mut block_locals = locals.clone();
            collect_actors_from_block(block, &mut block_locals, context, actors);
        }
        LambdaExpr::Call { args, .. } | LambdaExpr::Tuple(args) | LambdaExpr::List(args) => {
            for arg in args {
                collect_actors_from_expr(arg, locals, context, actors);
            }
        }
        LambdaExpr::Apply { callee, args, .. } => {
            collect_actors_from_expr(callee, locals, context, actors);
            for arg in args {
                collect_actors_from_expr(arg, locals, context, actors);
            }
        }
        LambdaExpr::Lambda { params, body, .. } => {
            let mut lambda_locals = locals.clone();
            for param in params {
                lambda_locals.insert(param.as_str().to_owned(), None);
            }
            collect_actors_from_block(body, &mut lambda_locals, context, actors);
        }
        LambdaExpr::Record { fields, .. } => {
            for (_, value) in fields {
                collect_actors_from_expr(value, locals, context, actors);
            }
        }
        LambdaExpr::Field { base, .. } | LambdaExpr::TupleIndex { base, .. } => {
            collect_actors_from_expr(base, locals, context, actors);
        }
        LambdaExpr::Receive { arms } => {
            for arm in arms {
                let mut arm_locals = locals.clone();
                bind_pattern_actor_slot_types(
                    &arm.pattern,
                    Some(ActorSlotType::Value),
                    &mut arm_locals,
                );
                collect_actors_from_expr(&arm.body, &mut arm_locals, context, actors);
            }
        }
        LambdaExpr::Variant { payload, .. } => {
            for value in payload {
                collect_actors_from_expr(value, locals, context, actors);
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
