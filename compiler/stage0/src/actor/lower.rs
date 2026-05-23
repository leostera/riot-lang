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
    context: &ActorSlotTypeContext,
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
    context: &ActorSlotTypeContext,
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
        LambdaExpr::Lambda {
            params,
            param_types,
            body,
            ..
        } => {
            let mut lambda_locals = locals.clone();
            for (param, type_) in params.iter().zip(param_types) {
                lambda_locals.insert(param.as_str().to_owned(), ActorSlotType::from_rsig(type_));
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
        | LambdaExpr::Local(_)
        | LambdaExpr::String(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::lambda::ir::{
        BindingKey, LambdaBlock, LambdaExpr, LambdaFunction, LambdaMatchArm, LambdaPattern,
        LambdaProgram, LambdaReceiveArm, LambdaStmt, Param,
    };
    use crate::signature::{ConstructorName, ImportedSignatures, ModuleName, RsigType, TypeName};

    use super::{ActorIrLowerer, ActorSlotType};

    fn bind(name: &str, id: usize) -> BindingKey {
        BindingKey::resolved(name, id)
    }

    fn binding_pattern(name: &str, id: usize, type_: RsigType) -> LambdaPattern {
        LambdaPattern::Bind {
            binding: bind(name, id),
            type_,
        }
    }

    fn program_with_body(
        params: Vec<Param>,
        param_types: Vec<RsigType>,
        body: LambdaBlock,
    ) -> LambdaProgram {
        LambdaProgram {
            module_name: ModuleName::new("ActorCaptureTest"),
            uses: Vec::new(),
            externals: Vec::new(),
            functions: vec![LambdaFunction {
                name: "main".to_owned(),
                params,
                param_types,
                result: RsigType::Unit,
                symbol: "riot_mod_ActorCaptureTest_main".to_owned(),
                body,
            }],
        }
    }

    fn program_with_tail(tail: LambdaExpr) -> LambdaProgram {
        program_with_body(
            Vec::new(),
            Vec::new(),
            LambdaBlock {
                statements: Vec::new(),
                tail: Some(tail),
            },
        )
    }

    fn capture_type(actors: &super::ActorIrProgram, name: &str) -> Option<ActorSlotType> {
        actors.actors[0]
            .frame
            .captures
            .iter()
            .find(|capture| capture.name.as_str() == name)
            .map(|capture| capture.type_)
    }

    #[test]
    fn actor_ir_captures_match_binders_for_nested_spawns() {
        let value = bind("value", 0);
        let program = program_with_tail(LambdaExpr::Match {
            scrutinee: Box::new(LambdaExpr::Int(41)),
            arms: vec![LambdaMatchArm {
                pattern: binding_pattern("value", 0, RsigType::Unknown),
                body: LambdaExpr::Spawn {
                    actor_id: 7,
                    body: Box::new(LambdaBlock {
                        statements: Vec::new(),
                        tail: Some(LambdaExpr::Local(value)),
                    }),
                },
            }],
        });

        let actors = ActorIrLowerer::new(&ImportedSignatures::default()).lower(&program);

        assert_eq!(actors.actors.len(), 1);
        assert_eq!(actors.actors[0].id, 7);
        assert_eq!(actors.actors[0].frame.captures.len(), 1);
        assert_eq!(actors.actors[0].frame.captures[0].name.as_str(), "value$0");
        assert_eq!(actors.actors[0].frame.captures[0].type_, ActorSlotType::I64);
    }

    #[test]
    fn actor_ir_captures_nested_match_binders_for_nested_spawns() {
        let scalar = bind("scalar", 0);
        let boxed = bind("boxed", 1);
        let program = program_with_tail(LambdaExpr::Match {
            scrutinee: Box::new(LambdaExpr::Tuple(vec![
                LambdaExpr::Int(41),
                LambdaExpr::String("payload".to_owned()),
            ])),
            arms: vec![LambdaMatchArm {
                pattern: LambdaPattern::Tuple(vec![
                    LambdaPattern::Constructor {
                        type_name: TypeName::new("option"),
                        constructor: ConstructorName::new("Some"),
                        payload: vec![binding_pattern("scalar", 0, RsigType::I64)],
                    },
                    LambdaPattern::Record {
                        type_name: TypeName::new("box"),
                        fields: vec![(
                            "value".to_owned(),
                            binding_pattern(
                                "boxed",
                                1,
                                RsigType::RecordApp {
                                    name: TypeName::new("payload"),
                                    args: vec![RsigType::String],
                                },
                            ),
                        )],
                    },
                ]),
                body: LambdaExpr::Spawn {
                    actor_id: 8,
                    body: Box::new(LambdaBlock {
                        statements: Vec::new(),
                        tail: Some(LambdaExpr::Tuple(vec![
                            LambdaExpr::Local(scalar),
                            LambdaExpr::Local(boxed),
                        ])),
                    }),
                },
            }],
        });

        let actors = ActorIrLowerer::new(&ImportedSignatures::default()).lower(&program);

        assert_eq!(actors.actors.len(), 1);
        assert_eq!(actors.actors[0].id, 8);
        assert_eq!(actors.actors[0].frame.captures.len(), 2);
        assert_eq!(capture_type(&actors, "scalar$0"), Some(ActorSlotType::I64));
        assert_eq!(capture_type(&actors, "boxed$1"), Some(ActorSlotType::Value));
    }

    #[test]
    fn actor_ir_captures_let_binders_for_later_nested_spawns() {
        let value = bind("value", 0);
        let program = program_with_body(
            Vec::new(),
            Vec::new(),
            LambdaBlock {
                statements: vec![
                    LambdaStmt::Let {
                        name: value.clone(),
                        value: LambdaExpr::Int(41),
                    },
                    LambdaStmt::Expr(LambdaExpr::Spawn {
                        actor_id: 9,
                        body: Box::new(LambdaBlock {
                            statements: Vec::new(),
                            tail: Some(LambdaExpr::Local(value)),
                        }),
                    }),
                ],
                tail: None,
            },
        );

        let actors = ActorIrLowerer::new(&ImportedSignatures::default()).lower(&program);

        assert_eq!(actors.actors.len(), 1);
        assert_eq!(actors.actors[0].id, 9);
        assert_eq!(actors.actors[0].frame.captures.len(), 1);
        assert_eq!(actors.actors[0].frame.captures[0].name.as_str(), "value$0");
        assert_eq!(actors.actors[0].frame.captures[0].type_, ActorSlotType::I64);
    }

    #[test]
    fn actor_ir_uses_outer_binding_for_nested_spawns_inside_shadowing_initializers() {
        let outer = bind("value", 0);
        let shadow = bind("value", 1);
        let program = program_with_body(
            vec![Param::from_key(outer.clone())],
            vec![RsigType::I64],
            LambdaBlock {
                statements: vec![LambdaStmt::Let {
                    name: shadow,
                    value: LambdaExpr::Spawn {
                        actor_id: 10,
                        body: Box::new(LambdaBlock {
                            statements: Vec::new(),
                            tail: Some(LambdaExpr::Local(outer)),
                        }),
                    },
                }],
                tail: None,
            },
        );

        let actors = ActorIrLowerer::new(&ImportedSignatures::default()).lower(&program);

        assert_eq!(actors.actors.len(), 1);
        assert_eq!(actors.actors[0].id, 10);
        assert_eq!(actors.actors[0].frame.captures.len(), 1);
        assert_eq!(actors.actors[0].frame.captures[0].name.as_str(), "value$0");
        assert_eq!(actors.actors[0].frame.captures[0].type_, ActorSlotType::I64);
    }

    #[test]
    fn actor_ir_captures_lambda_params_for_nested_spawns() {
        let outer = bind("value", 0);
        let param = bind("value", 1);
        let program = program_with_body(
            vec![Param::from_key(outer)],
            vec![RsigType::String],
            LambdaBlock {
                statements: Vec::new(),
                tail: Some(LambdaExpr::Lambda {
                    params: vec![Param::from_key(param.clone())],
                    param_types: vec![RsigType::I64],
                    captures: Vec::new(),
                    body: Box::new(LambdaBlock {
                        statements: Vec::new(),
                        tail: Some(LambdaExpr::Spawn {
                            actor_id: 12,
                            body: Box::new(LambdaBlock {
                                statements: Vec::new(),
                                tail: Some(LambdaExpr::Local(param)),
                            }),
                        }),
                    }),
                }),
            },
        );

        let actors = ActorIrLowerer::new(&ImportedSignatures::default()).lower(&program);

        assert_eq!(actors.actors.len(), 1);
        assert_eq!(actors.actors[0].id, 12);
        assert_eq!(actors.actors[0].frame.captures.len(), 1);
        assert_eq!(actors.actors[0].frame.captures[0].name.as_str(), "value$1");
        assert_eq!(actors.actors[0].frame.captures[0].type_, ActorSlotType::I64);
    }

    #[test]
    fn actor_ir_captures_receive_binders_for_nested_spawns() {
        let msg = bind("msg", 0);
        let program = program_with_tail(LambdaExpr::Receive {
            arms: vec![LambdaReceiveArm {
                pattern: binding_pattern("msg", 0, RsigType::I64),
                body: LambdaExpr::Spawn {
                    actor_id: 11,
                    body: Box::new(LambdaBlock {
                        statements: Vec::new(),
                        tail: Some(LambdaExpr::Local(msg)),
                    }),
                },
            }],
        });

        let actors = ActorIrLowerer::new(&ImportedSignatures::default()).lower(&program);

        assert_eq!(actors.actors.len(), 1);
        assert_eq!(actors.actors[0].id, 11);
        assert_eq!(actors.actors[0].frame.captures.len(), 1);
        assert_eq!(actors.actors[0].frame.captures[0].name.as_str(), "msg$0");
        assert_eq!(actors.actors[0].frame.captures[0].type_, ActorSlotType::I64);
    }

    #[test]
    fn actor_ir_captures_nested_receive_binders_for_nested_spawns() {
        let scalar = bind("scalar", 0);
        let boxed = bind("boxed", 1);
        let program = program_with_tail(LambdaExpr::Receive {
            arms: vec![LambdaReceiveArm {
                pattern: LambdaPattern::Tuple(vec![
                    LambdaPattern::Constructor {
                        type_name: TypeName::new("option"),
                        constructor: ConstructorName::new("Some"),
                        payload: vec![binding_pattern("scalar", 0, RsigType::I64)],
                    },
                    LambdaPattern::Record {
                        type_name: TypeName::new("box"),
                        fields: vec![(
                            "value".to_owned(),
                            binding_pattern(
                                "boxed",
                                1,
                                RsigType::RecordApp {
                                    name: TypeName::new("payload"),
                                    args: vec![RsigType::String],
                                },
                            ),
                        )],
                    },
                ]),
                body: LambdaExpr::Spawn {
                    actor_id: 13,
                    body: Box::new(LambdaBlock {
                        statements: Vec::new(),
                        tail: Some(LambdaExpr::Tuple(vec![
                            LambdaExpr::Local(scalar),
                            LambdaExpr::Local(boxed),
                        ])),
                    }),
                },
            }],
        });

        let actors = ActorIrLowerer::new(&ImportedSignatures::default()).lower(&program);

        assert_eq!(actors.actors.len(), 1);
        assert_eq!(actors.actors[0].id, 13);
        assert_eq!(actors.actors[0].frame.captures.len(), 2);
        assert_eq!(capture_type(&actors, "scalar$0"), Some(ActorSlotType::I64));
        assert_eq!(capture_type(&actors, "boxed$1"), Some(ActorSlotType::Value));
    }
}
