use std::collections::BTreeMap;

use crate::checker::tyir::{
    BindingId, EntityId, TypedBlock, TypedExpr, TypedExprKind, TypedLiteral, TypedPattern,
    TypedProgram, TypedStmt,
};
use crate::signature::TypeName;

use super::closure::closure_convert_program;

use super::ir::{
    BindingKey, LambdaBlock, LambdaExpr, LambdaExternal, LambdaFunction, LambdaMatchArm,
    LambdaPath, LambdaPattern, LambdaProgram, LambdaReceiveArm, LambdaStmt, Param,
};
use super::operators::lower_prelude_operator;

#[derive(Debug, Default)]
pub(crate) struct LambdaLowerer;

impl LambdaLowerer {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn lower(&self, program: TypedProgram) -> LambdaProgram {
        let mut context = LowerContext::default();
        let lowered = LambdaProgram {
            module_name: program.module_name,
            uses: program.uses.into_iter().map(|use_| use_.name).collect(),
            externals: program
                .externals
                .into_iter()
                .map(|external| LambdaExternal {
                    name: external.name,
                    params: external.params,
                    result: external.result,
                    abi: external.abi,
                })
                .collect(),
            functions: program
                .functions
                .into_iter()
                .map(|function| {
                    context.push_scope();
                    let mut params = Vec::new();
                    let mut param_types = Vec::new();
                    for param in function.params {
                        let key = context.bind_existing(&param.binding);
                        params.push(Param::from_key(key));
                        param_types.push(param.type_);
                    }
                    let body = lower_block(function.body, &mut context);
                    context.pop_scope();
                    LambdaFunction {
                        name: function.name,
                        params,
                        param_types,
                        result: function.result,
                        body,
                        symbol: function.symbol,
                    }
                })
                .collect(),
        };
        closure_convert_program(lowered)
    }
}

pub(crate) struct LambdaSimplifier;

impl LambdaSimplifier {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn simplify(&self, tyir: TypedProgram) -> LambdaProgram {
        LambdaLowerer::new().lower(tyir)
    }
}

#[derive(Default)]
struct LowerContext {
    next_actor_id: usize,
    scopes: Vec<BTreeMap<String, BindingKey>>,
}

impl LowerContext {
    fn next_actor_id(&mut self) -> usize {
        let id = self.next_actor_id;
        self.next_actor_id += 1;
        id
    }

    fn push_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind_existing(&mut self, binding: &BindingId) -> BindingKey {
        if self.scopes.is_empty() {
            self.push_scope();
        }
        let key = BindingKey::new(binding.key_name());
        self.scopes
            .last_mut()
            .expect("lowering always has a lexical scope")
            .insert(binding.name.clone(), key.clone());
        key
    }
}

fn lower_block(block: TypedBlock, context: &mut LowerContext) -> LambdaBlock {
    context.push_scope();
    let lowered = LambdaBlock {
        statements: block
            .statements
            .into_iter()
            .map(|stmt| match stmt {
                TypedStmt::Let { binding, value, .. } => {
                    let value = lower_expr(value, context);
                    let name = context.bind_existing(&binding);
                    LambdaStmt::Let { name, value }
                }
                TypedStmt::Expr(expr) => LambdaStmt::Expr(lower_expr(expr, context)),
            })
            .collect(),
        tail: block.tail.map(|tail| lower_expr(tail, context)),
    };
    context.pop_scope();
    lowered
}

fn lower_expr(expr: TypedExpr, context: &mut LowerContext) -> LambdaExpr {
    let expr_type = expr.type_;
    match expr.kind {
        TypedExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => LambdaExpr::If {
            condition: Box::new(lower_expr(*condition, context)),
            then_branch: Box::new(lower_expr(*then_branch, context)),
            else_branch: Box::new(lower_expr(*else_branch, context)),
        },
        TypedExprKind::Match { scrutinee, arms } => {
            let scrutinee = lower_expr(*scrutinee, context);
            let arms = arms
                .into_iter()
                .map(|arm| {
                    context.push_scope();
                    let pattern = lower_pattern(arm.pattern, context);
                    let body = lower_expr(arm.body, context);
                    context.pop_scope();
                    LambdaMatchArm { pattern, body }
                })
                .collect();
            LambdaExpr::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            }
        }
        TypedExprKind::Block(block) => LambdaExpr::Block(Box::new(lower_block(*block, context))),
        TypedExprKind::Literal(literal) => lower_literal(literal),
        TypedExprKind::Call { callee, args } => lower_call(callee, args, context),
        TypedExprKind::Apply { callee, args } => LambdaExpr::Apply {
            callee: Box::new(lower_expr(*callee, context)),
            args: args
                .into_iter()
                .map(|arg| lower_expr(arg, context))
                .collect(),
            result: expr_type,
        },
        TypedExprKind::Lambda { params, body } => {
            context.push_scope();
            let params = params
                .into_iter()
                .map(|param| Param::from_key(context.bind_existing(&param.binding)))
                .collect::<Vec<_>>();
            let body = lower_block(*body, context);
            context.pop_scope();
            LambdaExpr::Lambda {
                params,
                captures: Vec::new(),
                body: Box::new(body),
            }
        }
        TypedExprKind::Spawn { body } => LambdaExpr::Spawn {
            actor_id: context.next_actor_id(),
            body: Box::new(lower_block(*body, context)),
        },
        TypedExprKind::Receive { arms } => LambdaExpr::Receive {
            arms: arms
                .into_iter()
                .map(|arm| {
                    context.push_scope();
                    let pattern = lower_pattern(arm.pattern, context);
                    let body = lower_expr(arm.body, context);
                    context.pop_scope();
                    LambdaReceiveArm { pattern, body }
                })
                .collect(),
        },
        TypedExprKind::Tuple(items) => LambdaExpr::Tuple(
            items
                .into_iter()
                .map(|item| lower_expr(item, context))
                .collect(),
        ),
        TypedExprKind::List(items) => LambdaExpr::List(
            items
                .into_iter()
                .map(|item| lower_expr(item, context))
                .collect(),
        ),
        TypedExprKind::Record { path, fields } => LambdaExpr::Record {
            path: path.as_strings(),
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_expr(value, context)))
                .collect(),
        },
        TypedExprKind::Field { base, field } => LambdaExpr::Field {
            base: Box::new(lower_expr(*base, context)),
            field,
        },
        TypedExprKind::TupleIndex { base, index } => LambdaExpr::TupleIndex {
            base: Box::new(lower_expr(*base, context)),
            index,
        },
        TypedExprKind::Constructor {
            type_name,
            constructor,
            payload,
        } => {
            if type_name.is_none() && constructor.as_str() == "()" && payload.is_empty() {
                LambdaExpr::Unit
            } else {
                LambdaExpr::Variant {
                    type_name: type_name.unwrap_or_else(|| TypeName::new("unit")),
                    constructor,
                    payload: payload
                        .into_iter()
                        .map(|payload| lower_expr(payload, context))
                        .collect(),
                }
            }
        }
        TypedExprKind::Entity(ident) => LambdaExpr::Path(lower_entity_path(ident, context)),
        TypedExprKind::Local(binding) => {
            LambdaExpr::Path(LambdaPath::singleton(binding.key_name()))
        }
    }
}

fn lower_call(callee: EntityId, args: Vec<TypedExpr>, context: &mut LowerContext) -> LambdaExpr {
    let callee = lower_entity_path(callee, context);
    let args = args
        .into_iter()
        .map(|arg| lower_expr(arg, context))
        .collect::<Vec<_>>();
    lower_prelude_operator(callee, args)
}

fn lower_entity_path(entity: EntityId, _context: &LowerContext) -> LambdaPath {
    LambdaPath::from_segments(entity.as_strings())
}

fn lower_literal(literal: TypedLiteral) -> LambdaExpr {
    match literal {
        TypedLiteral::Bool(value) => LambdaExpr::Bool(value),
        TypedLiteral::Char(value) => LambdaExpr::Char(value),
        TypedLiteral::Float(value) => LambdaExpr::Float(value),
        TypedLiteral::Int(value) => LambdaExpr::Int(value),
        TypedLiteral::String(value) => LambdaExpr::String(value),
    }
}

fn lower_pattern(pattern: TypedPattern, context: &mut LowerContext) -> LambdaPattern {
    match pattern {
        TypedPattern::Wildcard => LambdaPattern::Wildcard,
        TypedPattern::Bind { binding, type_ } => LambdaPattern::Bind {
            binding: context.bind_existing(&binding),
            type_,
        },
        TypedPattern::Constructor {
            type_name,
            constructor,
            payload,
        } => LambdaPattern::Constructor {
            type_name,
            constructor,
            payload: payload
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
        },
        TypedPattern::Tuple(items) => LambdaPattern::Tuple(
            items
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
        ),
        TypedPattern::List { prefix, tail } => LambdaPattern::List {
            prefix: prefix
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
            tail: tail.map(|tail| Box::new(lower_pattern(*tail, context))),
        },
        TypedPattern::Record { type_name, fields } => LambdaPattern::Record {
            type_name,
            fields: fields
                .into_iter()
                .map(|(name, pattern)| (name, lower_pattern(pattern, context)))
                .collect(),
        },
        TypedPattern::Unit => LambdaPattern::Unit,
        TypedPattern::Bool(value) => LambdaPattern::Bool(value),
        TypedPattern::Int(value) => LambdaPattern::Int(value),
        TypedPattern::String(value) => LambdaPattern::String(value),
    }
}
