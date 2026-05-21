use std::collections::BTreeMap;

use crate::checker::tyir::{
    BindingId, EntityId, TypedBlock, TypedExpr, TypedExprKind, TypedLiteral, TypedPattern,
    TypedProgram, TypedStmt,
};
use crate::signature::TypeName;

use super::closure::closure_convert_program;

use super::ir::{
    BindingKey, Param, RirBlock, RirExpr, RirExternal, RirFunction, RirMatchArm, RirPath,
    RirPattern, RirProgram, RirReceiveArm, RirStmt,
};
use super::operators::lower_prelude_operator;

#[derive(Debug, Default)]
pub(crate) struct LambdaLowerer;

impl LambdaLowerer {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn lower(&self, program: TypedProgram) -> RirProgram {
        let mut context = LowerContext::default();
        let lowered = RirProgram {
            module_name: program.module_name,
            uses: program.uses.into_iter().map(|use_| use_.name).collect(),
            externals: program
                .externals
                .into_iter()
                .map(|external| RirExternal {
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
                    RirFunction {
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

    pub(crate) fn simplify(&self, tyir: TypedProgram) -> RirProgram {
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

fn lower_block(block: TypedBlock, context: &mut LowerContext) -> RirBlock {
    context.push_scope();
    let lowered = RirBlock {
        statements: block
            .statements
            .into_iter()
            .map(|stmt| match stmt {
                TypedStmt::Let { binding, value, .. } => {
                    let value = lower_expr(value, context);
                    let name = context.bind_existing(&binding);
                    RirStmt::Let { name, value }
                }
                TypedStmt::Expr(expr) => RirStmt::Expr(lower_expr(expr, context)),
            })
            .collect(),
        tail: block.tail.map(|tail| lower_expr(tail, context)),
    };
    context.pop_scope();
    lowered
}

fn lower_expr(expr: TypedExpr, context: &mut LowerContext) -> RirExpr {
    let expr_type = expr.type_;
    match expr.kind {
        TypedExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => RirExpr::If {
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
                    RirMatchArm { pattern, body }
                })
                .collect();
            RirExpr::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            }
        }
        TypedExprKind::Block(block) => RirExpr::Block(Box::new(lower_block(*block, context))),
        TypedExprKind::Literal(literal) => lower_literal(literal),
        TypedExprKind::Call { callee, args } => lower_call(callee, args, context),
        TypedExprKind::Apply { callee, args } => RirExpr::Apply {
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
            RirExpr::Lambda {
                params,
                captures: Vec::new(),
                body: Box::new(body),
            }
        }
        TypedExprKind::Spawn { body } => RirExpr::Spawn {
            actor_id: context.next_actor_id(),
            body: Box::new(lower_block(*body, context)),
        },
        TypedExprKind::Receive { arms } => RirExpr::Receive {
            arms: arms
                .into_iter()
                .map(|arm| {
                    context.push_scope();
                    let pattern = lower_pattern(arm.pattern, context);
                    let body = lower_expr(arm.body, context);
                    context.pop_scope();
                    RirReceiveArm { pattern, body }
                })
                .collect(),
        },
        TypedExprKind::Tuple(items) => RirExpr::Tuple(
            items
                .into_iter()
                .map(|item| lower_expr(item, context))
                .collect(),
        ),
        TypedExprKind::List(items) => RirExpr::List(
            items
                .into_iter()
                .map(|item| lower_expr(item, context))
                .collect(),
        ),
        TypedExprKind::Record { path, fields } => RirExpr::Record {
            path: path.as_strings(),
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_expr(value, context)))
                .collect(),
        },
        TypedExprKind::Field { base, field } => RirExpr::Field {
            base: Box::new(lower_expr(*base, context)),
            field,
        },
        TypedExprKind::TupleIndex { base, index } => RirExpr::TupleIndex {
            base: Box::new(lower_expr(*base, context)),
            index,
        },
        TypedExprKind::Constructor {
            type_name,
            constructor,
            payload,
        } => {
            if type_name.is_none() && constructor.as_str() == "()" && payload.is_empty() {
                RirExpr::Unit
            } else {
                RirExpr::Variant {
                    type_name: type_name.unwrap_or_else(|| TypeName::new("unit")),
                    constructor,
                    payload: payload
                        .into_iter()
                        .map(|payload| lower_expr(payload, context))
                        .collect(),
                }
            }
        }
        TypedExprKind::Entity(ident) => RirExpr::Path(lower_entity_path(ident, context)),
        TypedExprKind::Local(binding) => RirExpr::Path(RirPath::singleton(binding.key_name())),
    }
}

fn lower_call(callee: EntityId, args: Vec<TypedExpr>, context: &mut LowerContext) -> RirExpr {
    let callee = lower_entity_path(callee, context);
    let args = args
        .into_iter()
        .map(|arg| lower_expr(arg, context))
        .collect::<Vec<_>>();
    lower_prelude_operator(callee, args)
}

fn lower_entity_path(entity: EntityId, _context: &LowerContext) -> RirPath {
    RirPath::from_segments(entity.as_strings())
}

fn lower_literal(literal: TypedLiteral) -> RirExpr {
    match literal {
        TypedLiteral::Bool(value) => RirExpr::Bool(value),
        TypedLiteral::Char(value) => RirExpr::Char(value),
        TypedLiteral::Float(value) => RirExpr::Float(value),
        TypedLiteral::Int(value) => RirExpr::Int(value),
        TypedLiteral::String(value) => RirExpr::String(value),
    }
}

fn lower_pattern(pattern: TypedPattern, context: &mut LowerContext) -> RirPattern {
    match pattern {
        TypedPattern::Wildcard => RirPattern::Wildcard,
        TypedPattern::Bind { binding, type_ } => RirPattern::Bind {
            binding: context.bind_existing(&binding),
            type_,
        },
        TypedPattern::Constructor {
            type_name,
            constructor,
            payload,
        } => RirPattern::Constructor {
            type_name,
            constructor,
            payload: payload
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
        },
        TypedPattern::Tuple(items) => RirPattern::Tuple(
            items
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
        ),
        TypedPattern::List { prefix, tail } => RirPattern::List {
            prefix: prefix
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
            tail: tail.map(|tail| Box::new(lower_pattern(*tail, context))),
        },
        TypedPattern::Record { type_name, fields } => RirPattern::Record {
            type_name,
            fields: fields
                .into_iter()
                .map(|(name, pattern)| (name, lower_pattern(pattern, context)))
                .collect(),
        },
        TypedPattern::Unit => RirPattern::Unit,
        TypedPattern::Bool(value) => RirPattern::Bool(value),
        TypedPattern::Int(value) => RirPattern::Int(value),
        TypedPattern::String(value) => RirPattern::String(value),
    }
}
