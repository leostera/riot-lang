#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{AstBlock, AstDecl, AstExpr, AstPath, AstProgram, AstStmt};
use crate::infer::module::infer_function_signatures;
use crate::signature::{
    ImportedSignatures, Rsig, RsigExport, RsigExternal, RsigFunction, RsigType,
    parse_type_signature,
};

mod actor;
mod hir;
mod rir;

pub(crate) use actor::*;
pub(crate) use hir::*;
pub(crate) use rir::*;

pub(crate) fn typed_program_from_ast(
    module_name: String,
    ast: AstProgram,
    imports: &ImportedSignatures,
) -> TypedProgram {
    let inferred_function_types = infer_function_signatures(&ast, imports).ok();
    let mut uses = Vec::new();
    let mut externals = Vec::new();
    let mut ast_functions = Vec::new();

    for decl in ast.decls {
        match decl {
            AstDecl::Use(use_) => uses.push(TypedUse { name: use_.name }),
            AstDecl::External(external) => {
                let (params, result) = parse_type_signature(&external.type_text);
                externals.push(TypedExternal {
                    name: external.name,
                    type_text: external.type_text,
                    params,
                    result,
                    abi: external.abi,
                });
            }
            AstDecl::Function(function) => ast_functions.push(function),
        }
    }

    let function_types =
        inferred_function_types.unwrap_or_else(|| infer_ast_function_types(&ast_functions));
    let external_types = externals
        .iter()
        .map(|external| {
            (
                external.name.clone(),
                (external.params.clone(), external.result.clone()),
            )
        })
        .collect::<BTreeMap<_, _>>();
    let mut functions = Vec::new();
    for function in ast_functions {
        let symbol = module_function_symbol(&module_name, &function.name);
        let (param_types, inferred_result) = function_types
            .get(&function.name)
            .cloned()
            .unwrap_or_else(|| {
                (
                    vec![RsigType::Unknown; function.params.len()],
                    RsigType::Unknown,
                )
            });
        let annotated_result = function
            .return_type
            .as_ref()
            .map(|annotation| parse_type_signature(&annotation.text).1);
        let params = function
            .params
            .iter()
            .enumerate()
            .map(|(index, name)| TypedParam {
                name: name.clone(),
                type_: param_types.get(index).cloned().unwrap_or(RsigType::Unknown),
            })
            .collect::<Vec<_>>();
        let mut context = TypeContext {
            bindings: params
                .iter()
                .map(|param| (param.name.clone(), param.type_.clone()))
                .collect(),
            functions: &function_types,
            externals: &external_types,
            imports,
        };
        let body = type_block(function.body, &mut context);
        let result =
            annotated_result.unwrap_or_else(|| merge_types(inferred_result, body.type_.clone()));
        functions.push(TypedFunction {
            name: function.name,
            params,
            body,
            result,
            symbol,
        });
    }

    TypedProgram {
        module_name,
        uses,
        externals,
        functions,
    }
}

pub(crate) fn signature_for(program: &TypedProgram) -> Rsig {
    let mut exports = Vec::new();
    for function in &program.functions {
        if function.name == "main" {
            continue;
        }
        exports.push(RsigExport::Function(RsigFunction {
            name: function.name.clone(),
            params: function
                .params
                .iter()
                .map(|param| param.type_.clone())
                .collect(),
            result: function.result.clone(),
            symbol: function.symbol.clone(),
            fingerprint: 0,
        }));
    }
    for external in &program.externals {
        exports.push(RsigExport::External(RsigExternal {
            name: external.name.clone(),
            params: external.params.clone(),
            result: external.result.clone(),
            abi: external.abi.clone(),
            fingerprint: 0,
        }));
    }
    Rsig::new(program.module_name.clone(), exports)
}

pub(crate) fn lower_typed_to_rir(program: TypedProgram) -> RirProgram {
    let mut context = LowerContext::default();
    RirProgram {
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
            .map(|function| RirFunction {
                name: function.name,
                params: function
                    .params
                    .iter()
                    .map(|param| Param::new(param.name.clone()))
                    .collect(),
                param_types: function
                    .params
                    .iter()
                    .map(|param| param.type_.clone())
                    .collect(),
                result: function.result,
                body: lower_block(function.body, &mut context),
                symbol: function.symbol,
            })
            .collect(),
    }
}

pub(crate) fn lower_rir_to_actor_ir(
    program: &RirProgram,
    imports: &ImportedSignatures,
) -> ActorIrProgram {
    let mut actors = Vec::new();
    let context = ActorLowerContext {
        functions: function_type_map(program),
        externals: external_type_map(program),
        imports,
    };
    for function in &program.functions {
        let mut locals = function
            .params
            .iter()
            .zip(&function.param_types)
            .map(|(name, type_)| (name.as_str().to_owned(), ActorSlotType::from_rsig(type_)))
            .collect::<BTreeMap<_, _>>();
        collect_actors_from_block(
            &function.name,
            &function.body,
            &mut locals,
            &context,
            &mut actors,
        );
    }
    ActorIrProgram {
        module_name: program.module_name.clone(),
        actors,
    }
}

pub(crate) fn module_function_symbol(module: &str, name: &str) -> String {
    format!("riot_mod_{module}_{name}")
}

struct TypeContext<'a> {
    bindings: BTreeMap<String, RsigType>,
    functions: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    imports: &'a ImportedSignatures,
}

fn infer_ast_function_types(
    functions: &[crate::ast::AstFnDecl],
) -> BTreeMap<String, (Vec<RsigType>, RsigType)> {
    let mut signatures = functions
        .iter()
        .map(|function| {
            let params = function
                .params
                .iter()
                .enumerate()
                .map(|(index, _)| {
                    function
                        .param_types
                        .get(index)
                        .and_then(|annotation| annotation.as_ref())
                        .map(|annotation| parse_type_signature(&annotation.text).1)
                        .unwrap_or(RsigType::Unknown)
                })
                .collect::<Vec<_>>();
            let result = function
                .return_type
                .as_ref()
                .map(|annotation| parse_type_signature(&annotation.text).1)
                .unwrap_or(RsigType::Unknown);
            (function.name.clone(), (params, result))
        })
        .collect::<BTreeMap<_, _>>();

    for _ in 0..4 {
        let previous = signatures.clone();
        for function in functions {
            let (mut param_types, previous_result) =
                previous.get(&function.name).cloned().unwrap_or_else(|| {
                    (
                        vec![RsigType::Unknown; function.params.len()],
                        RsigType::Unknown,
                    )
                });
            for (index, annotation) in function.param_types.iter().enumerate() {
                if let Some(annotation) = annotation
                    && let Some(param_type) = param_types.get_mut(index)
                {
                    *param_type = parse_type_signature(&annotation.text).1;
                }
            }
            let mut locals = function
                .params
                .iter()
                .zip(&param_types)
                .map(|(name, type_)| (name.clone(), type_.clone()))
                .collect::<BTreeMap<_, _>>();
            let result = infer_ast_block_type(
                &function.body,
                &function.params,
                &mut locals,
                &mut param_types,
                &previous,
            );
            let result = function
                .return_type
                .as_ref()
                .map(|annotation| parse_type_signature(&annotation.text).1)
                .unwrap_or(result);
            signatures.insert(
                function.name.clone(),
                (param_types, merge_types(previous_result, result)),
            );
        }
        if signatures == previous {
            break;
        }
    }

    signatures
}

fn infer_ast_block_type(
    block: &AstBlock,
    params: &[String],
    locals: &mut BTreeMap<String, RsigType>,
    param_types: &mut [RsigType],
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) -> RsigType {
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                mark_ast_constraints(value, params, locals, param_types, functions);
                let inferred = type_annotation
                    .as_ref()
                    .map(|annotation| parse_type_signature(&annotation.text).1)
                    .unwrap_or_else(|| infer_ast_expr_type(value, locals, functions));
                locals.insert(name.clone(), inferred);
            }
            AstStmt::Expr(expr) => {
                mark_ast_constraints(expr, params, locals, param_types, functions);
            }
        }
    }
    if let Some(tail) = &block.tail {
        mark_ast_constraints(tail, params, locals, param_types, functions);
        infer_ast_expr_type(tail, locals, functions)
    } else {
        RsigType::Unit
    }
}

fn infer_ast_expr_type(
    expr: &AstExpr,
    locals: &BTreeMap<String, RsigType>,
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) -> RsigType {
    match expr {
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. } => {
            let _ = lhs;
            let _ = rhs;
            RsigType::I64
        }
        AstExpr::Neg { .. } => RsigType::I64,
        AstExpr::Eq { lhs, rhs, .. } => {
            infer_ast_expr_type(lhs, locals, functions);
            infer_ast_expr_type(rhs, locals, functions);
            RsigType::Bool
        }
        AstExpr::Lt { lhs, rhs, .. } => {
            let _ = lhs;
            let _ = rhs;
            RsigType::Bool
        }
        AstExpr::And { lhs, rhs, .. } | AstExpr::Or { lhs, rhs, .. } => {
            infer_ast_expr_type(lhs, locals, functions);
            infer_ast_expr_type(rhs, locals, functions);
            RsigType::Bool
        }
        AstExpr::Not { expr, .. } => {
            infer_ast_expr_type(expr, locals, functions);
            RsigType::Bool
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            infer_ast_expr_type(condition, locals, functions);
            merge_types(
                infer_ast_expr_type(then_branch, locals, functions),
                infer_ast_expr_type(else_branch, locals, functions),
            )
        }
        AstExpr::Bool { .. } => RsigType::Bool,
        AstExpr::Call { callee, args, .. } => {
            for arg in args {
                infer_ast_expr_type(arg, locals, functions);
            }
            match callee.segments.as_slice() {
                [name] if name == "dbg" || name == "println" => RsigType::Unit,
                [name] if name == "send" || name == "monitor" || name == "link" => RsigType::Unit,
                [name] if name == "list_len" || name == "string_len" => RsigType::I64,
                [name] if name == "string_concat" => RsigType::String,
                [name] if name == "list_get" => RsigType::Unknown,
                [name] => functions
                    .get(name)
                    .map(|(_, result)| result.clone())
                    .unwrap_or(RsigType::Unknown),
                _ => RsigType::Unknown,
            }
        }
        AstExpr::Apply { callee, args, .. } => {
            infer_ast_expr_type(callee, locals, functions);
            for arg in args {
                infer_ast_expr_type(arg, locals, functions);
            }
            RsigType::Unknown
        }
        AstExpr::Lambda { body, .. } => {
            let mut nested_locals = locals.clone();
            let mut nested_params = Vec::new();
            infer_ast_block_type(body, &[], &mut nested_locals, &mut nested_params, functions);
            RsigType::Unknown
        }
        AstExpr::Spawn { .. } => RsigType::ActorId(Box::new(RsigType::Unknown)),
        AstExpr::Receive { body, .. } => {
            infer_ast_expr_type(body, locals, functions);
            RsigType::Unit
        }
        AstExpr::Unit { .. } => RsigType::Unit,
        AstExpr::Tuple { items, .. } => RsigType::Tuple(
            items
                .iter()
                .map(|item| infer_ast_expr_type(item, locals, functions))
                .collect(),
        ),
        AstExpr::List { items, .. } => RsigType::List(Box::new(
            items
                .first()
                .map(|item| infer_ast_expr_type(item, locals, functions))
                .unwrap_or(RsigType::Unknown),
        )),
        AstExpr::Record { path, fields, .. } => {
            for (_, value) in fields {
                infer_ast_expr_type(value, locals, functions);
            }
            RsigType::Record(path.segments.join("."))
        }
        AstExpr::Field { base, field, .. } => infer_ast_field_type(base, field, locals, functions),
        AstExpr::TupleIndex { base, index, .. } => {
            infer_ast_tuple_index_type(base, *index, locals, functions)
        }
        AstExpr::Char { .. } => RsigType::Char,
        AstExpr::Float { .. } => RsigType::F64,
        AstExpr::Int { .. } => RsigType::I64,
        AstExpr::Path { path, .. } => path
            .segments
            .first()
            .and_then(|name| locals.get(name))
            .cloned()
            .unwrap_or(RsigType::Unknown),
        AstExpr::String { .. } => RsigType::String,
    }
}

fn infer_ast_field_type(
    base: &AstExpr,
    field: &str,
    locals: &BTreeMap<String, RsigType>,
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) -> RsigType {
    if let AstExpr::Record { fields, .. } = base {
        return fields
            .iter()
            .find_map(|(name, value)| {
                (name == field).then(|| infer_ast_expr_type(value, locals, functions))
            })
            .unwrap_or(RsigType::Unknown);
    }
    RsigType::Unknown
}

fn infer_ast_tuple_index_type(
    base: &AstExpr,
    index: usize,
    locals: &BTreeMap<String, RsigType>,
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) -> RsigType {
    if let AstExpr::Tuple { items, .. } = base {
        return items
            .get(index)
            .map(|item| infer_ast_expr_type(item, locals, functions))
            .unwrap_or(RsigType::Unknown);
    }
    RsigType::Unknown
}

fn mark_ast_constraints(
    expr: &AstExpr,
    params: &[String],
    locals: &mut BTreeMap<String, RsigType>,
    param_types: &mut [RsigType],
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) {
    match expr {
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. }
        | AstExpr::Lt { lhs, rhs, .. } => {
            mark_ast_expr_as(params, locals, param_types, lhs, RsigType::I64);
            mark_ast_expr_as(params, locals, param_types, rhs, RsigType::I64);
        }
        AstExpr::Neg { expr, .. } => {
            mark_ast_expr_as(params, locals, param_types, expr, RsigType::I64)
        }
        AstExpr::And { lhs, rhs, .. } | AstExpr::Or { lhs, rhs, .. } => {
            mark_ast_expr_as(params, locals, param_types, lhs, RsigType::Bool);
            mark_ast_expr_as(params, locals, param_types, rhs, RsigType::Bool);
        }
        AstExpr::Not { expr, .. } => {
            mark_ast_expr_as(params, locals, param_types, expr, RsigType::Bool)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            mark_ast_expr_as(params, locals, param_types, condition, RsigType::Bool);
            mark_ast_constraints(then_branch, params, locals, param_types, functions);
            mark_ast_constraints(else_branch, params, locals, param_types, functions);
        }
        AstExpr::Call { args, .. }
        | AstExpr::Tuple { items: args, .. }
        | AstExpr::List { items: args, .. } => {
            for arg in args {
                mark_ast_constraints(arg, params, locals, param_types, functions);
            }
        }
        AstExpr::Apply { callee, args, .. } => {
            mark_ast_constraints(callee, params, locals, param_types, functions);
            for arg in args {
                mark_ast_constraints(arg, params, locals, param_types, functions);
            }
        }
        AstExpr::Lambda {
            params: lambda_params,
            body,
            ..
        } => {
            let mut nested_locals = locals.clone();
            for param in lambda_params {
                nested_locals.insert(param.clone(), RsigType::Unknown);
            }
            let mut nested_params = vec![RsigType::Unknown; lambda_params.len()];
            infer_ast_block_type(
                body,
                lambda_params,
                &mut nested_locals,
                &mut nested_params,
                functions,
            );
        }
        AstExpr::Record { fields, .. } => {
            for (_, value) in fields {
                mark_ast_constraints(value, params, locals, param_types, functions);
            }
        }
        AstExpr::Field { base, .. } | AstExpr::TupleIndex { base, .. } => {
            mark_ast_constraints(base, params, locals, param_types, functions);
        }
        AstExpr::Receive { body, .. } => {
            mark_ast_constraints(body, params, locals, param_types, functions);
        }
        AstExpr::Spawn { body, .. } => {
            let mut nested_locals = BTreeMap::new();
            let mut nested_params = Vec::new();
            infer_ast_block_type(body, &[], &mut nested_locals, &mut nested_params, functions);
        }
        AstExpr::Eq { lhs, rhs, .. } => {
            mark_ast_constraints(lhs, params, locals, param_types, functions);
            mark_ast_constraints(rhs, params, locals, param_types, functions);
        }
        AstExpr::Bool { .. }
        | AstExpr::Unit { .. }
        | AstExpr::Char { .. }
        | AstExpr::Float { .. }
        | AstExpr::Int { .. }
        | AstExpr::Path { .. }
        | AstExpr::String { .. } => {}
    }
}

fn mark_ast_expr_as(
    params: &[String],
    locals: &mut BTreeMap<String, RsigType>,
    param_types: &mut [RsigType],
    expr: &AstExpr,
    type_: RsigType,
) {
    if let AstExpr::Path { path, .. } = expr
        && let [name] = path.segments.as_slice()
    {
        locals.insert(name.clone(), type_.clone());
        if let Some(index) = params.iter().position(|param| param == name) {
            param_types[index] = merge_types(param_types[index].clone(), type_);
        }
    }
}

fn type_block(block: AstBlock, context: &mut TypeContext<'_>) -> TypedBlock {
    let mut statements = Vec::new();
    for stmt in block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                let mut value = type_expr(value, context);
                if let Some(annotation) = type_annotation {
                    value.type_ = parse_type_signature(&annotation.text).1;
                }
                context.bindings.insert(name.clone(), value.type_.clone());
                statements.push(TypedStmt::Let { name, value });
            }
            AstStmt::Expr(expr) => statements.push(TypedStmt::Expr(type_expr(expr, context))),
        }
    }
    let tail = block.tail.map(|tail| type_expr(tail, context));
    let type_ = tail
        .as_ref()
        .map(|tail| tail.type_.clone())
        .unwrap_or(RsigType::Unit);
    TypedBlock {
        statements,
        tail,
        type_,
    }
}

fn type_expr(expr: AstExpr, context: &mut TypeContext<'_>) -> TypedExpr {
    match expr {
        AstExpr::Add { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Add, *lhs, *rhs, context),
        AstExpr::Sub { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Sub, *lhs, *rhs, context),
        AstExpr::Mul { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Mul, *lhs, *rhs, context),
        AstExpr::Div { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Div, *lhs, *rhs, context),
        AstExpr::Mod { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Mod, *lhs, *rhs, context),
        AstExpr::Neg { expr, .. } => TypedExpr {
            type_: RsigType::I64,
            kind: TypedExprKind::Neg(Box::new(type_expr(*expr, context))),
        },
        AstExpr::Eq { lhs, rhs, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Eq(
                Box::new(type_expr(*lhs, context)),
                Box::new(type_expr(*rhs, context)),
            ),
        },
        AstExpr::Lt { lhs, rhs, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Lt(
                Box::new(type_expr(*lhs, context)),
                Box::new(type_expr(*rhs, context)),
            ),
        },
        AstExpr::And { lhs, rhs, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::And(
                Box::new(type_expr(*lhs, context)),
                Box::new(type_expr(*rhs, context)),
            ),
        },
        AstExpr::Or { lhs, rhs, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Or(
                Box::new(type_expr(*lhs, context)),
                Box::new(type_expr(*rhs, context)),
            ),
        },
        AstExpr::Not { expr, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Not(Box::new(type_expr(*expr, context))),
        },
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let condition = type_expr(*condition, context);
            let then_branch = type_expr(*then_branch, context);
            let else_branch = type_expr(*else_branch, context);
            let type_ = merge_types(then_branch.type_.clone(), else_branch.type_.clone());
            TypedExpr {
                type_,
                kind: TypedExprKind::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
            }
        }
        AstExpr::Bool { value, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Bool(value),
        },
        AstExpr::Call { callee, args, .. } => {
            if is_named_call(&callee.segments, context) {
                let type_ = call_result_type(&callee.segments, context);
                TypedExpr {
                    type_,
                    kind: TypedExprKind::Call {
                        callee: callee.segments,
                        args: args
                            .into_iter()
                            .map(|arg| type_expr(arg, context))
                            .collect(),
                    },
                }
            } else {
                let callee = TypedExpr {
                    type_: path_type(&callee.segments, context),
                    kind: TypedExprKind::Path(callee.segments),
                };
                TypedExpr {
                    type_: RsigType::Unknown,
                    kind: TypedExprKind::Apply {
                        callee: Box::new(callee),
                        args: args
                            .into_iter()
                            .map(|arg| type_expr(arg, context))
                            .collect(),
                    },
                }
            }
        }
        AstExpr::Apply { callee, args, .. } => {
            let callee = type_expr(*callee, context);
            TypedExpr {
                type_: RsigType::Unknown,
                kind: TypedExprKind::Apply {
                    callee: Box::new(callee),
                    args: args
                        .into_iter()
                        .map(|arg| type_expr(arg, context))
                        .collect(),
                },
            }
        }
        AstExpr::Lambda {
            params,
            param_types,
            body,
            ..
        } => {
            let saved_bindings = context.bindings.clone();
            let typed_params = params
                .iter()
                .enumerate()
                .map(|(index, name)| {
                    let type_ = param_types
                        .get(index)
                        .and_then(|annotation| annotation.as_ref())
                        .map(|annotation| parse_type_signature(&annotation.text).1)
                        .unwrap_or(RsigType::Unknown);
                    context.bindings.insert(name.clone(), type_.clone());
                    TypedParam {
                        name: name.clone(),
                        type_,
                    }
                })
                .collect::<Vec<_>>();
            let body = type_block(*body, context);
            context.bindings = saved_bindings;
            TypedExpr {
                type_: RsigType::Unknown,
                kind: TypedExprKind::Lambda {
                    params: typed_params,
                    body: Box::new(body),
                },
            }
        }
        AstExpr::Spawn { body, .. } => TypedExpr {
            type_: RsigType::ActorId(Box::new(RsigType::Unknown)),
            kind: {
                let saved_bindings = context.bindings.clone();
                let body = type_block(*body, context);
                context.bindings = saved_bindings;
                TypedExprKind::Spawn {
                    body: Box::new(body),
                }
            },
        },
        AstExpr::Receive { binder, body, .. } => {
            let previous = context.bindings.insert(binder.clone(), RsigType::Unknown);
            let body = type_expr(*body, context);
            if let Some(previous) = previous {
                context.bindings.insert(binder.clone(), previous);
            } else {
                context.bindings.remove(&binder);
            }
            TypedExpr {
                type_: RsigType::Unit,
                kind: TypedExprKind::Receive {
                    binder,
                    body: Box::new(body),
                },
            }
        }
        AstExpr::Unit { .. } => TypedExpr {
            type_: RsigType::Unit,
            kind: TypedExprKind::Unit,
        },
        AstExpr::Tuple { items, .. } => {
            let items = items
                .into_iter()
                .map(|item| type_expr(item, context))
                .collect::<Vec<_>>();
            let type_ = RsigType::Tuple(items.iter().map(|item| item.type_.clone()).collect());
            TypedExpr {
                type_,
                kind: TypedExprKind::Tuple(items),
            }
        }
        AstExpr::List { items, .. } => {
            let items = items
                .into_iter()
                .map(|item| type_expr(item, context))
                .collect::<Vec<_>>();
            let type_ = RsigType::List(Box::new(
                items
                    .first()
                    .map(|item| item.type_.clone())
                    .unwrap_or(RsigType::Unknown),
            ));
            TypedExpr {
                type_,
                kind: TypedExprKind::List(items),
            }
        }
        AstExpr::Record { path, fields, .. } => {
            let fields = fields
                .into_iter()
                .map(|(name, value)| (name, type_expr(value, context)))
                .collect::<Vec<_>>();
            TypedExpr {
                type_: RsigType::Record(path.segments.join(".")),
                kind: TypedExprKind::Record {
                    path: path.segments,
                    fields,
                },
            }
        }
        AstExpr::Field { base, field, .. } => {
            let base = type_expr(*base, context);
            let type_ = typed_field_type(&base, &field);
            TypedExpr {
                type_,
                kind: TypedExprKind::Field {
                    base: Box::new(base),
                    field,
                },
            }
        }
        AstExpr::TupleIndex { base, index, .. } => {
            let base = type_expr(*base, context);
            let type_ = typed_tuple_index_type(&base, index);
            TypedExpr {
                type_,
                kind: TypedExprKind::TupleIndex {
                    base: Box::new(base),
                    index,
                },
            }
        }
        AstExpr::Char { value, .. } => TypedExpr {
            type_: RsigType::Char,
            kind: TypedExprKind::Char(value),
        },
        AstExpr::Float { value, .. } => TypedExpr {
            type_: RsigType::F64,
            kind: TypedExprKind::Float(value),
        },
        AstExpr::Int { value, .. } => TypedExpr {
            type_: RsigType::I64,
            kind: TypedExprKind::Int(value),
        },
        AstExpr::Path { path, .. } => TypedExpr {
            type_: path_type(&path.segments, context),
            kind: TypedExprKind::Path(path.segments),
        },
        AstExpr::String { value, .. } => TypedExpr {
            type_: RsigType::String,
            kind: TypedExprKind::String(value),
        },
    }
}

fn typed_field_type(base: &TypedExpr, field: &str) -> RsigType {
    if let TypedExprKind::Record { fields, .. } = &base.kind {
        return fields
            .iter()
            .find_map(|(name, value)| (name == field).then(|| value.type_.clone()))
            .unwrap_or(RsigType::Unknown);
    }
    RsigType::Unknown
}

fn typed_tuple_index_type(base: &TypedExpr, index: usize) -> RsigType {
    if let TypedExprKind::Tuple(items) = &base.kind {
        return items
            .get(index)
            .map(|item| item.type_.clone())
            .unwrap_or(RsigType::Unknown);
    }
    RsigType::Unknown
}

fn typed_binary_i64(
    build: fn(Box<TypedExpr>, Box<TypedExpr>) -> TypedExprKind,
    lhs: AstExpr,
    rhs: AstExpr,
    context: &mut TypeContext<'_>,
) -> TypedExpr {
    TypedExpr {
        type_: RsigType::I64,
        kind: build(
            Box::new(type_expr(lhs, context)),
            Box::new(type_expr(rhs, context)),
        ),
    }
}

fn is_named_call(callee: &[String], context: &TypeContext<'_>) -> bool {
    match callee {
        [name] if context.bindings.contains_key(name) => false,
        [name]
            if matches!(
                name.as_str(),
                "dbg"
                    | "println"
                    | "send"
                    | "monitor"
                    | "link"
                    | "list_len"
                    | "list_get"
                    | "string_len"
                    | "string_concat"
            ) =>
        {
            true
        }
        [name] => context.externals.contains_key(name) || context.functions.contains_key(name),
        [module, name] => context
            .imports
            .get(module)
            .and_then(|rsig| rsig.find(name))
            .is_some(),
        _ => false,
    }
}

fn call_result_type(callee: &[String], context: &TypeContext<'_>) -> RsigType {
    match callee {
        [name] if name == "dbg" || name == "println" => RsigType::Unit,
        [name] if name == "send" || name == "monitor" || name == "link" => RsigType::Unit,
        [name] => context
            .externals
            .get(name)
            .or_else(|| context.functions.get(name))
            .map(|(_, result)| result.clone())
            .unwrap_or_else(|| match name.as_str() {
                "list_len" | "string_len" => RsigType::I64,
                "string_concat" => RsigType::String,
                "list_get" => RsigType::Unknown,
                _ => RsigType::Unknown,
            }),
        [module, name] => context
            .imports
            .get(module)
            .and_then(|rsig| rsig.find(name))
            .map(|export| match export {
                RsigExport::Function(function) => function.result.clone(),
                RsigExport::External(external) => external.result.clone(),
            })
            .unwrap_or(RsigType::Unknown),
        _ => RsigType::Unknown,
    }
}

fn path_type(path: &[String], context: &TypeContext<'_>) -> RsigType {
    let Some((head, tail)) = path.split_first() else {
        return RsigType::Unknown;
    };
    if tail.is_empty() {
        context
            .bindings
            .get(head)
            .cloned()
            .unwrap_or(RsigType::Unknown)
    } else {
        RsigType::Unknown
    }
}

fn merge_types(lhs: RsigType, rhs: RsigType) -> RsigType {
    if lhs == rhs {
        lhs
    } else if matches!(lhs, RsigType::Unknown) {
        rhs
    } else if matches!(rhs, RsigType::Unknown) {
        lhs
    } else {
        RsigType::Unknown
    }
}

#[derive(Default)]
struct LowerContext {
    next_actor_id: usize,
}

impl LowerContext {
    fn next_actor_id(&mut self) -> usize {
        let id = self.next_actor_id;
        self.next_actor_id += 1;
        id
    }
}

struct ActorLowerContext<'a> {
    functions: BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: BTreeMap<String, (Vec<RsigType>, RsigType)>,
    imports: &'a ImportedSignatures,
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

fn collect_actors_from_block(
    owner: &str,
    block: &RirBlock,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
    actors: &mut Vec<ActorIrActor>,
) {
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                collect_actors_from_expr(owner, value, locals, context, actors);
                locals.insert(name.clone(), infer_actor_slot_type(value, locals, context));
            }
            RirStmt::Expr(expr) => collect_actors_from_expr(owner, expr, locals, context, actors),
        }
    }
    if let Some(tail) = &block.tail {
        collect_actors_from_expr(owner, tail, locals, context, actors);
    }
}

fn collect_actors_from_expr(
    owner: &str,
    expr: &RirExpr,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
    actors: &mut Vec<ActorIrActor>,
) {
    match expr {
        RirExpr::Spawn { actor_id, body } => {
            let actor = actor_frame_from_block(*actor_id, owner, body, locals, context);
            let mut actor_locals = actor
                .frame
                .slots
                .iter()
                .map(|slot| (slot.name.clone(), Some(slot.type_)))
                .collect::<BTreeMap<_, _>>();
            actors.push(actor);
            collect_actors_from_block(owner, body, &mut actor_locals, context, actors);
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
            collect_actors_from_expr(owner, lhs, locals, context, actors);
            collect_actors_from_expr(owner, rhs, locals, context, actors);
        }
        RirExpr::Neg(value) | RirExpr::Not(value) => {
            collect_actors_from_expr(owner, value, locals, context, actors);
        }
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_actors_from_expr(owner, condition, locals, context, actors);
            collect_actors_from_expr(owner, then_branch, locals, context, actors);
            collect_actors_from_expr(owner, else_branch, locals, context, actors);
        }
        RirExpr::Call { args, .. } | RirExpr::Tuple(args) | RirExpr::List(args) => {
            for arg in args {
                collect_actors_from_expr(owner, arg, locals, context, actors);
            }
        }
        RirExpr::Apply { callee, args } => {
            collect_actors_from_expr(owner, callee, locals, context, actors);
            for arg in args {
                collect_actors_from_expr(owner, arg, locals, context, actors);
            }
        }
        RirExpr::Lambda { params, body, .. } => {
            let mut lambda_locals = locals.clone();
            for param in params {
                lambda_locals.insert(param.as_str().to_owned(), None);
            }
            collect_actors_from_block(owner, body, &mut lambda_locals, context, actors);
        }
        RirExpr::Record { fields, .. } => {
            for (_, value) in fields {
                collect_actors_from_expr(owner, value, locals, context, actors);
            }
        }
        RirExpr::Field { base, .. } | RirExpr::TupleIndex { base, .. } => {
            collect_actors_from_expr(owner, base, locals, context, actors);
        }
        RirExpr::Receive { binder, body } => {
            let previous = locals.insert(binder.clone(), None);
            collect_actors_from_expr(owner, body, locals, context, actors);
            if let Some(previous) = previous {
                locals.insert(binder.clone(), previous);
            } else {
                locals.remove(binder);
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

fn actor_frame_from_block(
    actor_id: usize,
    owner: &str,
    block: &RirBlock,
    outer_locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
) -> ActorIrActor {
    let ops = actor_ops(block);
    let mut bound = BTreeSet::new();
    let mut free = BTreeSet::new();
    for op in &ops {
        match &op {
            ActorFrameOp::Let { name, value } => {
                collect_free_expr(value, &bound, &mut free);
                bound.insert(name.clone());
            }
            ActorFrameOp::Expr(expr) => collect_free_expr(expr, &bound, &mut free),
            ActorFrameOp::Receive { binder, body } => {
                let mut receive_bound = bound.clone();
                receive_bound.insert(binder.clone());
                collect_free_expr(body, &receive_bound, &mut free);
            }
        }
    }

    let mut slots = Vec::new();
    for name in free {
        if let Some(Some(type_)) = outer_locals.get(&name) {
            slots.push(ActorFrameSlot {
                name,
                type_: *type_,
                field_index: slots.len() as u32 + 1,
            });
        }
    }
    let captures = slots.clone();

    let mut local_types = slots
        .iter()
        .map(|slot| (slot.name.clone(), Some(slot.type_)))
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
                local_types.insert(name.clone(), Some(type_));
            } else {
                local_types.insert(name.clone(), None);
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
            ActorFrameState { index, op, next }
        })
        .collect::<Vec<_>>();

    ActorIrActor {
        id: actor_id,
        owner: owner.to_owned(),
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
                name: name.clone(),
                value: value.clone(),
            }),
            RirStmt::Expr(RirExpr::Receive { binder, body }) => {
                ops.push(ActorFrameOp::Receive {
                    binder: binder.clone(),
                    body: *body.clone(),
                });
            }
            RirStmt::Expr(expr) => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    if let Some(expr) = &block.tail {
        match expr {
            RirExpr::Receive { binder, body } => {
                ops.push(ActorFrameOp::Receive {
                    binder: binder.clone(),
                    body: *body.clone(),
                });
            }
            expr => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    ops
}

fn collect_free_expr(expr: &RirExpr, bound: &BTreeSet<String>, free: &mut BTreeSet<String>) {
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
        RirExpr::Call { args, .. } | RirExpr::Tuple(args) | RirExpr::List(args) => {
            for arg in args {
                collect_free_expr(arg, bound, free);
            }
        }
        RirExpr::Apply { callee, args } => {
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
        RirExpr::Receive { binder, body } => {
            let mut nested = bound.clone();
            nested.insert(binder.clone());
            collect_free_expr(body, &nested, free);
        }
        RirExpr::Bool(_)
        | RirExpr::Unit
        | RirExpr::Char(_)
        | RirExpr::Float(_)
        | RirExpr::Int(_)
        | RirExpr::String(_) => {}
    }
}

fn collect_free_block(
    block: &RirBlock,
    outer_bound: &BTreeSet<String>,
    free: &mut BTreeSet<String>,
) {
    let mut bound = outer_bound.clone();
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                collect_free_expr(value, &bound, free);
                bound.insert(name.clone());
            }
            RirStmt::Expr(expr) => collect_free_expr(expr, &bound, free),
        }
    }
    if let Some(tail) = &block.tail {
        collect_free_expr(tail, &bound, free);
    }
}

fn infer_actor_slot_type(
    expr: &RirExpr,
    locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
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
                .get(module)
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
        | RirExpr::Apply { .. }
        | RirExpr::Lambda { .. }
        | RirExpr::Record { .. }
        | RirExpr::Field { .. }
        | RirExpr::TupleIndex { .. }
        | RirExpr::String(_) => Some(ActorSlotType::Value),
        RirExpr::Unit | RirExpr::Receive { .. } | RirExpr::Char(_) | RirExpr::Float(_) => None,
    }
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

fn lower_block(block: TypedBlock, context: &mut LowerContext) -> RirBlock {
    RirBlock {
        statements: block
            .statements
            .into_iter()
            .map(|stmt| match stmt {
                TypedStmt::Let { name, value } => RirStmt::Let {
                    name,
                    value: lower_expr(value, context),
                },
                TypedStmt::Expr(expr) => RirStmt::Expr(lower_expr(expr, context)),
            })
            .collect(),
        tail: block.tail.map(|tail| lower_expr(tail, context)),
    }
}

fn lower_expr(expr: TypedExpr, context: &mut LowerContext) -> RirExpr {
    match expr.kind {
        TypedExprKind::Add(lhs, rhs) => RirExpr::Add(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Sub(lhs, rhs) => RirExpr::Sub(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Mul(lhs, rhs) => RirExpr::Mul(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Div(lhs, rhs) => RirExpr::Div(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Mod(lhs, rhs) => RirExpr::Mod(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Neg(expr) => RirExpr::Neg(Box::new(lower_expr(*expr, context))),
        TypedExprKind::Eq(lhs, rhs) => RirExpr::Eq(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Lt(lhs, rhs) => RirExpr::Lt(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::And(lhs, rhs) => RirExpr::And(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Or(lhs, rhs) => RirExpr::Or(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Not(expr) => RirExpr::Not(Box::new(lower_expr(*expr, context))),
        TypedExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => RirExpr::If {
            condition: Box::new(lower_expr(*condition, context)),
            then_branch: Box::new(lower_expr(*then_branch, context)),
            else_branch: Box::new(lower_expr(*else_branch, context)),
        },
        TypedExprKind::Bool(value) => RirExpr::Bool(value),
        TypedExprKind::Call { callee, args } => RirExpr::Call {
            callee,
            args: args
                .into_iter()
                .map(|arg| lower_expr(arg, context))
                .collect(),
        },
        TypedExprKind::Apply { callee, args } => RirExpr::Apply {
            callee: Box::new(lower_expr(*callee, context)),
            args: args
                .into_iter()
                .map(|arg| lower_expr(arg, context))
                .collect(),
        },
        TypedExprKind::Lambda { params, body } => {
            let params = params
                .into_iter()
                .map(|param| Param::new(param.name))
                .collect::<Vec<_>>();
            let body = lower_block(*body, context);
            let bound = params
                .iter()
                .map(|param| param.as_str().to_owned())
                .collect::<BTreeSet<_>>();
            let mut free = BTreeSet::new();
            collect_free_block(&body, &bound, &mut free);
            RirExpr::Lambda {
                params,
                captures: free.into_iter().map(Param::new).collect(),
                body: Box::new(body),
            }
        }
        TypedExprKind::Spawn { body } => RirExpr::Spawn {
            actor_id: context.next_actor_id(),
            body: Box::new(lower_block(*body, context)),
        },
        TypedExprKind::Receive { binder, body } => RirExpr::Receive {
            binder,
            body: Box::new(lower_expr(*body, context)),
        },
        TypedExprKind::Unit => RirExpr::Unit,
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
            path,
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
        TypedExprKind::Char(value) => RirExpr::Char(value),
        TypedExprKind::Float(value) => RirExpr::Float(value),
        TypedExprKind::Int(value) => RirExpr::Int(value),
        TypedExprKind::Path(path) => RirExpr::Path(path),
        TypedExprKind::String(value) => RirExpr::String(value),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{AstBlock, AstDecl, AstExpr, AstFnDecl, AstPath, AstProgram, TextSpan};
    use crate::signature::ImportedSignatures;

    use super::{Param, RirExpr, lower_typed_to_rir, typed_program_from_ast};

    fn span() -> TextSpan {
        TextSpan::new(0, 0)
    }

    fn path(name: &str) -> AstExpr {
        AstExpr::Path {
            path: AstPath {
                segments: vec![name.to_owned()],
            },
            span: span(),
        }
    }

    #[test]
    fn rir_lambdas_carry_capture_names() {
        let program = AstProgram {
            decls: vec![AstDecl::Function(AstFnDecl {
                name: "make_adder".to_owned(),
                name_span: span(),
                params: vec!["n".to_owned()],
                param_types: Vec::new(),
                return_type: None,
                body: AstBlock {
                    statements: Vec::new(),
                    tail: Some(AstExpr::Lambda {
                        params: vec!["x".to_owned()],
                        param_types: Vec::new(),
                        body: Box::new(AstBlock {
                            statements: Vec::new(),
                            tail: Some(AstExpr::Add {
                                lhs: Box::new(path("x")),
                                rhs: Box::new(path("n")),
                                span: span(),
                            }),
                            span: span(),
                        }),
                        span: span(),
                    }),
                    span: span(),
                },
                span: span(),
            })],
        };

        let typed =
            typed_program_from_ast("LambdaTest".to_owned(), program, &ImportedSignatures::new());
        let rir = lower_typed_to_rir(typed);
        let Some(RirExpr::Lambda { captures, .. }) = &rir.functions[0].body.tail else {
            panic!("expected lowered lambda");
        };

        assert_eq!(captures.as_slice(), &[Param::new("n")]);
    }
}

#[allow(dead_code)]
fn _keep_ast_path_used(_: &AstPath) {}
