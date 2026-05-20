use std::collections::{HashMap, HashSet};

use camino::Utf8Path;

mod const_eval;
mod span;
mod types;

use const_eval::{ConstFunction, ConstValue, resolve_const_value};
use span::expr_span;
use types::parse_primitive_type;

use crate::ast::{AstBlock, AstDecl, AstExpr, AstProgram, AstStmt, AstTypeAnnotation, TextSpan};
use crate::diagnostic::to_source_diagnostic;
use crate::infer::module::infer_program;
use crate::ir::{CheckedProgram, signature_for, typed_program_from_ast};
use crate::signature::{ImportedSignatures, RsigExport, RsigType, parse_type};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CheckMode {
    Executable,
    Interface,
}

pub(crate) fn typecheck(
    source_path: &Utf8Path,
    source: &str,
    module_name: String,
    program: AstProgram,
    imports: &ImportedSignatures,
    mode: CheckMode,
) -> miette::Result<CheckedProgram> {
    validate_program(source_path, source, &program, imports, mode)?;
    let inferred = infer_program(&program, imports).map_err(|error| {
        to_source_diagnostic(
            source_path,
            source,
            first_decl_span(&program).unwrap_or_else(|| TextSpan::new(0, source.len().min(1))),
            "type inference failed",
            error.to_string(),
            Some("fix the type error before lowering"),
        )
    })?;
    let function_types = inferred.function_signatures(&program);
    let expression_types = inferred.expression_rsig_types();
    let typed_tree = typed_program_from_ast(
        module_name,
        program,
        imports,
        &function_types,
        Some(&expression_types),
    );
    let signature = signature_for(&typed_tree);
    Ok(CheckedProgram {
        typed_tree,
        signature,
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BindingKind {
    Actor,
    Value(Option<RsigType>),
    Message,
}

#[derive(Debug)]
struct ValidationContext<'a> {
    source_path: &'a Utf8Path,
    source: &'a str,
    functions: HashMap<String, ConstFunction>,
    function_names: HashSet<String>,
    function_results: HashMap<String, RsigType>,
    declared_external_names: HashSet<String>,
    external_names: HashSet<String>,
    imports: &'a ImportedSignatures,
}

fn validate_program(
    source_path: &Utf8Path,
    source: &str,
    program: &AstProgram,
    imports: &ImportedSignatures,
    mode: CheckMode,
) -> miette::Result<()> {
    let functions = const_functions(program);
    let function_names = functions.keys().cloned().collect::<HashSet<_>>();
    let function_results = function_results(program);
    let declared_external_names = declared_external_names(program);
    let external_names = external_names(program);
    let ctx = ValidationContext {
        source_path,
        source,
        functions,
        function_names,
        function_results,
        declared_external_names,
        external_names,
        imports,
    };

    let main_decls = program
        .decls
        .iter()
        .filter_map(|decl| match decl {
            AstDecl::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .collect::<Vec<_>>();

    if main_decls.len() > 1 {
        let duplicate = main_decls[1];
        return Err(to_source_diagnostic(
            source_path,
            source,
            duplicate.span,
            "duplicate main function",
            "stage0 requires a single main function per file",
            Some("keep only one `fn main() { ... }`"),
        )
        .into());
    }

    if mode == CheckMode::Executable && main_decls.is_empty() {
        let span =
            first_decl_span(program).unwrap_or_else(|| TextSpan::new(0, source.len().min(1)));
        return Err(to_source_diagnostic(
            source_path,
            source,
            span,
            "missing main function",
            "stage0 compile requires one entrypoint named main",
            Some("add: fn main() { dbg(\"hello world\") }"),
        )
        .into());
    }

    for decl in &program.decls {
        match decl {
            AstDecl::Use(use_) => {
                if !imports.contains_key(&use_.name) {
                    return Err(to_source_diagnostic(
                        source_path,
                        source,
                        use_.name_span,
                        "missing imported signature",
                        format!("`use {}` did not resolve to a signature", use_.name),
                        Some("pass --sig-dir with the directory containing the .rsig file"),
                    )
                    .into());
                }
            }
            AstDecl::External(external) => {
                if external.type_text.trim().is_empty() {
                    return Err(to_source_diagnostic(
                        source_path,
                        source,
                        external.type_span,
                        "empty external type",
                        "external declarations need a source-level type",
                        Some("try: external println : string -> unit = \"riot_prim_println\""),
                    )
                    .into());
                }
            }
            AstDecl::Function(function) => {
                validate_function_annotations(&ctx, function)?;
                if function.name == "main" && !function.params.is_empty() {
                    return Err(to_source_diagnostic(
                        source_path,
                        source,
                        function.name_span,
                        "main function cannot have parameters",
                        "stage0 requires main to have no parameters",
                        Some("try: fn main() { dbg(\"hello world\") }"),
                    )
                    .into());
                }
                validate_block(
                    &ctx,
                    &function.body,
                    function.name == "main",
                    &function.params,
                    &function.param_types,
                )?;
            }
        }
    }

    Ok(())
}

fn function_results(program: &AstProgram) -> HashMap<String, RsigType> {
    program
        .decls
        .iter()
        .filter_map(|decl| match decl {
            AstDecl::Function(function) => function
                .return_type
                .as_ref()
                .map(|annotation| (function.name.clone(), parse_type(&annotation.text))),
            _ => None,
        })
        .collect()
}

fn const_functions(program: &AstProgram) -> HashMap<String, ConstFunction> {
    program
        .decls
        .iter()
        .filter_map(|decl| match decl {
            AstDecl::Function(function) if function.name != "main" => Some((
                function.name.clone(),
                ConstFunction {
                    params: function.params.clone(),
                    body: function.body.clone(),
                },
            )),
            _ => None,
        })
        .collect()
}

fn external_names(program: &AstProgram) -> HashSet<String> {
    let mut names = HashSet::from([
        "dbg".to_owned(),
        "println".to_owned(),
        "send".to_owned(),
        "monitor".to_owned(),
        "link".to_owned(),
        "list_len".to_owned(),
        "list_get".to_owned(),
        "string_len".to_owned(),
        "string_concat".to_owned(),
    ]);
    for decl in &program.decls {
        if let AstDecl::External(external) = decl {
            names.insert(external.name.clone());
        }
    }
    names
}

fn declared_external_names(program: &AstProgram) -> HashSet<String> {
    program
        .decls
        .iter()
        .filter_map(|decl| match decl {
            AstDecl::External(external) => Some(external.name.clone()),
            _ => None,
        })
        .collect()
}

fn first_decl_span(program: &AstProgram) -> Option<TextSpan> {
    program.decls.first().map(|decl| match decl {
        AstDecl::Use(use_) => use_.span,
        AstDecl::External(external) => external.span,
        AstDecl::Function(function) => function.span,
    })
}

fn validate_block(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    is_main: bool,
    params: &[String],
    param_types: &[Option<AstTypeAnnotation>],
) -> miette::Result<()> {
    let mut bindings = params
        .iter()
        .enumerate()
        .map(|(index, param)| {
            let type_ = param_types
                .get(index)
                .and_then(|annotation| annotation.as_ref())
                .map(|annotation| parse_type(&annotation.text));
            (param.clone(), BindingKind::Value(type_))
        })
        .collect::<HashMap<_, _>>();
    let mut output_actions = 0_usize;
    let mut actor_actions = 0_usize;

    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                name_span: _,
                type_annotation,
                value,
                span: _,
            } => {
                if let Some(annotation) = type_annotation {
                    validate_type_annotation(ctx, annotation, value, &bindings)?;
                }
                let kind = match value {
                    AstExpr::Spawn { body, span: _ } => {
                        validate_actor_block(ctx, body, &bindings)?;
                        actor_actions += 1;
                        BindingKind::Actor
                    }
                    _ => {
                        validate_expr(ctx, value, &bindings, false)?;
                        let type_ = type_annotation
                            .as_ref()
                            .map(|annotation| parse_type(&annotation.text))
                            .or_else(|| simple_expr_type(ctx, value, &bindings));
                        BindingKind::Value(type_)
                    }
                };
                bindings.insert(name.clone(), kind);
            }
            AstStmt::Expr(expr) => {
                let category = validate_expr(ctx, expr, &bindings, false)?;
                match category {
                    ExprCategory::Output => output_actions += 1,
                    ExprCategory::Actor => actor_actions += 1,
                    ExprCategory::Other => {}
                }
            }
        }
    }

    if let Some(tail) = &block.tail {
        let category = validate_expr(ctx, tail, &bindings, false)?;
        match category {
            ExprCategory::Output => output_actions += 1,
            ExprCategory::Actor => actor_actions += 1,
            ExprCategory::Other => {}
        }
    }

    if is_main && output_actions == 0 && actor_actions == 0 {
        return Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            block.span,
            "unsupported main body",
            "stage0 expects main to produce output or actor actions",
            Some("try: fn main() { dbg(\"hello world\") }"),
        )
        .into());
    }

    Ok(())
}

fn validate_scoped_block(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    outer_bindings: &HashMap<String, BindingKind>,
    params: &[String],
    param_types: &[Option<AstTypeAnnotation>],
    in_actor: bool,
) -> miette::Result<()> {
    let mut bindings = outer_bindings.clone();
    for (index, param) in params.iter().enumerate() {
        let type_ = param_types
            .get(index)
            .and_then(|annotation| annotation.as_ref())
            .map(|annotation| parse_type(&annotation.text));
        bindings.insert(param.clone(), BindingKind::Value(type_));
    }

    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                if let Some(annotation) = type_annotation {
                    validate_type_annotation(ctx, annotation, value, &bindings)?;
                }
                let kind = match value {
                    AstExpr::Spawn { body, .. } => {
                        validate_actor_block(ctx, body, &bindings)?;
                        BindingKind::Actor
                    }
                    _ => {
                        validate_expr(ctx, value, &bindings, in_actor)?;
                        let type_ = type_annotation
                            .as_ref()
                            .map(|annotation| parse_type(&annotation.text))
                            .or_else(|| simple_expr_type(ctx, value, &bindings));
                        BindingKind::Value(type_)
                    }
                };
                bindings.insert(name.clone(), kind);
            }
            AstStmt::Expr(expr) => {
                validate_expr(ctx, expr, &bindings, in_actor)?;
            }
        }
    }

    if let Some(tail) = &block.tail {
        validate_expr(ctx, tail, &bindings, in_actor)?;
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprCategory {
    Output,
    Actor,
    Other,
}

fn validate_expr(
    ctx: &ValidationContext<'_>,
    expr: &AstExpr,
    bindings: &HashMap<String, BindingKind>,
    in_actor: bool,
) -> miette::Result<ExprCategory> {
    match expr {
        AstExpr::Call { callee, args, span } => {
            if let [module, name] = callee.segments.as_slice() {
                let Some(rsig) = ctx.imports.get(module) else {
                    return Err(to_source_diagnostic(
                        ctx.source_path,
                        ctx.source,
                        *span,
                        "unknown imported module",
                        format!("`{module}` has not been brought into scope with `use {module}`"),
                        Some("add a top-level `use` declaration and pass --sig-dir"),
                    )
                    .into());
                };
                let Some(export) = rsig.find(name) else {
                    return Err(to_source_diagnostic(
                        ctx.source_path,
                        ctx.source,
                        *span,
                        "unknown imported value",
                        format!("module `{module}` does not export `{name}`"),
                        Some("check the producer .rsig"),
                    )
                    .into());
                };
                if export_has_unknown_abi(export) {
                    return Err(to_source_diagnostic(
                        ctx.source_path,
                        ctx.source,
                        *span,
                        "imported value has unknown ABI",
                        format!(
                            "module `{module}` exports `{name}`, but its .rsig type is not concrete enough to call"
                        ),
                        Some("add enough type information for the exported function"),
                    )
                    .into());
                }
                for arg in args {
                    validate_expr(ctx, arg, bindings, in_actor)?;
                }
                return Ok(ExprCategory::Other);
            }

            let [name] = callee.segments.as_slice() else {
                return Err(to_source_diagnostic(
                    ctx.source_path,
                    ctx.source,
                    *span,
                    "unsupported path call",
                    "stage0 only supports local calls and qualified module calls",
                    Some("try: Module.value(...)"),
                )
                .into());
            };

            match name.as_str() {
                "dbg" => {
                    if args.len() != 1 {
                        return Err(call_arity_error(ctx, *span, "dbg", 1, args.len()).into());
                    }
                    validate_expr(ctx, &args[0], bindings, in_actor)?;
                    Ok(ExprCategory::Output)
                }
                "println" => {
                    if args.len() != 1 {
                        return Err(call_arity_error(ctx, *span, "println", 1, args.len()).into());
                    }
                    validate_expr(ctx, &args[0], bindings, in_actor)?;
                    Ok(ExprCategory::Output)
                }
                "send" => {
                    if args.len() != 2 {
                        return Err(call_arity_error(ctx, *span, "send", 2, args.len()).into());
                    }
                    validate_actor_target(ctx, &args[0], bindings, *span, "send")?;
                    validate_expr(ctx, &args[1], bindings, in_actor)?;
                    Ok(ExprCategory::Actor)
                }
                "monitor" | "link" => {
                    if args.len() != 1 {
                        return Err(call_arity_error(ctx, *span, name, 1, args.len()).into());
                    }
                    validate_actor_target(ctx, &args[0], bindings, *span, name)?;
                    Ok(ExprCategory::Actor)
                }
                "list_len" if !ctx.declared_external_names.contains(name) => {
                    if args.len() != 1 {
                        return Err(call_arity_error(ctx, *span, "list_len", 1, args.len()).into());
                    }
                    validate_expr(ctx, &args[0], bindings, in_actor)?;
                    validate_call_arg_type(
                        ctx,
                        *span,
                        "list_len",
                        &args[0],
                        bindings,
                        "list",
                        |type_| matches!(type_, RsigType::List(_)),
                    )?;
                    Ok(ExprCategory::Other)
                }
                "list_get" if !ctx.declared_external_names.contains(name) => {
                    if args.len() != 2 {
                        return Err(call_arity_error(ctx, *span, "list_get", 2, args.len()).into());
                    }
                    validate_expr(ctx, &args[0], bindings, in_actor)?;
                    validate_expr(ctx, &args[1], bindings, in_actor)?;
                    validate_call_arg_type(
                        ctx,
                        *span,
                        "list_get",
                        &args[0],
                        bindings,
                        "list",
                        |type_| matches!(type_, RsigType::List(_)),
                    )?;
                    validate_call_arg_type(
                        ctx,
                        *span,
                        "list_get",
                        &args[1],
                        bindings,
                        "i64",
                        |type_| matches!(type_, RsigType::I64),
                    )?;
                    validate_static_list_index(ctx, *span, "list_get", &args[0], &args[1])?;
                    Ok(ExprCategory::Other)
                }
                "string_len" if !ctx.declared_external_names.contains(name) => {
                    if args.len() != 1 {
                        return Err(
                            call_arity_error(ctx, *span, "string_len", 1, args.len()).into()
                        );
                    }
                    validate_expr(ctx, &args[0], bindings, in_actor)?;
                    validate_call_arg_type(
                        ctx,
                        *span,
                        "string_len",
                        &args[0],
                        bindings,
                        "string",
                        |type_| matches!(type_, RsigType::String),
                    )?;
                    Ok(ExprCategory::Other)
                }
                "string_concat" if !ctx.declared_external_names.contains(name) => {
                    if args.len() != 2 {
                        return Err(
                            call_arity_error(ctx, *span, "string_concat", 2, args.len()).into()
                        );
                    }
                    validate_expr(ctx, &args[0], bindings, in_actor)?;
                    validate_expr(ctx, &args[1], bindings, in_actor)?;
                    validate_call_arg_type(
                        ctx,
                        *span,
                        "string_concat",
                        &args[0],
                        bindings,
                        "string",
                        |type_| matches!(type_, RsigType::String),
                    )?;
                    validate_call_arg_type(
                        ctx,
                        *span,
                        "string_concat",
                        &args[1],
                        bindings,
                        "string",
                        |type_| matches!(type_, RsigType::String),
                    )?;
                    Ok(ExprCategory::Other)
                }
                _ if ctx.function_names.contains(name) || ctx.external_names.contains(name) => {
                    for arg in args {
                        validate_expr(ctx, arg, bindings, in_actor)?;
                    }
                    Ok(ExprCategory::Other)
                }
                _ if matches!(bindings.get(name), Some(BindingKind::Value(_))) => {
                    for arg in args {
                        validate_expr(ctx, arg, bindings, in_actor)?;
                    }
                    Ok(ExprCategory::Other)
                }
                _ => Err(to_source_diagnostic(
                    ctx.source_path,
                    ctx.source,
                    *span,
                    "unsupported function call",
                    format!("stage0 does not know `{name}`"),
                    Some("try: dbg(\"hello world\")"),
                )
                .into()),
            }
        }
        AstExpr::Apply { callee, args, .. } => {
            validate_expr(ctx, callee, bindings, in_actor)?;
            for arg in args {
                validate_expr(ctx, arg, bindings, in_actor)?;
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Lambda {
            params,
            param_types,
            body,
            ..
        } => {
            validate_scoped_block(ctx, body, bindings, params, param_types, in_actor)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::Spawn { body, span: _ } => {
            validate_actor_block(ctx, body, bindings)?;
            Ok(ExprCategory::Actor)
        }
        AstExpr::Receive { span, .. } if !in_actor => Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            *span,
            "receive outside spawn",
            "`receive` is only valid inside an actor body",
            Some("wrap the receive in `spawn { ... }`"),
        )
        .into()),
        AstExpr::Receive { body, .. } => {
            validate_expr(ctx, body, bindings, true)?;
            Ok(ExprCategory::Actor)
        }
        AstExpr::Eq { lhs, rhs, span } => {
            validate_expr(ctx, lhs, bindings, in_actor)?;
            validate_expr(ctx, rhs, bindings, in_actor)?;
            let lhs_type = simple_expr_type(ctx, lhs, bindings);
            let rhs_type = simple_expr_type(ctx, rhs, bindings);
            if let (Some(lhs_type), Some(rhs_type)) = (lhs_type, rhs_type)
                && !type_matches(&lhs_type, &rhs_type)
            {
                return Err(to_source_diagnostic(
                    ctx.source_path,
                    ctx.source,
                    *span,
                    "equality operands have different types",
                    format!(
                        "left side has type `{}`, but right side has type `{}`",
                        lhs_type.canonical(),
                        rhs_type.canonical()
                    ),
                    Some("compare values with the same type"),
                )
                .into());
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Lt { lhs, rhs, span } => {
            validate_expr(ctx, lhs, bindings, in_actor)?;
            validate_expr(ctx, rhs, bindings, in_actor)?;
            let lhs_type = simple_expr_type(ctx, lhs, bindings);
            let rhs_type = simple_expr_type(ctx, rhs, bindings);
            if let (Some(lhs_type), Some(rhs_type)) = (lhs_type, rhs_type) {
                let comparable = matches!(lhs_type, RsigType::I64 | RsigType::String)
                    && matches!(rhs_type, RsigType::I64 | RsigType::String)
                    && type_matches(&lhs_type, &rhs_type);
                if !comparable {
                    return Err(to_source_diagnostic(
                        ctx.source_path,
                        ctx.source,
                        *span,
                        "ordering operands are not comparable",
                        format!(
                            "left side has type `{}`, but right side has type `{}`",
                            lhs_type.canonical(),
                            rhs_type.canonical()
                        ),
                        Some("compare i64 values with i64 values or string values with string values"),
                    )
                    .into());
                }
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. }
        | AstExpr::And { lhs, rhs, .. }
        | AstExpr::Or { lhs, rhs, .. } => {
            validate_expr(ctx, lhs, bindings, in_actor)?;
            validate_expr(ctx, rhs, bindings, in_actor)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::Neg { expr, .. } | AstExpr::Not { expr, .. } => {
            validate_expr(ctx, expr, bindings, in_actor)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            validate_expr(ctx, condition, bindings, in_actor)?;
            validate_expr(ctx, then_branch, bindings, in_actor)?;
            validate_expr(ctx, else_branch, bindings, in_actor)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::Tuple { items, .. } | AstExpr::List { items, .. } => {
            for item in items {
                validate_expr(ctx, item, bindings, in_actor)?;
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Record { fields, .. } => {
            for (_, value) in fields {
                validate_expr(ctx, value, bindings, in_actor)?;
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Field { base, .. } => {
            validate_expr(ctx, base, bindings, in_actor)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::TupleIndex { base, index, span } => {
            validate_expr(ctx, base, bindings, in_actor)?;
            if let AstExpr::Tuple { items, .. } = base.as_ref()
                && *index >= items.len()
            {
                return Err(to_source_diagnostic(
                    ctx.source_path,
                    ctx.source,
                    *span,
                    "tuple projection is out of bounds",
                    format!(
                        "tuple has {} item(s), but projection requested index {index}",
                        items.len()
                    ),
                    Some("use a tuple projection index that exists"),
                )
                .into());
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Path { path, span } => {
            match path.segments.as_slice() {
                [name]
                    if bindings.contains_key(name)
                        || ctx.function_names.contains(name)
                        || ctx.external_names.contains(name) => {}
                [module, name] => {
                    let Some(rsig) = ctx.imports.get(module) else {
                        return Err(to_source_diagnostic(
                            ctx.source_path,
                            ctx.source,
                            *span,
                            "unknown imported module",
                            format!(
                                "`{module}` has not been brought into scope with `use {module}`"
                            ),
                            Some("add a top-level `use` declaration and pass --sig-dir"),
                        )
                        .into());
                    };
                    let Some(export) = rsig.find(name) else {
                        return Err(to_source_diagnostic(
                            ctx.source_path,
                            ctx.source,
                            *span,
                            "unknown imported value",
                            format!("module `{module}` does not export `{name}`"),
                            Some("check the producer .rsig"),
                        )
                        .into());
                    };
                    if export_has_unknown_abi(export) {
                        return Err(to_source_diagnostic(
                            ctx.source_path,
                            ctx.source,
                            *span,
                            "imported value has unknown ABI",
                            format!(
                                "module `{module}` exports `{name}`, but its .rsig type is not concrete enough to use as a value"
                            ),
                            Some("add enough type information for the exported function"),
                        )
                        .into());
                    }
                }
                [head, ..] => {
                    return Err(to_source_diagnostic(
                        ctx.source_path,
                        ctx.source,
                        *span,
                        "unknown value",
                        format!("`{head}` is not bound in this scope"),
                        Some("bind the value with `let` before using it"),
                    )
                    .into());
                }
                [] => {}
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Bool { .. }
        | AstExpr::Char { .. }
        | AstExpr::Unit { .. }
        | AstExpr::Float { .. }
        | AstExpr::Int { .. }
        | AstExpr::String { .. } => Ok(ExprCategory::Other),
    }
}

fn validate_call_arg_type(
    ctx: &ValidationContext<'_>,
    _function_span: TextSpan,
    function_name: &str,
    arg: &AstExpr,
    bindings: &HashMap<String, BindingKind>,
    expected: &str,
    accepts: impl Fn(&RsigType) -> bool,
) -> miette::Result<()> {
    let Some(actual) = simple_expr_type(ctx, arg, bindings) else {
        return Ok(());
    };
    if accepts(&actual) {
        return Ok(());
    }
    Err(to_source_diagnostic(
        ctx.source_path,
        ctx.source,
        expr_span(arg),
        format!("invalid {function_name} argument"),
        format!(
            "`{function_name}` expects `{expected}`, but this argument has type `{}`",
            actual.canonical()
        ),
        Some("pass a value with the expected type"),
    )
    .into())
}

fn validate_static_list_index(
    ctx: &ValidationContext<'_>,
    span: TextSpan,
    function_name: &str,
    list: &AstExpr,
    index: &AstExpr,
) -> miette::Result<()> {
    let Some(index_value) = static_i64_expr(index) else {
        return Ok(());
    };
    if index_value < 0 {
        return Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            expr_span(index),
            format!("{function_name} index is negative"),
            "`list_get` indexes start at 0",
            Some("use a non-negative index"),
        )
        .into());
    }
    if let AstExpr::List { items, .. } = list
        && index_value as usize >= items.len()
    {
        return Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            span,
            format!("{function_name} index out of bounds"),
            format!(
                "`list_get` index {index_value} is outside this list of length {}",
                items.len()
            ),
            Some("use an index that exists in the list"),
        )
        .into());
    }
    Ok(())
}

fn static_i64_expr(expr: &AstExpr) -> Option<i64> {
    match expr {
        AstExpr::Int { value, .. } => Some(*value),
        AstExpr::Neg { expr, .. } => static_i64_expr(expr).and_then(i64::checked_neg),
        _ => None,
    }
}

fn validate_actor_block(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    inherited_bindings: &HashMap<String, BindingKind>,
) -> miette::Result<()> {
    let mut bindings = inherited_bindings.clone();
    let mut has_receive = false;

    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                name_span: _,
                value,
                type_annotation,
                ..
            } => {
                if type_annotation.is_some() {
                    return Err(to_source_diagnostic(
                        ctx.source_path,
                        ctx.source,
                        expr_span(value),
                        "unsupported actor local annotation",
                        "stage0 actor locals do not support type annotations yet",
                        Some("omit the annotation"),
                    )
                    .into());
                }
                if let AstExpr::Spawn { body, .. } = value {
                    validate_actor_block(ctx, body, &bindings)?;
                    bindings.insert(name.clone(), BindingKind::Actor);
                } else {
                    validate_expr(ctx, value, &bindings, true)?;
                    let type_ = simple_expr_type(ctx, value, &bindings);
                    bindings.insert(name.clone(), BindingKind::Value(type_));
                }
            }
            AstStmt::Expr(expr) => {
                if let AstExpr::Receive { binder, body, .. } = expr {
                    has_receive = true;
                    let mut receive_bindings = bindings.clone();
                    receive_bindings.insert(binder.clone(), BindingKind::Message);
                    validate_expr(ctx, body, &receive_bindings, true)?;
                } else {
                    validate_expr(ctx, expr, &bindings, true)?;
                }
            }
        }
    }

    if let Some(tail) = &block.tail {
        if let AstExpr::Receive { binder, body, .. } = tail {
            has_receive = true;
            let mut receive_bindings = bindings.clone();
            receive_bindings.insert(binder.clone(), BindingKind::Message);
            validate_expr(ctx, body, &receive_bindings, true)?;
        } else {
            validate_expr(ctx, tail, &bindings, true)?;
        }
    }

    if !has_receive {
        return Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            block.span,
            "actor body never receives",
            "stage0 actors must contain at least one receive point",
            Some("try: spawn { receive { msg -> dbg(msg) } }"),
        )
        .into());
    }

    Ok(())
}

fn validate_actor_target(
    ctx: &ValidationContext<'_>,
    expr: &AstExpr,
    bindings: &HashMap<String, BindingKind>,
    span: TextSpan,
    operation: &str,
) -> miette::Result<()> {
    if let AstExpr::Path { path, .. } = expr
        && let [name] = path.segments.as_slice()
    {
        match bindings.get(name) {
            Some(BindingKind::Actor) | Some(BindingKind::Message) => return Ok(()),
            None => {
                return Err(to_source_diagnostic(
                    ctx.source_path,
                    ctx.source,
                    span,
                    format!("{operation} target is unknown"),
                    format!("`{name}` is not bound in this scope"),
                    Some("send to the result of `spawn { ... }`"),
                )
                .into());
            }
            Some(BindingKind::Value(_)) => {
                return Err(to_source_diagnostic(
                    ctx.source_path,
                    ctx.source,
                    span,
                    format!("{operation} target is not an actor"),
                    format!("`{name}` is a value, not an actor id"),
                    Some("send to the result of `spawn { ... }`"),
                )
                .into());
            }
        }
    }
    validate_expr(ctx, expr, bindings, false)?;
    Ok(())
}

fn validate_type_annotation(
    ctx: &ValidationContext<'_>,
    annotation: &AstTypeAnnotation,
    value: &AstExpr,
    _bindings: &HashMap<String, BindingKind>,
) -> miette::Result<()> {
    if annotation.text == "int" || annotation.text == "float" {
        let error = parse_primitive_type(&annotation.text).unwrap_err();
        return Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            annotation.span,
            "unsupported type annotation",
            error.message,
            error.help,
        )
        .into());
    }

    if parse_primitive_type(&annotation.text).is_err()
        && !annotation.text.starts_with('(')
        && !annotation.text.ends_with(" list")
    {
        return Ok(());
    }

    let value_type = resolve_const_value(value, &HashMap::new(), &ctx.functions)
        .map(|value| const_value_type(&value));
    if let Some(value_type) = value_type {
        let expected = parse_type(&annotation.text);
        if type_matches(&expected, &value_type) {
            return Ok(());
        }
        return Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            annotation.span,
            "type annotation does not match value",
            format!(
                "expected `{}`, but this binding has type `{}`",
                expected.canonical(),
                value_type.canonical()
            ),
            Some("change the annotation or the value"),
        )
        .into());
    }

    Ok(())
}

fn validate_function_annotations(
    ctx: &ValidationContext<'_>,
    function: &crate::ast::AstFnDecl,
) -> miette::Result<()> {
    let mut bindings = HashMap::new();
    for (index, annotation) in function.param_types.iter().enumerate() {
        let Some(annotation) = annotation else {
            continue;
        };
        validate_function_abi_annotation(ctx, annotation)?;
        if let Some(param) = function.params.get(index) {
            bindings.insert(param.clone(), parse_type(&annotation.text));
        }
    }

    let Some(return_annotation) = &function.return_type else {
        return Ok(());
    };
    validate_function_abi_annotation(ctx, return_annotation)?;

    let expected = parse_type(&return_annotation.text);
    let actual = infer_annotation_block_type(ctx, &function.body, &mut bindings);
    if let Some(actual) = actual
        && !type_matches(&expected, &actual)
    {
        return Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            return_annotation.span,
            "function return type does not match body",
            format!(
                "expected `{}`, but this function body has type `{}`",
                expected.canonical(),
                actual.canonical()
            ),
            Some("change the return annotation or the function body"),
        )
        .into());
    }

    Ok(())
}

fn validate_function_abi_annotation(
    ctx: &ValidationContext<'_>,
    annotation: &AstTypeAnnotation,
) -> miette::Result<()> {
    if annotation.text == "int" || annotation.text == "float" {
        let error = parse_primitive_type(&annotation.text).unwrap_err();
        return Err(to_source_diagnostic(
            ctx.source_path,
            ctx.source,
            annotation.span,
            "unsupported function ABI annotation",
            error.message,
            error.help,
        )
        .into());
    }

    let type_ = parse_type(&annotation.text);
    if matches!(
        type_,
        RsigType::I64
            | RsigType::Bool
            | RsigType::Unit
            | RsigType::ActorId(_)
            | RsigType::String
            | RsigType::Tuple(_)
            | RsigType::List(_)
            | RsigType::Record(_)
            | RsigType::Arrow { .. }
    ) {
        return Ok(());
    }

    Err(to_source_diagnostic(
        ctx.source_path,
        ctx.source,
        annotation.span,
        "unsupported function ABI annotation",
        format!(
            "`{}` is not supported for native Riot function parameters or returns yet",
            annotation.text
        ),
        Some("use a scalar type or a boxed value type such as `string`, a tuple, a list, or a record"),
    )
    .into())
}

fn infer_annotation_block_type(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    bindings: &mut HashMap<String, RsigType>,
) -> Option<RsigType> {
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                let type_ = type_annotation
                    .as_ref()
                    .map(|annotation| parse_type(&annotation.text))
                    .or_else(|| infer_annotation_expr_type(ctx, value, bindings))
                    .unwrap_or(RsigType::Unknown);
                bindings.insert(name.clone(), type_);
            }
            AstStmt::Expr(expr) => {
                infer_annotation_expr_type(ctx, expr, bindings);
            }
        }
    }
    if let Some(tail) = &block.tail {
        infer_annotation_expr_type(ctx, tail, bindings)
    } else {
        Some(RsigType::Unit)
    }
}

fn infer_annotation_expr_type(
    ctx: &ValidationContext<'_>,
    expr: &AstExpr,
    bindings: &HashMap<String, RsigType>,
) -> Option<RsigType> {
    match expr {
        AstExpr::Add { .. }
        | AstExpr::Sub { .. }
        | AstExpr::Mul { .. }
        | AstExpr::Div { .. }
        | AstExpr::Mod { .. }
        | AstExpr::Neg { .. }
        | AstExpr::Int { .. } => Some(RsigType::I64),
        AstExpr::Eq { .. }
        | AstExpr::Lt { .. }
        | AstExpr::And { .. }
        | AstExpr::Or { .. }
        | AstExpr::Not { .. }
        | AstExpr::Bool { .. } => Some(RsigType::Bool),
        AstExpr::If {
            then_branch,
            else_branch,
            ..
        } => Some(merge_annotation_types(
            infer_annotation_expr_type(ctx, then_branch, bindings).unwrap_or(RsigType::Unknown),
            infer_annotation_expr_type(ctx, else_branch, bindings).unwrap_or(RsigType::Unknown),
        )),
        AstExpr::Call { callee, args, .. } => match callee.segments.as_slice() {
            [name] if name == "dbg" || name == "println" => Some(RsigType::Unit),
            [name] if name == "send" || name == "monitor" || name == "link" => Some(RsigType::Unit),
            [name] if name == "list_len" || name == "string_len" => Some(RsigType::I64),
            [name] if name == "string_concat" => Some(RsigType::String),
            [name] if name == "list_get" => args.first().and_then(|list| {
                match infer_annotation_expr_type(ctx, list, bindings) {
                    Some(RsigType::List(item)) => Some(*item),
                    _ => Some(RsigType::Unknown),
                }
            }),
            [name] => ctx.function_results.get(name).cloned(),
            [module, name] => ctx
                .imports
                .get(module)
                .and_then(|rsig| rsig.find(name))
                .map(|export| match export {
                    RsigExport::Function(function) => function.result.clone(),
                    RsigExport::External(external) => external.result.clone(),
                }),
            _ => None,
        },
        AstExpr::Apply { .. } => Some(RsigType::Unknown),
        AstExpr::Unit { .. } => Some(RsigType::Unit),
        AstExpr::Tuple { items, .. } => Some(RsigType::Tuple(
            items
                .iter()
                .map(|item| {
                    infer_annotation_expr_type(ctx, item, bindings).unwrap_or(RsigType::Unknown)
                })
                .collect(),
        )),
        AstExpr::List { items, .. } => Some(RsigType::List(Box::new(
            items
                .first()
                .and_then(|item| infer_annotation_expr_type(ctx, item, bindings))
                .unwrap_or(RsigType::Unknown),
        ))),
        AstExpr::Record { path, .. } => Some(RsigType::Record(path.segments.join("."))),
        AstExpr::Field { base, field, .. } => {
            infer_annotation_field_type(ctx, base, field, bindings)
        }
        AstExpr::TupleIndex { base, index, .. } => {
            infer_annotation_tuple_index_type(ctx, base, *index, bindings)
        }
        AstExpr::Char { .. } => Some(RsigType::Char),
        AstExpr::Float { .. } => Some(RsigType::F64),
        AstExpr::Path { path, .. } => path
            .segments
            .first()
            .and_then(|name| bindings.get(name))
            .cloned(),
        AstExpr::Lambda { .. } => Some(RsigType::Unknown),
        AstExpr::String { .. } => Some(RsigType::String),
        AstExpr::Spawn { .. } => Some(RsigType::ActorId(Box::new(RsigType::Unknown))),
        AstExpr::Receive { .. } => Some(RsigType::Unit),
    }
}

fn infer_annotation_field_type(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    field: &str,
    bindings: &HashMap<String, RsigType>,
) -> Option<RsigType> {
    if let AstExpr::Record { fields, .. } = base {
        return fields.iter().find_map(|(name, value)| {
            (name == field).then(|| {
                infer_annotation_expr_type(ctx, value, bindings).unwrap_or(RsigType::Unknown)
            })
        });
    }
    None
}

fn infer_annotation_tuple_index_type(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    index: usize,
    bindings: &HashMap<String, RsigType>,
) -> Option<RsigType> {
    if let AstExpr::Tuple { items, .. } = base {
        return items
            .get(index)
            .and_then(|item| infer_annotation_expr_type(ctx, item, bindings));
    }
    None
}

fn merge_annotation_types(lhs: RsigType, rhs: RsigType) -> RsigType {
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

fn const_value_type(value: &ConstValue) -> RsigType {
    match value {
        ConstValue::Bool(_) => RsigType::Bool,
        ConstValue::Char(_) => RsigType::Char,
        ConstValue::Float(_) => RsigType::F64,
        ConstValue::Int(_) => RsigType::I64,
        ConstValue::String(_) => RsigType::String,
        ConstValue::Unit => RsigType::Unit,
        ConstValue::Tuple(items) => RsigType::Tuple(items.iter().map(const_value_type).collect()),
        ConstValue::List(items) => RsigType::List(Box::new(
            items
                .first()
                .map(const_value_type)
                .unwrap_or(RsigType::Unknown),
        )),
        ConstValue::Record { path, fields: _ } => RsigType::Record(path.clone()),
    }
}

fn type_matches(expected: &RsigType, actual: &RsigType) -> bool {
    expected == actual
        || matches!(expected, RsigType::Unknown)
        || matches!(actual, RsigType::Unknown)
}

fn export_has_unknown_abi(export: &RsigExport) -> bool {
    match export {
        RsigExport::Function(function) => {
            rsig_type_has_unknown_abi(&function.result)
                || function.params.iter().any(rsig_type_has_unknown_abi)
        }
        RsigExport::External(external) => {
            rsig_type_has_unknown_abi(&external.result)
                || external.params.iter().any(rsig_type_has_unknown_abi)
        }
    }
}

fn rsig_type_has_unknown_abi(type_: &RsigType) -> bool {
    match type_ {
        RsigType::Unknown | RsigType::Var(_) => true,
        RsigType::ActorId(message) | RsigType::List(message) => rsig_type_has_unknown_abi(message),
        RsigType::Arrow { parameter, result } => {
            rsig_type_has_unknown_abi(parameter) || rsig_type_has_unknown_abi(result)
        }
        RsigType::Tuple(items) => items.iter().any(rsig_type_has_unknown_abi),
        RsigType::Bool
        | RsigType::Char
        | RsigType::F64
        | RsigType::I64
        | RsigType::String
        | RsigType::Unit
        | RsigType::Record(_) => false,
    }
}

fn simple_expr_type(
    ctx: &ValidationContext<'_>,
    expr: &AstExpr,
    bindings: &HashMap<String, BindingKind>,
) -> Option<RsigType> {
    match expr {
        AstExpr::Add { .. }
        | AstExpr::Sub { .. }
        | AstExpr::Mul { .. }
        | AstExpr::Div { .. }
        | AstExpr::Mod { .. }
        | AstExpr::Neg { .. }
        | AstExpr::Int { .. } => Some(RsigType::I64),
        AstExpr::Eq { .. }
        | AstExpr::Lt { .. }
        | AstExpr::And { .. }
        | AstExpr::Or { .. }
        | AstExpr::Not { .. }
        | AstExpr::Bool { .. } => Some(RsigType::Bool),
        AstExpr::Char { .. } => Some(RsigType::Char),
        AstExpr::Float { .. } => Some(RsigType::F64),
        AstExpr::String { .. } => Some(RsigType::String),
        AstExpr::Unit { .. } => Some(RsigType::Unit),
        AstExpr::Tuple { items, .. } => Some(RsigType::Tuple(
            items
                .iter()
                .map(|item| simple_expr_type(ctx, item, bindings).unwrap_or(RsigType::Unknown))
                .collect(),
        )),
        AstExpr::List { items, .. } => Some(RsigType::List(Box::new(
            items
                .first()
                .and_then(|item| simple_expr_type(ctx, item, bindings))
                .unwrap_or(RsigType::Unknown),
        ))),
        AstExpr::Path { path, .. } if path.segments.len() == 1 => {
            match bindings.get(&path.segments[0]) {
                Some(BindingKind::Actor) => Some(RsigType::ActorId(Box::new(RsigType::Unknown))),
                Some(BindingKind::Value(type_)) => type_.clone(),
                Some(BindingKind::Message) => None,
                None => None,
            }
        }
        AstExpr::Call { callee, args, .. } => match callee.segments.as_slice() {
            [name] if name == "dbg" || name == "println" => Some(RsigType::Unit),
            [name] if name == "send" || name == "monitor" || name == "link" => Some(RsigType::Unit),
            [name] if name == "list_len" || name == "string_len" => Some(RsigType::I64),
            [name] if name == "string_concat" => Some(RsigType::String),
            [name] if name == "list_get" => {
                args.first()
                    .and_then(|list| match simple_expr_type(ctx, list, bindings) {
                        Some(RsigType::List(item)) => Some(*item),
                        _ => Some(RsigType::Unknown),
                    })
            }
            [module, name] => ctx
                .imports
                .get(module)
                .and_then(|rsig| rsig.find(name))
                .map(|export| match export {
                    RsigExport::Function(function) => function.result.clone(),
                    RsigExport::External(external) => external.result.clone(),
                }),
            _ => None,
        },
        AstExpr::Apply { .. } => Some(RsigType::Unknown),
        AstExpr::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_type = simple_expr_type(ctx, then_branch, bindings)?;
            let else_type = simple_expr_type(ctx, else_branch, bindings)?;
            type_matches(&then_type, &else_type).then_some(then_type)
        }
        AstExpr::Record { path, .. } => Some(RsigType::Record(path.segments.join("."))),
        AstExpr::Field { base, field, .. } => simple_field_type(ctx, base, field, bindings),
        AstExpr::TupleIndex { base, index, .. } => {
            simple_tuple_index_type(ctx, base, *index, bindings)
        }
        AstExpr::Lambda { .. } => Some(RsigType::Unknown),
        AstExpr::Spawn { .. } => Some(RsigType::ActorId(Box::new(RsigType::Unknown))),
        AstExpr::Receive { .. } | AstExpr::Path { .. } => None,
    }
}

fn simple_field_type(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    field: &str,
    bindings: &HashMap<String, BindingKind>,
) -> Option<RsigType> {
    if let AstExpr::Record { fields, .. } = base {
        return fields.iter().find_map(|(name, value)| {
            (name == field)
                .then(|| simple_expr_type(ctx, value, bindings).unwrap_or(RsigType::Unknown))
        });
    }
    None
}

fn simple_tuple_index_type(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    index: usize,
    bindings: &HashMap<String, BindingKind>,
) -> Option<RsigType> {
    if let AstExpr::Tuple { items, .. } = base {
        return items
            .get(index)
            .and_then(|item| simple_expr_type(ctx, item, bindings));
    }
    None
}

fn call_arity_error(
    ctx: &ValidationContext<'_>,
    span: TextSpan,
    name: &str,
    expected: usize,
    actual: usize,
) -> miette::Report {
    to_source_diagnostic(
        ctx.source_path,
        ctx.source,
        span,
        format!("{name} expects {expected} argument(s)"),
        format!("found {actual} argument(s)"),
        Some("check the call arity"),
    )
    .into()
}
