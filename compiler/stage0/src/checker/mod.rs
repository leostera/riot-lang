use std::collections::HashMap;

use camino::Utf8Path;

mod const_eval;
mod span;
mod types;

use const_eval::{ConstFunction, ConstValue, resolve_const_value};
use span::expr_span;
use types::parse_primitive_type;

use crate::ast::{AstBlock, AstExpr, AstProgram, AstStmt, TextSpan};
use crate::diagnostic::to_source_diagnostic;
use crate::ir::{TypedExpr, TypedFunction, TypedProgram};

pub(crate) fn typecheck(
    source_path: &Utf8Path,
    source: &str,
    program: AstProgram,
) -> miette::Result<TypedProgram> {
    let Some(decl) = program.decls.iter().find(|decl| decl.name == "main") else {
        let span = program
            .decls
            .first()
            .map(|decl| decl.span)
            .unwrap_or_else(|| TextSpan::new(0, source.len().min(1)));
        return Err(to_source_diagnostic(
            source_path,
            source,
            span,
            "missing main function",
            "stage0 requires one entrypoint named main",
            Some("add: fn main() { dbg(\"hello world\") }"),
        )
        .into());
    };

    if decl.params.len() != 0 {
        return Err(to_source_diagnostic(
            source_path,
            source,
            decl.name_span,
            "main function cannot have parameters",
            "stage0 requires main to have no parameters",
            Some("try: fn main() { dbg(\"hello world\") }"),
        )
        .into());
    }

    let duplicate_main = program.decls.iter().filter(|decl| decl.name == "main").nth(1);
    if let Some(duplicate_main) = duplicate_main {
        return Err(to_source_diagnostic(
            source_path,
            source,
            duplicate_main.span,
            "duplicate main function",
            "stage0 requires a single main function per file",
            Some("keep only one `fn main() { ... }`"),
        )
        .into());
    }

    if decl.name != "main" {
        return Err(to_source_diagnostic(
            source_path,
            source,
            decl.name_span,
            "missing main function",
            "this function must be named main",
            Some("stage0 requires one entrypoint: fn main() { dbg(\"hello world\") }"),
        )
        .into());
    }

    let functions = const_functions(&program);
    let body = resolve_main_output(source_path, source, &decl.body, &functions)?;

    Ok(TypedProgram {
        main: TypedFunction { body },
    })
}

fn const_functions(program: &AstProgram) -> HashMap<String, ConstFunction> {
    program
        .decls
        .iter()
        .filter(|decl| decl.name != "main")
        .map(|decl| {
            (
                decl.name.clone(),
                ConstFunction {
                    params: decl.params.clone(),
                    body: decl.body.clone(),
                },
            )
        })
        .collect()
}

fn resolve_main_output(
    source_path: &Utf8Path,
    source: &str,
    block: &AstBlock,
    functions: &HashMap<String, ConstFunction>,
) -> miette::Result<TypedExpr> {
    let mut bindings = HashMap::<String, ConstValue>::new();
    let mut final_expr = None;

    for (index, stmt) in block.statements.iter().enumerate() {
        match stmt {
            AstStmt::Let {
                name,
                name_span,
                type_annotation,
                value,
                span,
            } => {
                if bindings.contains_key(name) {
                    return Err(to_source_diagnostic(
                        source_path,
                        source,
                        *name_span,
                        "duplicate local binding",
                        format!("`{name}` is already bound in this block"),
                        Some("use a unique local name"),
                    )
                    .into());
                }

                let value = resolve_const_value(value, &bindings, functions).ok_or_else(|| {
                    to_source_diagnostic(
                        source_path,
                        source,
                        *span,
                        "unsupported local binding",
                        "stage0 currently supports literal local bindings",
                        Some("try: let name = \"Alice\";"),
                    )
                })?;
                if let Some(type_annotation) = type_annotation {
                    validate_type_annotation(source_path, source, type_annotation, &value)?;
                }
                bindings.insert(name.clone(), value);
            }
            AstStmt::Expr(expr) => {
                if block.tail.is_some() || index + 1 != block.statements.len() {
                    return Err(to_source_diagnostic(
                        source_path,
                        source,
                        expr_span(expr),
                        "unsupported main body",
                        "stage0 currently allows local lets followed by one dbg expression",
                        Some("try: fn main() { let name = \"Alice\"; dbg(name) }"),
                    )
                    .into());
                }

                final_expr = Some(expr);
            }
        }
    }

    if let Some(tail) = &block.tail {
        final_expr = Some(tail);
    }

    let Some(expr) = final_expr else {
        return Err(to_source_diagnostic(
            source_path,
            source,
            block.span,
            "unsupported main body",
            "stage0 currently expects main to call dbg with one constant value",
            Some("try: fn main() { dbg(\"hello world\") }"),
        )
        .into());
    };

    output_expr(expr, &bindings, functions).ok_or_else(|| {
        to_source_diagnostic(
            source_path,
            source,
            expr_span(expr),
            "unsupported main body",
            "stage0 currently expects main to call dbg with one constant value",
            Some("try: fn main() { let name = \"Alice\"; dbg(name) }"),
        )
        .into()
    })
}

fn validate_type_annotation(
    source_path: &Utf8Path,
    source: &str,
    annotation: &crate::ast::AstTypeAnnotation,
    value: &ConstValue,
) -> miette::Result<()> {
    let primitive = parse_primitive_type(&annotation.text).map_err(|error| {
        to_source_diagnostic(
            source_path,
            source,
            annotation.span,
            "unsupported type annotation",
            error.message,
            error.help,
        )
    })?;

    if value.matches_primitive(primitive) {
        return Ok(());
    }

    Err(to_source_diagnostic(
        source_path,
        source,
        annotation.span,
        "type annotation does not match value",
        format!(
            "expected `{}`, but this binding has type `{}`",
            primitive.name(),
            value.type_name()
        ),
        Some("change the annotation or the value"),
    )
    .into())
}

fn output_expr(
    expr: &AstExpr,
    bindings: &HashMap<String, ConstValue>,
    functions: &HashMap<String, ConstFunction>,
) -> Option<TypedExpr> {
    let AstExpr::Call {
        callee,
        args,
        span: _,
    } = expr
    else {
        return None;
    };
    let callee = match callee.segments.as_slice() {
        [segment] => segment.as_str(),
        _ => return None,
    };

    let value = match args.as_slice() {
        [arg] => resolve_const_value(arg, bindings, functions)?,
        _ => return None,
    };

    match callee {
        "dbg" => Some(TypedExpr::Dbg {
            message: value.to_print_string(),
        }),
        "println" => {
            let ConstValue::String(message) = value else {
                return None;
            };
            Some(TypedExpr::Println { message })
        }
        _ => None,
    }
}
