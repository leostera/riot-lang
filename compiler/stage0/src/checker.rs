use std::collections::HashMap;

use camino::Utf8Path;
use chumsky::span::{SimpleSpan, Span};

use crate::ast::{AstBlock, AstExpr, AstProgram, AstStmt, TextSpan};
use crate::diagnostic::to_source_diagnostic;
use crate::ir::{TypedExpr, TypedFunction, TypedProgram};

pub(crate) fn typecheck(
    source_path: &Utf8Path,
    source: &str,
    program: AstProgram,
) -> miette::Result<TypedProgram> {
    if program.decls.len() != 1 {
        let span = program
            .decls
            .get(1)
            .map(|decl| decl.span)
            .or_else(|| program.decls.first().map(|decl| decl.span))
            .unwrap_or_else(|| SimpleSpan::new((), 0..source.len().min(1)));

        return Err(to_source_diagnostic(
            source_path,
            source,
            span,
            "expected exactly one top-level function",
            "stage0 requires a single main function per file",
            Some("keep only: fn main() { println(\"hello world\") }"),
        )
        .into());
    }

    let decl = &program.decls[0];
    if decl.name != "main" {
        return Err(to_source_diagnostic(
            source_path,
            source,
            decl.name_span,
            "missing main function",
            "this function must be named main",
            Some("stage0 requires one entrypoint: fn main() { println(\"hello world\") }"),
        )
        .into());
    }

    let message = resolve_main_message(source_path, source, &decl.body)?;

    Ok(TypedProgram {
        main: TypedFunction {
            body: TypedExpr::Println { message },
        },
    })
}

fn resolve_main_message(
    source_path: &Utf8Path,
    source: &str,
    block: &AstBlock,
) -> miette::Result<String> {
    let mut bindings = HashMap::<String, ConstValue>::new();
    let mut final_expr = None;

    for (index, stmt) in block.statements.iter().enumerate() {
        match stmt {
            AstStmt::Let {
                name,
                name_span,
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

                let value = resolve_const_value(value, &bindings).ok_or_else(|| {
                    to_source_diagnostic(
                        source_path,
                        source,
                        *span,
                        "unsupported local binding",
                        "stage0 currently supports literal local bindings",
                        Some("try: let name = \"Alice\";"),
                    )
                })?;
                bindings.insert(name.clone(), value);
            }
            AstStmt::Expr(expr) => {
                if block.tail.is_some() || index + 1 != block.statements.len() {
                    return Err(to_source_diagnostic(
                        source_path,
                        source,
                        expr_span(expr),
                        "unsupported main body",
                        "stage0 currently allows local lets followed by one println expression",
                        Some("try: fn main() { let name = \"Alice\"; println(name) }"),
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
            "stage0 currently expects main to call println with one string value",
            Some("try: fn main() { println(\"hello world\") }"),
        )
        .into());
    };

    println_message(expr, &bindings).ok_or_else(|| {
        to_source_diagnostic(
            source_path,
            source,
            expr_span(expr),
            "unsupported main body",
            "stage0 currently expects main to call println with one string value",
            Some("try: fn main() { let name = \"Alice\"; println(name) }"),
        )
        .into()
    })
}

fn println_message(expr: &AstExpr, bindings: &HashMap<String, ConstValue>) -> Option<String> {
    let AstExpr::Call { callee, args } = expr else {
        return None;
    };
    if callee.segments.as_slice() != ["println"] {
        return None;
    }

    match args.as_slice() {
        [arg] => resolve_const_value(arg, bindings).map(|value| value.to_print_string()),
        _ => None,
    }
}

#[derive(Debug, Clone)]
enum ConstValue {
    Bool(bool),
    Char(char),
    Int(i64),
    String(String),
}

impl ConstValue {
    fn to_print_string(&self) -> String {
        match self {
            ConstValue::Bool(true) => "true".to_owned(),
            ConstValue::Bool(false) => "false".to_owned(),
            ConstValue::Char(value) => value.to_string(),
            ConstValue::Int(value) => value.to_string(),
            ConstValue::String(value) => value.clone(),
        }
    }
}

fn resolve_const_value(
    expr: &AstExpr,
    bindings: &HashMap<String, ConstValue>,
) -> Option<ConstValue> {
    match expr {
        AstExpr::Add {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs + rhs)),
            _ => None,
        },
        AstExpr::Sub {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs - rhs)),
            _ => None,
        },
        AstExpr::Mul {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs * rhs)),
            _ => None,
        },
        AstExpr::Div {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Int(_), ConstValue::Int(0)) => None,
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs / rhs)),
            _ => None,
        },
        AstExpr::Mod {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Int(_), ConstValue::Int(0)) => None,
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Int(lhs % rhs)),
            _ => None,
        },
        AstExpr::Neg { expr, span: _ } => match resolve_const_value(expr, bindings)? {
            ConstValue::Int(value) => Some(ConstValue::Int(-value)),
            _ => None,
        },
        AstExpr::Eq {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(ConstValue::Bool(lhs == rhs)),
            (ConstValue::Char(lhs), ConstValue::Char(rhs)) => Some(ConstValue::Bool(lhs == rhs)),
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Bool(lhs == rhs)),
            (ConstValue::String(lhs), ConstValue::String(rhs)) => Some(ConstValue::Bool(lhs == rhs)),
            _ => None,
        },
        AstExpr::Lt {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Int(lhs), ConstValue::Int(rhs)) => Some(ConstValue::Bool(lhs < rhs)),
            _ => None,
        },
        AstExpr::And {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(ConstValue::Bool(lhs && rhs)),
            _ => None,
        },
        AstExpr::Or {
            lhs,
            rhs,
            span: _,
        } => match (resolve_const_value(lhs, bindings)?, resolve_const_value(rhs, bindings)?) {
            (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(ConstValue::Bool(lhs || rhs)),
            _ => None,
        },
        AstExpr::Not { expr, span: _ } => match resolve_const_value(expr, bindings)? {
            ConstValue::Bool(value) => Some(ConstValue::Bool(!value)),
            _ => None,
        },
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            span: _,
        } => match resolve_const_value(condition, bindings)? {
            ConstValue::Bool(true) => resolve_const_value(then_branch, bindings),
            ConstValue::Bool(false) => resolve_const_value(else_branch, bindings),
            _ => None,
        },
        AstExpr::Bool { value, span: _ } => Some(ConstValue::Bool(*value)),
        AstExpr::Char { value, span: _ } => Some(ConstValue::Char(*value)),
        AstExpr::Int { value, span: _ } => Some(ConstValue::Int(*value)),
        AstExpr::String { value, span: _ } => Some(ConstValue::String(value.clone())),
        AstExpr::Path { path, span: _ } if path.segments.len() == 1 => {
            bindings.get(&path.segments[0]).cloned()
        }
        AstExpr::Path { .. } | AstExpr::Call { .. } => None,
    }
}

fn expr_span(expr: &AstExpr) -> TextSpan {
    match expr {
        AstExpr::Add {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Sub {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Mul {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Div {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Mod {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Neg { expr: _, span }
        | AstExpr::Eq {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Lt {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::And {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Or {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Not { expr: _, span }
        | AstExpr::If {
            condition: _,
            then_branch: _,
            else_branch: _,
            span,
        } => *span,
        AstExpr::Call { callee: _, args } => args
            .first()
            .map(expr_span)
            .unwrap_or_else(|| SimpleSpan::new((), 0..0)),
        AstExpr::Bool { value: _, span }
        | AstExpr::Char { value: _, span }
        | AstExpr::Int { value: _, span }
        | AstExpr::Path { path: _, span }
        | AstExpr::String { value: _, span } => *span,
    }
}
