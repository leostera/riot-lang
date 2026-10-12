//! C source emission for the TinyML Stage 0 bootstrap bridge.
//!
//! This deliberately starts with a small, typed `main : unit -> i32` subset.
//! It emits C only when every expression is represented exactly; the owned
//! Riotc backend remains a separate pipeline and never falls back to this code.

use std::{collections::HashMap, fmt::Write as _};

use thiserror::Error;

use crate::{
    checker::{
        builtin,
        tir::{BindingId, EntityId, Type, ValueEntity},
    },
    lambda::ir::{self, ExprKind, Primitive},
    parser::{ast::Literal, lexer::Span},
};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum EmitError {
    #[error("C Stage 0 backend does not yet support {construct} at bytes {start}..{end}")]
    Unsupported {
        construct: &'static str,
        start: usize,
        end: usize,
    },
}

/// Emit one closed `main : unit -> i32` module as a C translation unit.
///
/// This first slice is useful for the Stage 0 status-42 smoke test. More source
/// forms will be added by extending this typed lowering, not by falling back to
/// JavaScript or guessing a C representation.
pub fn emit_main_i32(module: &ir::Module) -> Result<String, EmitError> {
    let [main] = module.values.as_slice() else {
        return Err(unsupported_module(
            "a single top-level `main` function",
            0,
            0,
        ));
    };
    if main.name.as_name() != Some("main") {
        return Err(unsupported_module(
            "a top-level `main` function",
            main.span.start,
            main.span.end,
        ));
    }
    let Type::Arrow { parameter, result } = &main.ty else {
        return Err(unsupported_module(
            "`main : unit -> i32`",
            main.span.start,
            main.span.end,
        ));
    };
    if !builtin::is_unqualified_type(parameter.as_ref(), "unit")
        || !builtin::is_unqualified_type(result.as_ref(), "i32")
    {
        return Err(unsupported_module(
            "`main : unit -> i32`",
            main.span.start,
            main.span.end,
        ));
    }
    let ExprKind::Function { params, body } = &main.body.kind else {
        return Err(unsupported_module(
            "a unit-argument `main` function",
            main.body.span.start,
            main.body.span.end,
        ));
    };
    let [unit_parameter] = params.as_slice() else {
        return Err(unsupported_module(
            "one unit parameter to `main`",
            main.body.span.start,
            main.body.span.end,
        ));
    };
    if !builtin::is_unqualified_type(&unit_parameter.ty, "unit") {
        return Err(unsupported_module(
            "a unit parameter to `main`",
            unit_parameter.span.start,
            unit_parameter.span.end,
        ));
    }
    if !builtin::is_unqualified_type(&body.ty, "i32") {
        return Err(unsupported_module(
            "an i32 result from `main`",
            body.span.start,
            body.span.end,
        ));
    }

    let mut body_emitter = I32BodyEmitter::default();
    let body = body_emitter.emit_expression(body)?;
    let mut output = String::new();
    output.push_str(C_I32_RUNTIME);
    writeln!(output, "\nstatic int32_t tinyml_entry(void) {{")
        .expect("writing C into a String cannot fail");
    for statement in body_emitter.statements {
        writeln!(output, "    {statement}").expect("writing C into a String cannot fail");
    }
    writeln!(output, "    return {body};\n}}").expect("writing C into a String cannot fail");
    output.push_str(
        "\nint main(int argc, char **argv) {\n    (void)argc;\n    (void)argv;\n    return (int)tinyml_entry();\n}\n",
    );
    Ok(output)
}

#[derive(Default)]
struct I32BodyEmitter {
    locals: HashMap<BindingId, String>,
    statements: Vec<String>,
    next_local: usize,
}

impl I32BodyEmitter {
    fn emit_expression(&mut self, expression: &ir::Expr) -> Result<String, EmitError> {
        if !builtin::is_unqualified_type(&expression.ty, "i32") {
            return Err(unsupported_expr(
                "a non-i32 expression in `main`",
                expression.span,
            ));
        }

        match &expression.kind {
            ExprKind::Constant(ir::Constant::Literal(Literal::Int(value))) => {
                let Some(decimal) = integer_literal_as_decimal(value.as_str()) else {
                    return Err(unsupported_expr(
                        "an invalid checked i32 literal",
                        expression.span,
                    ));
                };
                Ok(format!("tinyml_i32_literal(\"{decimal}\")"))
            }
            ExprKind::Var {
                entity: EntityId::Value(ValueEntity { binding }),
                ..
            } => self
                .locals
                .get(binding)
                .cloned()
                .ok_or_else(|| unsupported_expr("a non-local value reference", expression.span)),
            ExprKind::Prim { op, args } => {
                let [left, right] = args.as_slice() else {
                    return Err(unsupported_expr(
                        "a non-binary integer operation",
                        expression.span,
                    ));
                };
                let operation = match op {
                    Primitive::Add { operand } if builtin::is_unqualified_type(operand, "i32") => {
                        "add"
                    }
                    Primitive::Sub { operand } if builtin::is_unqualified_type(operand, "i32") => {
                        "sub"
                    }
                    Primitive::Mul { operand } if builtin::is_unqualified_type(operand, "i32") => {
                        "mul"
                    }
                    Primitive::Div { operand } if builtin::is_unqualified_type(operand, "i32") => {
                        "div"
                    }
                    Primitive::Rem { operand } if builtin::is_unqualified_type(operand, "i32") => {
                        "rem"
                    }
                    _ => {
                        return Err(unsupported_expr(
                            "this primitive operation",
                            expression.span,
                        ));
                    }
                };
                let left = self.emit_expression(left)?;
                let right = self.emit_expression(right)?;
                Ok(format!("tinyml_i32_{operation}({left}, {right})"))
            }
            ExprKind::Block(expressions) => self.emit_block(expressions, expression),
            _ => Err(unsupported_expr(
                "this expression in `main`",
                expression.span,
            )),
        }
    }

    fn emit_block(
        &mut self,
        expressions: &[ir::Expr],
        block: &ir::Expr,
    ) -> Result<String, EmitError> {
        let Some((last, prefix)) = expressions.split_last() else {
            return Err(unsupported_expr("an empty block", block.span));
        };
        let previous_locals = self.locals.clone();

        for expression in prefix {
            match &expression.kind {
                ExprKind::Let { bind, body, .. } => {
                    let value = self.emit_expression(body)?;
                    match &bind.kind {
                        ir::PatternKind::Var { binding, .. }
                            if builtin::is_unqualified_type(&bind.ty, "i32") =>
                        {
                            let local = format!("tinyml_local_{}", self.next_local);
                            self.next_local += 1;
                            self.statements.push(format!("int32_t {local} = {value};"));
                            self.locals.insert(binding.clone(), local);
                        }
                        ir::PatternKind::Wildcard => {}
                        _ => {
                            return Err(unsupported_expr("this local binding pattern", bind.span));
                        }
                    }
                }
                _ if builtin::is_unqualified_type(&expression.ty, "unit") => {}
                _ => {
                    return Err(unsupported_expr(
                        "a discarded non-unit block expression",
                        expression.span,
                    ));
                }
            }
        }

        let result = self.emit_expression(last);
        self.locals = previous_locals;
        result
    }
}

fn integer_literal_as_decimal(value: &str) -> Option<String> {
    let (negative, magnitude) = value
        .strip_prefix('-')
        .map_or((false, value), |magnitude| (true, magnitude));
    let (radix, digits) = if let Some(digits) = magnitude.strip_prefix("0x") {
        (16, digits)
    } else if let Some(digits) = magnitude.strip_prefix("0o") {
        (8, digits)
    } else if let Some(digits) = magnitude.strip_prefix("0b") {
        (2, digits)
    } else {
        (10, magnitude)
    };
    let magnitude = u128::from_str_radix(digits, radix).ok()?;
    Some(if negative {
        format!("-{magnitude}")
    } else {
        magnitude.to_string()
    })
}

fn unsupported_module(construct: &'static str, start: usize, end: usize) -> EmitError {
    EmitError::Unsupported {
        construct,
        start,
        end,
    }
}

fn unsupported_expr(construct: &'static str, span: Span) -> EmitError {
    unsupported_module(construct, span.start, span.end)
}

const C_I32_RUNTIME: &str = r#"#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int32_t tinyml_i32_from_bits(uint32_t bits) {
    int32_t value;
    memcpy(&value, &bits, sizeof(value));
    return value;
}

static int32_t tinyml_i32_literal(const char *decimal) {
    char *end = NULL;
    errno = 0;
    intmax_t value = strtoimax(decimal, &end, 10);
    if (errno != 0 || end == decimal || *end != '\0' || value < INT32_MIN || value > INT32_MAX) {
        fputs("invalid checked i32 literal\n", stderr);
        abort();
    }
    return (int32_t)value;
}

static int32_t tinyml_i32_add(int32_t left, int32_t right) {
    return tinyml_i32_from_bits((uint32_t)left + (uint32_t)right);
}

static int32_t tinyml_i32_sub(int32_t left, int32_t right) {
    return tinyml_i32_from_bits((uint32_t)left - (uint32_t)right);
}

static int32_t tinyml_i32_mul(int32_t left, int32_t right) {
    return tinyml_i32_from_bits((uint32_t)left * (uint32_t)right);
}

static int32_t tinyml_i32_div(int32_t left, int32_t right) {
    if (right == 0) {
        fputs("i32 division by zero\n", stderr);
        abort();
    }
    if (left == INT32_MIN && right == -1) return INT32_MIN;
    return left / right;
}

static int32_t tinyml_i32_rem(int32_t left, int32_t right) {
    if (right == 0) {
        fputs("i32 remainder by zero\n", stderr);
        abort();
    }
    if (left == INT32_MIN && right == -1) return 0;
    return left % right;
}
"#;

#[cfg(test)]
mod tests {
    use crate::{
        checker::Checker,
        lambda::ir,
        parser::{lexer::Lexer, parse::Parser},
    };

    use super::*;

    fn checked_lambda(source: &str) -> ir::Module {
        let tokens = Lexer::new(source).expect("source lexes");
        let ast = Parser::new(tokens).parse_module().expect("source parses");
        let summary = Checker::new().check_module(&ast);
        assert!(summary.diagnostics.is_empty(), "{:?}", summary.diagnostics);
        crate::lambda::translate(&summary.tir)
    }

    #[test]
    fn emits_c_for_status_42_entry() {
        let emitted = emit_main_i32(&checked_lambda("let main = fn () -> 42")).unwrap();
        assert!(emitted.contains("static int32_t tinyml_entry(void)"));
        assert!(emitted.contains("tinyml_i32_literal(\"42\")"));
        assert!(emitted.contains("return (int)tinyml_entry();"));
    }

    #[test]
    fn emits_wrapping_i32_operations() {
        let emitted = emit_main_i32(&checked_lambda("let main = fn () -> 2147483647 + 1")).unwrap();
        assert!(emitted.contains("tinyml_i32_add("));
        assert!(emitted.contains("tinyml_i32_from_bits"));
    }

    #[test]
    fn rejects_non_i32_entry_without_fallback() {
        let error = emit_main_i32(&checked_lambda("let main = fn () -> true")).unwrap_err();
        assert!(error.to_string().contains("`main : unit -> i32`"));
    }

    #[test]
    fn emits_block_bindings_to_local_c_variables() {
        let emitted = emit_main_i32(&checked_lambda(
            "let main = fn () -> { let x = 20; x + 22 }",
        ))
        .unwrap();
        assert!(emitted.contains("int32_t tinyml_local_0 = tinyml_i32_literal(\"20\");"));
        assert!(emitted.contains("tinyml_i32_add(tinyml_local_0,"));
    }
}
