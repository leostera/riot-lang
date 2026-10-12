//! C source emission for the Rust Stage 0 bootstrap compiler.
//!
//! The output uses the existing `riotc/runtime.h` ABI for dynamic TinyML values,
//! closures, aggregates, and Host helpers. This is a bootstrap bridge only; it
//! is not the Riotc-owned ARM backend and never supplies a C fallback for it.

use std::fmt::Write as _;

use thiserror::Error;

use crate::{
    checker::{self, builtin, tir},
    lambda::{self, ir as lambda_ir},
    modules::{IntrinsicModule, ModuleKind, SourceModule},
    parser::{ast, ast::Literal, ast::UseDecl, ident::Ident, lexer::Span},
};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum EmitError {
    #[error("Stage 0 C emission does not support {construct} at bytes {start}..{end}")]
    Unsupported {
        construct: &'static str,
        start: usize,
        end: usize,
    },
    #[error("Stage 0 C emission could not resolve module import `{module}`")]
    MissingDependency { module: String },
    #[error("Stage 0 C emission requires a top-level `main : unit -> i32` entry")]
    MissingEntry,
}

type CheckedSource<'a> = (&'a SourceModule, &'a checker::ModuleSummary);

/// Emit a dependency-first checked module graph as one C translation unit.
///
/// Imports become bindings between module environments. Host and String
/// intrinsics are provided by the existing C runtime; ordinary source modules
/// are lowered from typed LambdaIR. The generated file is compiled with
/// `riotc/runtime.c` and `-Iriotc`.
pub fn emit_graph(modules: &[CheckedSource<'_>], root: usize) -> Result<String, EmitError> {
    let Some((_, root_summary)) = modules.get(root).copied() else {
        return Err(EmitError::MissingEntry);
    };
    ensure_entry(&root_summary.tir)?;

    let mut emitter = CEmitter::default();
    let mut initializers = Vec::with_capacity(modules.len());
    for (index, (source, summary)) in modules.iter().copied().enumerate() {
        initializers.push(emitter.emit_module_initializer(index, source, summary, modules)?);
    }

    let mut output = String::from("#include <stdlib.h>\n#include \"runtime.h\"\n\n");
    for index in 0..modules.len() {
        writeln!(output, "static RtValue module_environment_{index};")
            .expect("writing C into a String cannot fail");
    }
    output.push('\n');
    for function in emitter.functions {
        output.push_str(&function);
        output.push('\n');
    }

    output.push_str("int main(int argc, char **argv) {\n");
    output.push_str("  rt_set_args(argc, argv);\n");
    output.push_str("  const char *runtime_directory = getenv(\"RIOTML_RUNTIME_DIR\");\n");
    output.push_str(
        "  rt_set_runtime_directory(runtime_directory == NULL ? \"\" : runtime_directory);\n",
    );
    output.push_str("  RtValue root_environment = rt_environment_root();\n");
    for index in 0..modules.len() {
        writeln!(
            output,
            "  module_environment_{index} = rt_environment_child(root_environment);"
        )
        .expect("writing C into a String cannot fail");
    }
    for initializer in initializers {
        for line in initializer.lines() {
            writeln!(output, "  {line}").expect("writing C into a String cannot fail");
        }
    }
    writeln!(
        output,
        "  RtValue entry = rt_environment_get(module_environment_{root}, \"main\");"
    )
    .expect("writing C into a String cannot fail");
    output.push_str("  return (int)rt_as_int(rt_apply(entry, rt_unit()));\n}\n");

    Ok(output)
}

fn ensure_entry(module: &tir::Module) -> Result<(), EmitError> {
    let lambda = lambda::translate(module);
    let Some(entry) = lambda
        .values
        .iter()
        .find(|value| value.name.as_name() == Some("main"))
    else {
        return Err(EmitError::MissingEntry);
    };
    let tir::Type::Arrow { parameter, result } = &entry.ty else {
        return Err(EmitError::MissingEntry);
    };
    if !builtin::is_unqualified_type(parameter, "unit")
        || !builtin::is_unqualified_type(result, "i32")
    {
        return Err(EmitError::MissingEntry);
    }
    Ok(())
}

#[derive(Default)]
struct CEmitter {
    functions: Vec<String>,
    next_function: usize,
    next_temporary: usize,
}

impl CEmitter {
    fn emit_module_initializer(
        &mut self,
        index: usize,
        source: &SourceModule,
        summary: &checker::ModuleSummary,
        modules: &[CheckedSource<'_>],
    ) -> Result<String, EmitError> {
        let environment = format!("module_environment_{index}");
        let mut statements = String::new();

        self.emit_imports(source, &environment, modules, &mut statements)?;
        match source.kind {
            ModuleKind::Intrinsic(IntrinsicModule::Host) => {
                emit_host_bindings(&environment, &mut statements);
            }
            ModuleKind::Intrinsic(IntrinsicModule::String) => {
                emit_string_bindings(&environment, &mut statements);
            }
            ModuleKind::Source => {
                let lambda = lambda::translate(&summary.tir);
                for (value_index, value) in lambda.values.iter().enumerate() {
                    let expression = self.emit_expression(
                        &value.body,
                        &environment,
                        &format!("module{index}_value{value_index}"),
                    )?;
                    writeln!(
                        statements,
                        "rt_environment_bind({environment}, {}, {expression});",
                        c_string(&ident_text(&value.name))
                    )
                    .expect("writing C into a String cannot fail");
                }
            }
        }
        Ok(statements)
    }

    fn emit_imports(
        &self,
        source: &SourceModule,
        target_environment: &str,
        modules: &[CheckedSource<'_>],
        output: &mut String,
    ) -> Result<(), EmitError> {
        for import in source.ast.items.iter().filter_map(|item| match item {
            ast::ModuleItem::UseDecl(import) => Some(import),
            _ => None,
        }) {
            let (module_name, selected) = match import {
                UseDecl::Concrete {
                    module, members, ..
                } => (module, Some(members.as_slice())),
                UseDecl::Wildcard { module, .. } => (module, None),
            };
            let dependency_index = source
                .dependencies
                .iter()
                .copied()
                .find(|dependency| {
                    modules
                        .get(*dependency)
                        .and_then(|(dependency, _)| dependency.name.as_ref())
                        .is_some_and(|name| name.to_ident().same_path(module_name))
                })
                .ok_or_else(|| EmitError::MissingDependency {
                    module: ident_text(module_name),
                })?;
            let dependency_environment = format!("module_environment_{dependency_index}");
            let dependency_interface = &modules[dependency_index].1.interface;
            for value in &dependency_interface.values {
                let value_name = ident_text(&value.name);
                let selected_value = selected.is_none_or(|members| {
                    members.iter().any(|member| member.same_path(&value.name))
                });
                if selected_value {
                    writeln!(
                        output,
                        "rt_environment_bind({target_environment}, {}, rt_environment_get({dependency_environment}, {}));",
                        c_string(&value_name),
                        c_string(&value_name)
                    )
                    .expect("writing C into a String cannot fail");
                }
            }
        }
        Ok(())
    }

    fn emit_expression(
        &mut self,
        expression: &lambda_ir::Expr,
        environment: &str,
        path: &str,
    ) -> Result<String, EmitError> {
        match &expression.kind {
            lambda_ir::ExprKind::Constant(constant) => {
                self.emit_constant(constant, &expression.ty, expression.span)
            }
            lambda_ir::ExprKind::Var { name, .. } => Ok(format!(
                "rt_environment_get({environment}, {})",
                c_string(&ident_text(name))
            )),
            lambda_ir::ExprKind::Construct { name, args, .. } => {
                let call_prefix = format!(
                    "rt_variant_named({}, {},",
                    c_string(&ident_text(name)),
                    args.len()
                );
                self.emit_call_with_values(&call_prefix, args, environment, &format!("{path}_arg"))
            }
            lambda_ir::ExprKind::Function { params, body } => {
                self.emit_function(params, body, environment, path)
            }
            lambda_ir::ExprKind::Let { bind, body, .. } => {
                self.emit_let(bind, body, environment, path)
            }
            lambda_ir::ExprKind::Apply { callee, args } => {
                self.emit_application(callee, args, environment, path)
            }
            lambda_ir::ExprKind::Prim { op, args } => {
                self.emit_primitive(op, args, environment, path, expression.span)
            }
            lambda_ir::ExprKind::Tuple(items) => {
                let call_prefix = format!("rt_tuple({},", items.len());
                self.emit_call_with_values(
                    &call_prefix,
                    items,
                    environment,
                    &format!("{path}_item"),
                )
            }
            lambda_ir::ExprKind::Record { name, fields } => {
                self.emit_record(name, fields, environment, path)
            }
            lambda_ir::ExprKind::Field { base, field } => {
                let base = self.emit_expression(base, environment, &format!("{path}_base"))?;
                Ok(format!(
                    "rt_field_named({base}, {})",
                    c_string(&ident_text(field))
                ))
            }
            lambda_ir::ExprKind::Block(expressions) => {
                self.emit_block(expressions, environment, path, expression.span)
            }
            lambda_ir::ExprKind::Match { scrutinee, arms } => {
                self.emit_match(scrutinee, arms, environment, path)
            }
        }
    }

    fn emit_constant(
        &self,
        constant: &lambda_ir::Constant,
        _ty: &tir::Type,
        span: Span,
    ) -> Result<String, EmitError> {
        match constant {
            lambda_ir::Constant::Unit => Ok("rt_unit()".to_owned()),
            lambda_ir::Constant::Literal(Literal::Bool(value)) => {
                Ok(format!("rt_bool({})", i32::from(*value)))
            }
            lambda_ir::Constant::Literal(Literal::Int(value)) => {
                let Some(value) = integer_literal_as_decimal(value) else {
                    return Err(unsupported("an invalid checked integer literal", span));
                };
                let Some(value) = value.parse::<i64>().ok() else {
                    return Err(unsupported(
                        "an integer outside the C runtime's signed 64-bit value range",
                        span,
                    ));
                };
                Ok(format!("rt_int(INT64_C({value}))"))
            }
            lambda_ir::Constant::Literal(Literal::String(value)) => {
                let value = decode_string_literal(value);
                if value.contains('\0') {
                    return Err(unsupported(
                        "a string literal containing an embedded NUL byte",
                        span,
                    ));
                }
                Ok(format!("rt_string({})", c_string(&value)))
            }
            lambda_ir::Constant::Literal(Literal::Float(_)) => {
                Err(unsupported("a floating-point literal", span))
            }
        }
    }

    fn emit_function(
        &mut self,
        params: &[lambda_ir::Pattern],
        body: &lambda_ir::Expr,
        environment: &str,
        path: &str,
    ) -> Result<String, EmitError> {
        let Some((parameter, remaining)) = params.split_first() else {
            return Err(unsupported("a zero-argument function value", body.span));
        };
        let function_id = self.next_function;
        self.next_function += 1;
        let function_name = format!("riotml_lambda_{function_id}");
        let function_environment = format!("function_environment_{function_id}");
        let local_environment = format!("local_environment_{function_id}");

        let result = if remaining.is_empty() {
            self.emit_expression(body, &local_environment, &format!("{path}_body"))?
        } else {
            self.emit_function(
                remaining,
                body,
                &local_environment,
                &format!("{path}_curried"),
            )?
        };
        let condition = self.emit_pattern_condition(parameter, "argument")?;
        let bindings = self.emit_pattern_bindings(parameter, "argument", &local_environment)?;

        let mut definition = String::new();
        writeln!(
            definition,
            "static RtValue {function_name}(RtValue {function_environment}, RtValue argument) {{"
        )
        .expect("writing C into a String cannot fail");
        writeln!(
            definition,
            "  RtValue {local_environment} = rt_environment_child({function_environment});"
        )
        .expect("writing C into a String cannot fail");
        writeln!(definition, "  if (!({condition})) rt_pattern_fail();")
            .expect("writing C into a String cannot fail");
        definition.push_str(&bindings);
        writeln!(definition, "  return {result};\n}}")
            .expect("writing C into a String cannot fail");
        self.functions.push(definition);
        Ok(format!("rt_function({function_name}, {environment})"))
    }

    fn emit_let(
        &mut self,
        pattern: &lambda_ir::Pattern,
        value: &lambda_ir::Expr,
        environment: &str,
        path: &str,
    ) -> Result<String, EmitError> {
        let value_name = self.fresh_temporary("let_value");
        let local_environment = self.fresh_temporary("let_environment");
        let value = self.emit_expression(value, environment, &format!("{path}_value"))?;
        let condition = self.emit_pattern_condition(pattern, &value_name)?;
        let bindings = self.emit_pattern_bindings(pattern, &value_name, &local_environment)?;
        let mut result = format!(
            "({{ RtValue {value_name} = {value}; RtValue {local_environment} = rt_environment_child({environment}); if (!({condition})) rt_pattern_fail(); {bindings} {value_name}; }})"
        );
        if result.is_empty() {
            result = "rt_unit()".to_owned();
        }
        Ok(result)
    }

    fn emit_application(
        &mut self,
        callee: &lambda_ir::Expr,
        args: &[lambda_ir::Expr],
        environment: &str,
        path: &str,
    ) -> Result<String, EmitError> {
        let callee_value = self.emit_expression(callee, environment, &format!("{path}_callee"))?;
        let mut output = String::from("({ ");
        let mut current = self.fresh_temporary("callee");
        write!(output, "RtValue {current} = {callee_value}; ")
            .expect("writing C into a String cannot fail");
        for (index, argument) in args.iter().enumerate() {
            let argument_value =
                self.emit_expression(argument, environment, &format!("{path}_argument{index}"))?;
            let argument_name = self.fresh_temporary("argument");
            let result_name = self.fresh_temporary("application");
            write!(
                output,
                "RtValue {argument_name} = {argument_value}; RtValue {result_name} = rt_apply({current}, {argument_name}); "
            )
            .expect("writing C into a String cannot fail");
            current = result_name;
        }
        write!(output, "{current}; }})").expect("writing C into a String cannot fail");
        Ok(output)
    }

    fn emit_primitive(
        &mut self,
        operation: &lambda_ir::Primitive,
        args: &[lambda_ir::Expr],
        environment: &str,
        path: &str,
        span: Span,
    ) -> Result<String, EmitError> {
        let [left, right] = args else {
            return Err(unsupported("a non-binary primitive operation", span));
        };
        let lowering = primitive_helper(operation, span)?;
        let left_value = self.emit_expression(left, environment, &format!("{path}_left"))?;
        let right_value = self.emit_expression(right, environment, &format!("{path}_right"))?;
        let left_name = self.fresh_temporary("left");
        let right_name = self.fresh_temporary("right");
        let call = format!("{}({left_name}, {right_name})", lowering.helper);
        let result = if lowering.negate_result {
            format!("rt_bool(!rt_as_bool({call}))")
        } else {
            call
        };
        Ok(format!(
            "({{ RtValue {left_name} = {left_value}; RtValue {right_name} = {right_value}; {result}; }})"
        ))
    }

    fn emit_record(
        &mut self,
        name: &Ident,
        fields: &[(Ident, lambda_ir::Expr)],
        environment: &str,
        path: &str,
    ) -> Result<String, EmitError> {
        let mut output = String::from("({ ");
        let mut values = Vec::with_capacity(fields.len());
        for (index, (_, expression)) in fields.iter().enumerate() {
            let expression =
                self.emit_expression(expression, environment, &format!("{path}_field{index}"))?;
            let temporary = self.fresh_temporary("field");
            writeln!(output, "RtValue {temporary} = {expression};")
                .expect("writing C into a String cannot fail");
            values.push(temporary);
        }
        let names = c_string_array(fields.iter().map(|(name, _)| ident_text(name)));
        let values = c_value_array(&values);
        write!(
            output,
            "rt_record_named({}, {}, {names}, {values}); }})",
            c_string(&ident_text(name)),
            fields.len()
        )
        .expect("writing C into a String cannot fail");
        Ok(output)
    }

    fn emit_call_with_values(
        &mut self,
        call_prefix: &str,
        expressions: &[lambda_ir::Expr],
        environment: &str,
        path: &str,
    ) -> Result<String, EmitError> {
        let mut output = String::from("({ ");
        let mut values = Vec::with_capacity(expressions.len());
        for (index, expression) in expressions.iter().enumerate() {
            let expression =
                self.emit_expression(expression, environment, &format!("{path}{index}"))?;
            let temporary = self.fresh_temporary("item");
            write!(output, "RtValue {temporary} = {expression}; ")
                .expect("writing C into a String cannot fail");
            values.push(temporary);
        }
        let values = c_value_array(&values);
        write!(output, "{call_prefix} {values}); }})")
            .expect("writing C into a String cannot fail");
        Ok(output)
    }

    fn emit_block(
        &mut self,
        expressions: &[lambda_ir::Expr],
        environment: &str,
        path: &str,
        span: Span,
    ) -> Result<String, EmitError> {
        let local_environment = self.fresh_temporary("block_environment");
        let result = self.fresh_temporary("block_result");
        let mut output = format!(
            "({{ RtValue {local_environment} = rt_environment_child({environment}); RtValue {result} = rt_unit(); "
        );
        for (index, expression) in expressions.iter().enumerate() {
            match &expression.kind {
                lambda_ir::ExprKind::Let { bind, body, .. } => {
                    let value = self.emit_expression(
                        body,
                        &local_environment,
                        &format!("{path}_let{index}"),
                    )?;
                    let value_name = self.fresh_temporary("block_let");
                    let condition = self.emit_pattern_condition(bind, &value_name)?;
                    let bindings =
                        self.emit_pattern_bindings(bind, &value_name, &local_environment)?;
                    write!(
                        output,
                        "RtValue {value_name} = {value}; if (!({condition})) rt_pattern_fail(); {bindings} {result} = {value_name}; "
                    )
                    .expect("writing C into a String cannot fail");
                }
                _ => {
                    let value = self.emit_expression(
                        expression,
                        &local_environment,
                        &format!("{path}_item{index}"),
                    )?;
                    write!(output, "{result} = {value}; ")
                        .expect("writing C into a String cannot fail");
                }
            }
        }
        let _ = span;
        write!(output, "{result}; }})").expect("writing C into a String cannot fail");
        Ok(output)
    }

    fn emit_match(
        &mut self,
        scrutinee: &lambda_ir::Expr,
        arms: &[lambda_ir::MatchArm],
        environment: &str,
        path: &str,
    ) -> Result<String, EmitError> {
        let scrutinee_value =
            self.emit_expression(scrutinee, environment, &format!("{path}_scrutinee"))?;
        let scrutinee_name = self.fresh_temporary("match_value");
        let result_name = self.fresh_temporary("match_result");
        let found_name = self.fresh_temporary("match_found");
        let mut output = format!(
            "({{ RtValue {scrutinee_name} = {scrutinee_value}; RtValue {result_name} = rt_unit(); int {found_name} = 0; "
        );
        for (index, arm) in arms.iter().enumerate() {
            let condition = self.emit_pattern_condition(&arm.pattern, &scrutinee_name)?;
            let arm_environment = self.fresh_temporary("match_environment");
            let bindings =
                self.emit_pattern_bindings(&arm.pattern, &scrutinee_name, &arm_environment)?;
            let body =
                self.emit_expression(&arm.body, &arm_environment, &format!("{path}_arm{index}"))?;
            let branch = if index == 0 { "if" } else { "else if" };
            write!(
                output,
                "{branch} ({condition}) {{ RtValue {arm_environment} = rt_environment_child({environment}); {bindings} {result_name} = {body}; {found_name} = 1; }} "
            )
            .expect("writing C into a String cannot fail");
        }
        write!(
            output,
            "if (!{found_name}) rt_pattern_fail(); {result_name}; }})"
        )
        .expect("writing C into a String cannot fail");
        Ok(output)
    }

    fn emit_pattern_condition(
        &self,
        pattern: &lambda_ir::Pattern,
        value: &str,
    ) -> Result<String, EmitError> {
        match &pattern.kind {
            lambda_ir::PatternKind::Wildcard | lambda_ir::PatternKind::Var { .. } => {
                Ok("1".to_owned())
            }
            lambda_ir::PatternKind::Constant(lambda_ir::Constant::Unit) => {
                Ok(format!("rt_is_unit({value})"))
            }
            lambda_ir::PatternKind::Constant(lambda_ir::Constant::Literal(Literal::Bool(
                expected,
            ))) => Ok(format!(
                "(rt_is_bool({value}) && rt_as_bool({value}) == {})",
                i32::from(*expected)
            )),
            lambda_ir::PatternKind::Constant(lambda_ir::Constant::Literal(Literal::Int(
                expected,
            ))) => {
                let Some(literal) = integer_literal_as_decimal(expected) else {
                    return Err(unsupported("an invalid integer pattern", pattern.span));
                };
                let Some(literal) = literal.parse::<i64>().ok() else {
                    return Err(unsupported(
                        "an integer pattern outside the signed 64-bit range",
                        pattern.span,
                    ));
                };
                Ok(format!(
                    "(rt_is_int({value}) && rt_as_int({value}) == INT64_C({literal}))"
                ))
            }
            lambda_ir::PatternKind::Constant(lambda_ir::Constant::Literal(Literal::String(
                expected,
            ))) => {
                let expected = decode_string_literal(expected);
                if expected.contains('\0') {
                    return Err(unsupported(
                        "a string pattern containing an embedded NUL byte",
                        pattern.span,
                    ));
                }
                Ok(format!(
                    "(rt_is_string({value}) && rt_as_bool(rt_string_equal_to({value}, {})))",
                    c_string(&expected)
                ))
            }
            lambda_ir::PatternKind::Constant(lambda_ir::Constant::Literal(Literal::Float(_))) => {
                Err(unsupported("a floating-point pattern", pattern.span))
            }
            lambda_ir::PatternKind::Construct { name, args, .. } => {
                let checks = args
                    .iter()
                    .enumerate()
                    .map(|(index, argument)| {
                        self.emit_pattern_condition(
                            argument,
                            &format!("rt_field({value}, {index})"),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let fields = if checks.is_empty() {
                    "1".to_owned()
                } else {
                    checks.join(" && ")
                };
                Ok(format!(
                    "(rt_has_variant_name({value}, {}) && rt_has_variant_count({value}, {}) && ({fields}))",
                    c_string(&ident_text(name)),
                    args.len()
                ))
            }
            lambda_ir::PatternKind::Tuple(items) => {
                let checks = items
                    .iter()
                    .enumerate()
                    .map(|(index, item)| {
                        self.emit_pattern_condition(item, &format!("rt_field({value}, {index})"))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let fields = if checks.is_empty() {
                    "1".to_owned()
                } else {
                    checks.join(" && ")
                };
                Ok(format!(
                    "(rt_has_tuple_count({value}, {}) && ({fields}))",
                    items.len()
                ))
            }
            lambda_ir::PatternKind::Record { name, fields } => {
                let checks = fields
                    .iter()
                    .map(|(field, pattern)| {
                        self.emit_pattern_condition(
                            pattern,
                            &format!("rt_field_named({value}, {})", c_string(&ident_text(field))),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let field_tests = fields
                    .iter()
                    .map(|(field, _)| {
                        format!(
                            "rt_has_named_field({value}, {})",
                            c_string(&ident_text(field))
                        )
                    })
                    .chain(checks)
                    .collect::<Vec<_>>();
                let fields = if field_tests.is_empty() {
                    "1".to_owned()
                } else {
                    field_tests.join(" && ")
                };
                Ok(format!(
                    "(rt_has_variant_name({value}, {}) && ({fields}))",
                    c_string(&ident_text(name))
                ))
            }
        }
    }

    fn emit_pattern_bindings(
        &self,
        pattern: &lambda_ir::Pattern,
        value: &str,
        environment: &str,
    ) -> Result<String, EmitError> {
        let mut output = String::new();
        self.append_pattern_bindings(pattern, value, environment, &mut output)?;
        Ok(output)
    }

    fn append_pattern_bindings(
        &self,
        pattern: &lambda_ir::Pattern,
        value: &str,
        environment: &str,
        output: &mut String,
    ) -> Result<(), EmitError> {
        match &pattern.kind {
            lambda_ir::PatternKind::Wildcard
            | lambda_ir::PatternKind::Constant(_)
            | lambda_ir::PatternKind::Construct { .. } => {
                if let lambda_ir::PatternKind::Construct { args, .. } = &pattern.kind {
                    for (index, argument) in args.iter().enumerate() {
                        self.append_pattern_bindings(
                            argument,
                            &format!("rt_field({value}, {index})"),
                            environment,
                            output,
                        )?;
                    }
                }
            }
            lambda_ir::PatternKind::Var { name, .. } => {
                writeln!(
                    output,
                    "rt_environment_bind({environment}, {}, {value});",
                    c_string(&ident_text(name))
                )
                .expect("writing C into a String cannot fail");
            }
            lambda_ir::PatternKind::Tuple(items) => {
                for (index, item) in items.iter().enumerate() {
                    self.append_pattern_bindings(
                        item,
                        &format!("rt_field({value}, {index})"),
                        environment,
                        output,
                    )?;
                }
            }
            lambda_ir::PatternKind::Record { fields, .. } => {
                for (field, pattern) in fields {
                    self.append_pattern_bindings(
                        pattern,
                        &format!("rt_field_named({value}, {})", c_string(&ident_text(field))),
                        environment,
                        output,
                    )?;
                }
            }
        }
        Ok(())
    }

    fn fresh_temporary(&mut self, prefix: &str) -> String {
        let id = self.next_temporary;
        self.next_temporary += 1;
        format!("tinyml_{prefix}_{id}")
    }
}

fn emit_host_bindings(environment: &str, output: &mut String) {
    for (name, function) in [
        ("args", "rt_host_args"),
        ("current_directory", "rt_host_current_directory"),
        ("runtime_directory", "rt_host_runtime_directory"),
        ("read_file", "rt_read_file"),
        ("write_file", "rt_write_file"),
        ("write_stdout", "rt_write_stdout"),
        ("write_stderr", "rt_write_stderr"),
        ("run_process", "rt_run_process"),
    ] {
        writeln!(
            output,
            "rt_environment_bind({environment}, {}, rt_function({function}, rt_unit()));",
            c_string(name)
        )
        .expect("writing C into a String cannot fail");
    }
}

fn emit_string_bindings(environment: &str, output: &mut String) {
    for (name, function) in [
        ("concat", "rt_string_concat"),
        ("length", "rt_string_length"),
        ("get", "rt_string_get"),
        ("slice", "rt_string_slice"),
        ("equal", "rt_string_equal"),
    ] {
        writeln!(
            output,
            "rt_environment_bind({environment}, {}, rt_function({function}, rt_unit()));",
            c_string(name)
        )
        .expect("writing C into a String cannot fail");
    }
}

struct PrimitiveLowering {
    helper: &'static str,
    negate_result: bool,
}

fn primitive_helper(
    operation: &lambda_ir::Primitive,
    span: Span,
) -> Result<PrimitiveLowering, EmitError> {
    use lambda_ir::Primitive as P;
    let (helper, negate_result) = match operation {
        P::Add { operand } if builtin::type_name(operand) == Some("i32") => ("rt_i32_add", false),
        P::Add { operand } if builtin::type_name(operand) == Some("string") => ("rt_concat", false),
        P::Sub { operand } if builtin::type_name(operand) == Some("i32") => ("rt_i32_sub", false),
        P::Mul { operand } if builtin::type_name(operand) == Some("i32") => ("rt_i32_mul", false),
        P::Div { operand } if builtin::type_name(operand) == Some("i32") => ("rt_i32_div", false),
        P::Rem { operand } if builtin::type_name(operand) == Some("i32") => ("rt_i32_rem", false),
        P::Eq { .. } => ("rt_equal", false),
        P::Ne { .. } => ("rt_equal", true),
        P::Lt { operand } if builtin::type_name(operand) == Some("i32") => ("rt_i32_less", false),
        P::Lte { operand } if builtin::type_name(operand) == Some("i32") => {
            ("rt_i32_greater", true)
        }
        P::Gt { operand } if builtin::type_name(operand) == Some("i32") => {
            ("rt_i32_greater", false)
        }
        P::Gte { operand } if builtin::type_name(operand) == Some("i32") => ("rt_i32_less", true),
        P::Lt { .. } => ("rt_less", false),
        P::Lte { .. } => ("rt_greater", true),
        P::Gt { .. } => ("rt_greater", false),
        P::Gte { .. } => ("rt_less", true),
        _ => return Err(unsupported("this typed primitive operation", span)),
    };
    Ok(PrimitiveLowering {
        helper,
        negate_result,
    })
}

fn decode_string_literal(value: &str) -> String {
    let body = value
        .strip_prefix('"')
        .and_then(|value| value.strip_suffix('"'))
        .unwrap_or(value);
    let mut decoded = String::with_capacity(body.len());
    let mut characters = body.chars();
    while let Some(character) = characters.next() {
        if character != '\\' {
            decoded.push(character);
            continue;
        }
        match characters.next() {
            Some('n') => decoded.push('\n'),
            Some('r') => decoded.push('\r'),
            Some('t') => decoded.push('\t'),
            Some('0') => decoded.push('\0'),
            Some('\\') => decoded.push('\\'),
            Some('"') => decoded.push('"'),
            Some('b') => decoded.push('\u{0008}'),
            Some('f') => decoded.push('\u{000c}'),
            Some('v') => decoded.push('\u{000b}'),
            Some(other) => {
                decoded.push('\\');
                decoded.push(other);
            }
            None => decoded.push('\\'),
        }
    }
    decoded
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

fn ident_text(ident: &Ident) -> String {
    match ident {
        Ident::Name { name, .. } => name.clone(),
        Ident::Qualified { name, rest, .. } => format!("{name}_{}", ident_text(rest)),
    }
}

fn c_string(value: &str) -> String {
    let mut output = String::from("\"");
    for byte in value.bytes() {
        match byte {
            b'"' => output.push_str("\\\""),
            b'\\' => output.push_str("\\\\"),
            b'\n' => output.push_str("\\n"),
            b'\r' => output.push_str("\\r"),
            b'\t' => output.push_str("\\t"),
            b'\0' => output.push_str("\\000"),
            0x20..=0x7e => output.push(byte as char),
            _ => write!(output, "\\{byte:03o}").expect("writing C string cannot fail"),
        }
    }
    output.push('"');
    output
}

fn c_value_array(values: &[String]) -> String {
    if values.is_empty() {
        "NULL".to_owned()
    } else {
        format!("(RtValue[]){{{}}}", values.join(", "))
    }
}

fn c_string_array<'a>(values: impl Iterator<Item = String>) -> String {
    let values = values.map(|value| c_string(&value)).collect::<Vec<_>>();
    if values.is_empty() {
        "NULL".to_owned()
    } else {
        format!("(const char*[]){{{}}}", values.join(", "))
    }
}

fn unsupported(construct: &'static str, span: Span) -> EmitError {
    EmitError::Unsupported {
        construct,
        start: span.start,
        end: span.end,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn c_strings_escape_literals_and_control_characters() {
        assert_eq!(
            c_string("quote\" slash\\ line\n"),
            "\"quote\\\" slash\\\\ line\\n\""
        );
    }
}
