use std::collections::HashMap;

use crate::ast::{AstExpr, AstFnDecl, AstTypeAnnotation};
use crate::signature::RsigType;

use super::{
    ValidationContext, const_eval::resolve_const_value, const_value_type,
    infer_annotation_block_type, type_matches, types::parse_primitive_type,
};

pub(super) struct TypeAnnotationChecker<'a, 'ctx> {
    ctx: &'ctx ValidationContext<'a>,
}

impl<'a, 'ctx> TypeAnnotationChecker<'a, 'ctx> {
    pub(super) fn new(ctx: &'ctx ValidationContext<'a>) -> Self {
        Self { ctx }
    }

    pub(super) fn validate_source_spelling(
        &self,
        annotation: &AstTypeAnnotation,
    ) -> miette::Result<()> {
        if type_text_has_token(&annotation.text, "string") {
            return Err(self
                .ctx
                .diagnostic(
                    annotation.span,
                    "invalid type spelling",
                    "`string` is not a source-level type name",
                    Some("use `String`"),
                )
                .into());
        }
        Ok(())
    }

    pub(super) fn validate_binding(
        &self,
        annotation: &AstTypeAnnotation,
        value: &AstExpr,
    ) -> miette::Result<()> {
        self.validate_source_spelling(annotation)?;
        if annotation.text == "float" {
            let error = parse_primitive_type(&annotation.text).unwrap_err();
            return Err(self
                .ctx
                .diagnostic(
                    annotation.span,
                    "unsupported type annotation",
                    error.message,
                    error.help,
                )
                .into());
        }

        if parse_primitive_type(&annotation.text).is_err() && !annotation.text.starts_with('(') {
            return Ok(());
        }

        let value_type = resolve_const_value(value, &HashMap::new(), &self.ctx.functions)
            .map(|value| const_value_type(&value));
        if let Some(value_type) = value_type {
            let expected = self.ctx.lower_type_annotation(annotation);
            if type_matches(&expected, &value_type) {
                return Ok(());
            }
            return Err(self
                .ctx
                .diagnostic(
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

    pub(super) fn validate_function(&self, function: &AstFnDecl) -> miette::Result<()> {
        let mut bindings = HashMap::new();
        for (index, annotation) in function.param_types.iter().enumerate() {
            let Some(annotation) = annotation else {
                continue;
            };
            self.validate_function_abi(annotation)?;
            if let Some(param) = function.params.get(index) {
                bindings.insert(param.clone(), self.ctx.lower_type_annotation(annotation));
            }
        }

        let Some(return_annotation) = &function.return_type else {
            return Ok(());
        };
        self.validate_function_abi(return_annotation)?;

        let expected = self.ctx.lower_type_annotation(return_annotation);
        let actual = infer_annotation_block_type(self.ctx, &function.body, &mut bindings);
        if let Some(actual) = actual
            && !type_matches(&expected, &actual)
        {
            return Err(self
                .ctx
                .diagnostic(
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

    fn validate_function_abi(&self, annotation: &AstTypeAnnotation) -> miette::Result<()> {
        self.validate_source_spelling(annotation)?;
        if annotation.text == "float" {
            let error = parse_primitive_type(&annotation.text).unwrap_err();
            return Err(self
                .ctx
                .diagnostic(
                    annotation.span,
                    "unsupported function ABI annotation",
                    error.message,
                    error.help,
                )
                .into());
        }

        let type_ = self.ctx.lower_type_annotation(annotation);
        if matches!(
            type_,
            RsigType::I32
                | RsigType::I64
                | RsigType::Bool
                | RsigType::Unit
                | RsigType::ActorId(_)
                | RsigType::String
                | RsigType::Tuple(_)
                | RsigType::List(_)
                | RsigType::Record(_)
                | RsigType::Variant(_)
                | RsigType::VariantApp { .. }
                | RsigType::Arrow { .. }
        ) {
            return Ok(());
        }

        Err(self
            .ctx
            .diagnostic(
                annotation.span,
                "unsupported function ABI annotation",
                format!(
                    "`{}` is not supported for native Riot function parameters or returns yet",
                    annotation.text
                ),
                Some("use a scalar type or a boxed value type such as `String`, a tuple, a list, or a record"),
            )
            .into())
    }
}

fn type_text_has_token(text: &str, token: &str) -> bool {
    text.split(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '_'))
        .any(|part| part == token)
}
