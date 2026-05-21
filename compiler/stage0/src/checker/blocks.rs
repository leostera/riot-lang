use std::collections::HashMap;

use crate::ast::{AstBlock, AstTypeAnnotation};

use super::{BindingKind, ValidationContext};

pub(super) struct BlockValidator<'ctx, 'source> {
    ctx: &'ctx ValidationContext<'source>,
}

impl<'ctx, 'source> BlockValidator<'ctx, 'source> {
    pub(super) fn new(ctx: &'ctx ValidationContext<'source>) -> Self {
        Self { ctx }
    }

    pub(super) fn validate_function_body(
        &self,
        block: &AstBlock,
        is_main: bool,
        params: &[String],
        param_types: &[Option<AstTypeAnnotation>],
    ) -> miette::Result<()> {
        super::validate_block(self.ctx, block, is_main, params, param_types)
    }

    pub(super) fn validate_scoped(
        &self,
        block: &AstBlock,
        outer_bindings: &HashMap<String, BindingKind>,
        params: &[String],
        param_types: &[Option<AstTypeAnnotation>],
        in_actor: bool,
    ) -> miette::Result<()> {
        super::validate_scoped_block(
            self.ctx,
            block,
            outer_bindings,
            params,
            param_types,
            in_actor,
        )
    }
}
