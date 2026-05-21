use std::collections::HashMap;

use crate::ast::{AstMatchArm, AstPattern};
use crate::signature::RsigType;

use super::{BindingKind, ValidationContext};

pub(super) struct PatternValidator<'ctx, 'source> {
    ctx: &'ctx ValidationContext<'source>,
}

impl<'ctx, 'source> PatternValidator<'ctx, 'source> {
    pub(super) fn new(ctx: &'ctx ValidationContext<'source>) -> Self {
        Self { ctx }
    }

    pub(super) fn is_match_exhaustive(
        &self,
        arms: &[AstMatchArm],
        scrutinee_type: Option<&RsigType>,
    ) -> bool {
        super::match_is_exhaustive(self.ctx, arms, scrutinee_type)
    }

    pub(super) fn validate(
        &self,
        pattern: &AstPattern,
        scrutinee_type: Option<&RsigType>,
    ) -> miette::Result<()> {
        super::validate_pattern(self.ctx, pattern, scrutinee_type)
    }

    pub(super) fn bind(
        &self,
        pattern: &AstPattern,
        scrutinee_type: Option<RsigType>,
        bindings: &mut HashMap<String, BindingKind>,
    ) {
        super::bind_pattern(self.ctx, pattern, scrutinee_type, bindings);
    }

    pub(super) fn pattern_type(&self, pattern: &AstPattern) -> Option<RsigType> {
        super::pattern_type(self.ctx, pattern)
    }
}
