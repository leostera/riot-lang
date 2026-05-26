use crate::ast::AstUseDecl;

use super::ValidationContext;

pub(super) struct ImportValidator<'ctx, 'source> {
    ctx: &'ctx ValidationContext<'source>,
}

impl<'ctx, 'source> ImportValidator<'ctx, 'source> {
    pub(super) fn new(ctx: &'ctx ValidationContext<'source>) -> Self {
        Self { ctx }
    }

    pub(super) fn validate_use(&self, use_: &AstUseDecl) -> miette::Result<()> {
        if self.ctx.imports.contains_key(use_.name.as_str()) {
            return Ok(());
        }

        Err(self
            .ctx
            .diagnostic(
                use_.name_span,
                "missing imported signature",
                format!("`use {}` did not resolve to a signature", use_.name),
                Some("pass --sig-dir with the directory containing the .rsig file"),
            )
            .into())
    }
}
