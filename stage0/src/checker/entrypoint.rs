use crate::ast::{AstFnDecl, AstProgram, TextSpan};

use super::{CheckMode, ValidationContext, first_decl_span};

pub(super) struct EntrypointValidator<'ctx, 'source> {
    ctx: &'ctx ValidationContext<'source>,
    mode: CheckMode,
}

impl<'ctx, 'source> EntrypointValidator<'ctx, 'source> {
    pub(super) fn new(ctx: &'ctx ValidationContext<'source>, mode: CheckMode) -> Self {
        Self { ctx, mode }
    }

    pub(super) fn validate_program(
        &self,
        program: &AstProgram,
        main_decls: &[&AstFnDecl],
    ) -> miette::Result<()> {
        if main_decls.len() > 1 {
            let duplicate = main_decls[1];
            return Err(self
                .ctx
                .diagnostic(
                    duplicate.span,
                    "duplicate main function",
                    "stage0 requires a single main function per file",
                    Some("keep only one `fn main() { ... }`"),
                )
                .into());
        }

        if self.mode == CheckMode::Executable && main_decls.is_empty() {
            let span = first_decl_span(program)
                .unwrap_or_else(|| TextSpan::new(0, self.ctx.source.len().min(1)));
            return Err(self
                .ctx
                .diagnostic(
                    span,
                    "missing main function",
                    "stage0 compile requires one entrypoint named main",
                    Some("add: fn main() { dbg(\"hello world\") }"),
                )
                .into());
        }

        Ok(())
    }

    pub(super) fn validate_function(&self, function: &AstFnDecl) -> miette::Result<()> {
        super::validate_main_function_signature(self.ctx, function)
    }

    pub(super) fn requires_output_action(&self, function: &AstFnDecl) -> bool {
        super::main_requires_output_action(self.ctx, function)
    }
}
