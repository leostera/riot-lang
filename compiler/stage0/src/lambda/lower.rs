use crate::checker::tyir::TypedProgram;

use super::ir::RirProgram;

#[derive(Debug, Default)]
pub(crate) struct RirLowerer;

impl RirLowerer {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn lower(&self, program: TypedProgram) -> RirProgram {
        crate::ir::lower_typed_to_rir(program)
    }
}

pub(crate) struct LambdaSimplifier;

impl LambdaSimplifier {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn simplify(&self, tyir: TypedProgram) -> RirProgram {
        RirLowerer::new().lower(tyir)
    }
}
