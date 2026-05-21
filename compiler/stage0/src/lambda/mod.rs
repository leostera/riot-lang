pub(crate) mod ir;

use crate::checker::tyir::TypedProgram;

pub(crate) use ir::*;

pub(crate) struct LambdaSimplifier;

impl LambdaSimplifier {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn simplify(&self, tyir: TypedProgram) -> RirProgram {
        crate::ir::lower_typed_to_rir(tyir)
    }
}
