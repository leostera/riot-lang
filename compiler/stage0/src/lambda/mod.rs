pub(crate) mod ir;

use crate::checker::tyir::TypedProgram;
use crate::ir::RirLowerer;

pub(crate) use ir::*;

pub(crate) struct LambdaSimplifier;

impl LambdaSimplifier {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn simplify(&self, tyir: TypedProgram) -> RirProgram {
        RirLowerer::new().lower(tyir)
    }
}
