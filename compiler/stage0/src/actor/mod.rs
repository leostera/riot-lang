pub(crate) mod air;

use crate::lambda::ir::RirProgram;
use crate::signature::ImportedSignatures;

pub(crate) use air::*;

pub(crate) struct StacklessActorLowerer<'a> {
    imports: &'a ImportedSignatures,
}

impl<'a> StacklessActorLowerer<'a> {
    pub(crate) fn new(imports: &'a ImportedSignatures) -> Self {
        Self { imports }
    }

    pub(crate) fn lower(&self, lir: &RirProgram) -> ActorIrProgram {
        crate::ir::lower_rir_to_actor_ir(lir, self.imports)
    }
}
