use crate::lambda::ir::RirProgram;
use crate::signature::ImportedSignatures;

use super::air::ActorIrProgram;

pub(crate) struct ActorIrLowerer<'a> {
    imports: &'a ImportedSignatures,
}

impl<'a> ActorIrLowerer<'a> {
    pub(crate) fn new(imports: &'a ImportedSignatures) -> Self {
        Self { imports }
    }

    pub(crate) fn lower(&self, program: &RirProgram) -> ActorIrProgram {
        crate::ir::lower_rir_to_actor_ir(program, self.imports)
    }
}

pub(crate) struct StacklessActorLowerer<'a> {
    imports: &'a ImportedSignatures,
}

impl<'a> StacklessActorLowerer<'a> {
    pub(crate) fn new(imports: &'a ImportedSignatures) -> Self {
        Self { imports }
    }

    pub(crate) fn lower(&self, lir: &RirProgram) -> ActorIrProgram {
        ActorIrLowerer::new(self.imports).lower(lir)
    }
}
