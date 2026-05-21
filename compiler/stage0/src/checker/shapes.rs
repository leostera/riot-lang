use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::ast::AstProgram;
use crate::signature::{ImportedSignatures, TypeName};

use super::{ConstructorShape, RecordShape};

pub(super) struct CollectedTypeShapes {
    pub(super) declared_variants: BTreeSet<TypeName>,
    pub(super) constructor_types: HashMap<String, ConstructorShape>,
    pub(super) record_shapes: HashMap<String, RecordShape>,
    pub(super) record_shapes_by_type: BTreeMap<TypeName, RecordShape>,
}

pub(super) struct TypeShapeCollector<'a> {
    imports: &'a ImportedSignatures,
}

impl<'a> TypeShapeCollector<'a> {
    pub(super) fn new(imports: &'a ImportedSignatures) -> Self {
        Self { imports }
    }

    pub(super) fn collect(&self, program: &AstProgram) -> CollectedTypeShapes {
        let declared_variants = super::declared_variant_names(program, self.imports);
        let constructor_types = super::constructor_types(program, &declared_variants);
        let (record_shapes, record_shapes_by_type) =
            super::record_shapes(program, self.imports, &declared_variants);
        CollectedTypeShapes {
            declared_variants,
            constructor_types,
            record_shapes,
            record_shapes_by_type,
        }
    }
}
