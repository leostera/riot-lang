use crate::{checker::tst::Type, parser::ident::Ident};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Allocation {
    StackCandidate,
    Heap,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueRepr {
    Immediate,
    Block(BlockLayout),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockLayout {
    pub tag: BlockTag,
    pub fields: Vec<FieldLayout>,
    pub allocation: Allocation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockTag {
    Tuple,
    Record { name: Ident },
    Constructor { name: Ident, tag: u32 },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldLayout {
    pub index: usize,
    pub ty: Type,
    pub repr: ValueRepr,
}

pub fn immediate() -> ValueRepr {
    ValueRepr::Immediate
}

pub fn tuple(fields: Vec<FieldLayout>) -> BlockLayout {
    BlockLayout {
        tag: BlockTag::Tuple,
        fields,
        allocation: Allocation::StackCandidate,
    }
}

pub fn record(name: Ident, fields: Vec<FieldLayout>) -> BlockLayout {
    BlockLayout {
        tag: BlockTag::Record { name },
        fields,
        allocation: Allocation::StackCandidate,
    }
}

pub fn constructor(name: Ident, tag: u32, fields: Vec<FieldLayout>) -> BlockLayout {
    BlockLayout {
        tag: BlockTag::Constructor { name, tag },
        fields,
        allocation: Allocation::StackCandidate,
    }
}

pub fn fields(types: impl IntoIterator<Item = Type>) -> Vec<FieldLayout> {
    types
        .into_iter()
        .enumerate()
        .map(|(index, ty)| FieldLayout {
            index,
            ty,
            repr: ValueRepr::Immediate,
        })
        .collect()
}
