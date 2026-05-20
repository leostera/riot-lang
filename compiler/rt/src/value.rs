use crate::actor_id::ActorId;

pub type RtValue = u64;

pub(crate) const VALUE_NULL: RtValue = 0;
pub(crate) const VALUE_TAG_MASK: RtValue = 0x0f;
pub(crate) const VALUE_HEAP_TAG: RtValue = 0x00;
pub(crate) const VALUE_I64_TAG: RtValue = 0x01;
pub(crate) const VALUE_BOOL_FALSE: RtValue = 0x02;
pub(crate) const VALUE_BOOL_TRUE: RtValue = 0x03;
pub(crate) const VALUE_UNIT: RtValue = 0x04;
pub(crate) const VALUE_ACTOR_ID_TAG: RtValue = 0x05;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum HeapOwner {
    Local(ActorId),
    Shared,
}

pub(crate) enum HeapObjectKind {
    String(Vec<u8>),
    Tuple(Vec<RtValue>),
    List(Vec<RtValue>),
    Record {
        path: String,
        fields: Vec<(String, RtValue)>,
    },
}

pub(crate) struct HeapObject {
    pub(crate) marked: bool,
    pub(crate) owner: HeapOwner,
    pub(crate) kind: HeapObjectKind,
}

impl HeapObject {
    pub(crate) fn owned_children(&self) -> Vec<RtValue> {
        match &self.kind {
            HeapObjectKind::Tuple(items) | HeapObjectKind::List(items) => items.clone(),
            HeapObjectKind::Record { fields, .. } => {
                fields.iter().map(|(_, value)| *value).collect()
            }
            HeapObjectKind::String(_) => Vec::new(),
        }
    }
}

pub(crate) fn value_tag(value: RtValue) -> RtValue {
    value & VALUE_TAG_MASK
}

pub(crate) fn value_i64(value: i64) -> RtValue {
    ((value as RtValue) << 4) | VALUE_I64_TAG
}

pub(crate) fn value_i64_payload(value: RtValue) -> Option<i64> {
    (value_tag(value) == VALUE_I64_TAG).then_some((value as i64) >> 4)
}

pub(crate) fn value_bool(value: bool) -> RtValue {
    if value {
        VALUE_BOOL_TRUE
    } else {
        VALUE_BOOL_FALSE
    }
}

pub(crate) fn value_actor_id(value: ActorId) -> RtValue {
    if value.is_null() || !value.is_aligned() {
        crate::scheduler::runtime_abort("actor id is not pointer-aligned");
    }
    value.as_raw() | VALUE_ACTOR_ID_TAG
}

pub(crate) fn value_actor_id_payload(value: RtValue) -> Option<ActorId> {
    (value_tag(value) == VALUE_ACTOR_ID_TAG)
        .then(|| unsafe { ActorId::from_raw(value & !VALUE_TAG_MASK) })
}

pub(crate) fn value_heap(index: usize) -> RtValue {
    (((index as RtValue) + 1) << 4) | VALUE_HEAP_TAG
}

pub(crate) fn heap_index(value: RtValue) -> Option<usize> {
    if value == VALUE_NULL || value_tag(value) != VALUE_HEAP_TAG {
        return None;
    }
    usize::try_from((value >> 4).checked_sub(1)?).ok()
}
