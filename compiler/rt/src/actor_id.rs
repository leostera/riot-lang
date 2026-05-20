const ACTOR_ID_ALIGNMENT_MASK: u64 = 0x0f;

#[repr(transparent)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct ActorId(u64);

impl ActorId {
    pub(crate) const NULL: Self = Self(0);

    pub(crate) unsafe fn from_raw(value: u64) -> Self {
        Self(value)
    }

    pub(crate) fn from_ptr<T>(ptr: *const T) -> Self {
        Self(ptr as u64)
    }

    pub(crate) fn as_raw(self) -> u64 {
        self.0
    }

    pub(crate) fn is_null(self) -> bool {
        self.0 == 0
    }

    pub(crate) fn is_aligned(self) -> bool {
        self.0 & ACTOR_ID_ALIGNMENT_MASK == 0
    }
}
