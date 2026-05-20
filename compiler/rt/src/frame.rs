use std::alloc::{Layout, alloc_zeroed, dealloc};
use std::ptr;

pub(crate) type ActorDropFn = unsafe extern "C" fn(*mut u8);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct RtFrameLayout {
    pub(crate) size: usize,
    pub(crate) align: usize,
    pub(crate) drop_fn: Option<ActorDropFn>,
}

impl RtFrameLayout {
    pub(crate) fn new(size: usize, align: usize, drop_fn: Option<ActorDropFn>) -> Self {
        Self {
            size,
            align: align.max(1),
            drop_fn,
        }
    }

    pub(crate) fn legacy() -> Self {
        Self::new(0, 8, None)
    }
}

pub(crate) fn alloc_frame(size: usize, align: usize) -> *mut u8 {
    let Ok(layout) = Layout::from_size_align(size.max(1), align.max(1)) else {
        return ptr::null_mut();
    };
    unsafe { alloc_zeroed(layout) }
}

pub(crate) unsafe fn free_frame(frame: *mut u8, layout: RtFrameLayout) {
    if frame.is_null() || layout.size == 0 {
        return;
    }
    if let Some(drop_fn) = layout.drop_fn {
        unsafe { drop_fn(frame) };
    }
    let Ok(layout) = Layout::from_size_align(layout.size.max(1), layout.align.max(1)) else {
        return;
    };
    unsafe { dealloc(frame, layout) };
}
