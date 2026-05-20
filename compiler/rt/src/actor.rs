use std::collections::VecDeque;
use std::mem::size_of;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Mutex, MutexGuard};

use crate::actor_id::ActorId;
use crate::frame::{RtFrameLayout, free_frame};
use crate::value::{RtValue, VALUE_NULL, value_actor_id, value_bool, value_i64};

pub(crate) const MSG_BYTES: u32 = 0;
pub(crate) const MSG_I64: u32 = 1;
pub(crate) const MSG_BOOL: u32 = 2;
pub(crate) const MSG_ACTOR_ID: u32 = 3;
pub(crate) const MSG_VALUE: u32 = 4;
pub(crate) const MSG_UNIT: u32 = 5;

pub(crate) const POLL_CONSUMED: u32 = 1;
pub(crate) const POLL_DONE: u32 = 2;
pub(crate) const POLL_PROGRESS: u32 = 4;
pub(crate) const POLL_YIELD: u32 = 8;
pub(crate) const POLL_WAITING: u32 = 16;

pub(crate) type ActorResumeFn = unsafe extern "C" fn(*mut u8, ActorId, *const RtMessage) -> u32;

#[repr(C)]
pub struct RtMessage {
    kind: u32,
    i64_value: i64,
    bool_value: u8,
    value: RtValue,
    ptr: *const u8,
    len: usize,
}

#[derive(Clone)]
pub(crate) enum RuntimeMessage {
    Unit,
    Bytes(Vec<u8>),
    I64(i64),
    Bool(bool),
    ActorId(ActorId),
    Value(RtValue),
}

impl RuntimeMessage {
    pub(crate) fn as_rt_message(&self) -> RtMessage {
        match self {
            RuntimeMessage::Unit => RtMessage {
                kind: MSG_UNIT,
                i64_value: 0,
                bool_value: 0,
                value: crate::value::VALUE_UNIT,
                ptr: ptr::null(),
                len: 0,
            },
            RuntimeMessage::Bytes(bytes) => RtMessage {
                kind: MSG_BYTES,
                i64_value: 0,
                bool_value: 0,
                value: VALUE_NULL,
                ptr: bytes.as_ptr(),
                len: bytes.len(),
            },
            RuntimeMessage::I64(value) => RtMessage {
                kind: MSG_I64,
                i64_value: *value,
                bool_value: 0,
                value: value_i64(*value),
                ptr: ptr::null(),
                len: 0,
            },
            RuntimeMessage::Bool(value) => RtMessage {
                kind: MSG_BOOL,
                i64_value: 0,
                bool_value: u8::from(*value),
                value: value_bool(*value),
                ptr: ptr::null(),
                len: 0,
            },
            RuntimeMessage::ActorId(value) => RtMessage {
                kind: MSG_ACTOR_ID,
                i64_value: value.as_raw() as i64,
                bool_value: 0,
                value: value_actor_id(*value),
                ptr: ptr::null(),
                len: 0,
            },
            RuntimeMessage::Value(value) => RtMessage {
                kind: MSG_VALUE,
                i64_value: 0,
                bool_value: 0,
                value: *value,
                ptr: ptr::null(),
                len: 0,
            },
        }
    }

    pub(crate) fn root_value(&self) -> Option<RtValue> {
        match self {
            RuntimeMessage::Value(value) => Some(*value),
            _ => None,
        }
    }
}

pub(crate) struct Mailbox {
    messages: Mutex<VecDeque<RuntimeMessage>>,
    cursor: AtomicUsize,
}

impl Mailbox {
    fn new() -> Self {
        Self {
            messages: Mutex::new(VecDeque::new()),
            cursor: AtomicUsize::new(0),
        }
    }

    pub(crate) fn push(&self, message: RuntimeMessage) {
        self.messages().push_back(message);
    }

    pub(crate) fn front(&self) -> Option<RuntimeMessage> {
        self.messages().front().cloned()
    }

    pub(crate) fn scan_candidates(&self, limit: usize) -> Vec<(usize, RuntimeMessage)> {
        let messages = self.messages();
        let len = messages.len();
        if len == 0 {
            self.cursor.store(0, Ordering::Release);
            return Vec::new();
        }
        let limit = limit.max(1).min(len);
        let start = self.cursor.load(Ordering::Acquire).min(len - 1);
        (0..limit)
            .filter_map(|offset| {
                let index = (start + offset) % len;
                messages.get(index).cloned().map(|message| (index, message))
            })
            .collect()
    }

    pub(crate) fn advance_cursor(&self, count: usize) -> bool {
        let messages = self.messages();
        let len = messages.len();
        if len == 0 {
            self.cursor.store(0, Ordering::Release);
            return false;
        }
        let start = self.cursor.load(Ordering::Acquire).min(len - 1);
        let next = (start + count) % len;
        self.cursor.store(next, Ordering::Release);
        next != 0
    }

    pub(crate) fn reset_cursor(&self) {
        self.cursor.store(0, Ordering::Release);
    }

    pub(crate) fn remove(&self, index: usize) {
        self.messages().remove(index);
        self.reset_cursor();
    }

    pub(crate) fn clear(&self) {
        self.messages().clear();
        self.reset_cursor();
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.messages().is_empty()
    }

    pub(crate) fn root_values(&self) -> Vec<RtValue> {
        self.messages()
            .iter()
            .filter_map(RuntimeMessage::root_value)
            .collect()
    }

    fn messages(&self) -> MutexGuard<'_, VecDeque<RuntimeMessage>> {
        self.messages
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}

#[repr(align(16))]
pub(crate) struct ActorSlot {
    actor_id: ActorId,
    display_id: u64,
    scheduler_id: u32,
    frame: AtomicUsize,
    layout: RtFrameLayout,
    resume: ActorResumeFn,
    mailbox: Mailbox,
    frame_root_offsets: Mutex<Vec<usize>>,
    monitors: Mutex<Vec<ActorId>>,
    links: Mutex<Vec<ActorId>>,
    terminated: AtomicBool,
}

impl ActorSlot {
    fn new(
        scheduler_id: u32,
        display_id: u64,
        frame: *mut u8,
        layout: RtFrameLayout,
        resume: ActorResumeFn,
    ) -> Self {
        Self {
            actor_id: ActorId::NULL,
            display_id,
            scheduler_id,
            frame: AtomicUsize::new(frame as usize),
            layout,
            resume,
            mailbox: Mailbox::new(),
            frame_root_offsets: Mutex::new(Vec::new()),
            monitors: Mutex::new(Vec::new()),
            links: Mutex::new(Vec::new()),
            terminated: AtomicBool::new(false),
        }
    }

    pub(crate) fn boxed(
        scheduler_id: u32,
        display_id: u64,
        frame: *mut u8,
        layout: RtFrameLayout,
        resume: ActorResumeFn,
    ) -> Box<Self> {
        let mut actor = Box::new(Self::new(scheduler_id, display_id, frame, layout, resume));
        actor.actor_id = ActorId::from_ptr(actor.as_ref());
        actor
    }

    pub(crate) fn actor_id(&self) -> ActorId {
        self.actor_id
    }

    pub(crate) fn display_id(&self) -> u64 {
        self.display_id
    }

    pub(crate) fn scheduler_id(&self) -> u32 {
        self.scheduler_id
    }

    pub(crate) fn frame_ptr(&self) -> *mut u8 {
        self.frame.load(Ordering::Acquire) as *mut u8
    }

    pub(crate) fn resume(&self) -> ActorResumeFn {
        self.resume
    }

    pub(crate) fn is_terminated(&self) -> bool {
        self.terminated.load(Ordering::Acquire)
    }

    pub(crate) fn send(&self, message: RuntimeMessage) {
        if !self.is_terminated() {
            self.mailbox.push(message);
        }
    }

    pub(crate) fn current_message(&self) -> Option<RuntimeMessage> {
        self.mailbox.front()
    }

    pub(crate) fn receive_candidates(&self, limit: usize) -> Vec<(usize, RuntimeMessage)> {
        self.mailbox.scan_candidates(limit)
    }

    pub(crate) fn advance_receive_cursor(&self, count: usize) -> bool {
        self.mailbox.advance_cursor(count)
    }

    pub(crate) fn consume_message(&self, index: usize) {
        if !self.mailbox.is_empty() {
            self.mailbox.remove(index);
        }
    }

    pub(crate) fn root_values(&self) -> Vec<RtValue> {
        let mut roots = self.mailbox.root_values();
        roots.extend(self.frame_root_values());
        roots
    }

    pub(crate) fn set_frame_root_offsets(&self, offsets: &[usize]) {
        *self.frame_root_offsets() = offsets.to_vec();
    }

    pub(crate) fn push_monitor(&self, watcher: ActorId) {
        self.monitors().push(watcher);
    }

    pub(crate) fn push_link(&self, peer: ActorId) {
        self.links().push(peer);
    }

    pub(crate) fn monitor_ids(&self) -> Vec<ActorId> {
        self.monitors().clone()
    }

    pub(crate) fn link_ids(&self) -> Vec<ActorId> {
        self.links().clone()
    }

    pub(crate) fn terminate(&self) -> bool {
        if self.terminated.swap(true, Ordering::AcqRel) {
            return false;
        }
        let frame = self.frame.swap(0, Ordering::AcqRel) as *mut u8;
        unsafe { free_frame(frame, self.layout) };
        self.mailbox.clear();
        true
    }

    fn monitors(&self) -> MutexGuard<'_, Vec<ActorId>> {
        self.monitors
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn links(&self) -> MutexGuard<'_, Vec<ActorId>> {
        self.links
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn frame_root_offsets(&self) -> MutexGuard<'_, Vec<usize>> {
        self.frame_root_offsets
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn frame_root_values(&self) -> Vec<RtValue> {
        let frame = self.frame_ptr();
        if frame.is_null() {
            return Vec::new();
        }

        self.frame_root_offsets()
            .iter()
            .filter_map(|offset| {
                if offset.saturating_add(size_of::<RtValue>()) > self.layout.size {
                    return None;
                }
                Some(unsafe { ptr::read_unaligned(frame.add(*offset) as *const RtValue) })
            })
            .collect()
    }
}

pub(crate) unsafe fn actor_from_id(actor_id: ActorId) -> Option<&'static ActorSlot> {
    if actor_id.is_null() || !actor_id.is_aligned() {
        return None;
    }
    let actor = actor_id.as_raw() as *const ActorSlot;
    Some(unsafe { &*actor })
}

pub(crate) unsafe fn runtime_message_from_raw(message: *const RtMessage) -> Option<RuntimeMessage> {
    let message = unsafe { message.as_ref() }?;
    match message.kind {
        MSG_BYTES => unsafe { crate::io::bytes_from_raw(message.ptr, message.len) }
            .map(|bytes| RuntimeMessage::Bytes(bytes.to_vec())),
        MSG_I64 => Some(RuntimeMessage::I64(message.i64_value)),
        MSG_BOOL => Some(RuntimeMessage::Bool(message.bool_value != 0)),
        MSG_ACTOR_ID => Some(RuntimeMessage::ActorId(unsafe {
            ActorId::from_raw(message.i64_value as u64)
        })),
        MSG_VALUE => Some(RuntimeMessage::Value(message.value)),
        MSG_UNIT => Some(RuntimeMessage::Unit),
        _ => None,
    }
}
