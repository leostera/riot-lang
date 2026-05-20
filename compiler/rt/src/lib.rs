#![deny(unsafe_op_in_unsafe_fn)]

use std::alloc::{Layout, alloc_zeroed, dealloc};
use std::cell::Cell;
use std::collections::VecDeque;
use std::io::{self, Write};
use std::ptr;
use std::slice;
use std::sync::{Mutex, OnceLock};

const POLL_CONSUMED: u32 = 1;
const POLL_DONE: u32 = 2;
const POLL_PROGRESS: u32 = 4;
const POLL_YIELD: u32 = 8;
const POLL_WAITING: u32 = 16;

const MSG_BYTES: u32 = 0;
const MSG_I64: u32 = 1;
const MSG_BOOL: u32 = 2;
const MSG_PID: u32 = 3;
const MSG_VALUE: u32 = 4;

pub type RtValue = u64;

const VALUE_NULL: RtValue = 0;
const VALUE_TAG_MASK: RtValue = 0x0f;
const VALUE_HEAP_TAG: RtValue = 0x00;
const VALUE_I64_TAG: RtValue = 0x01;
const VALUE_BOOL_FALSE: RtValue = 0x02;
const VALUE_BOOL_TRUE: RtValue = 0x03;
const VALUE_UNIT: RtValue = 0x04;
const VALUE_PID_TAG: RtValue = 0x05;

static RUNTIME: OnceLock<Mutex<Runtime>> = OnceLock::new();

thread_local! {
    static ACTIVE_RUNTIME: Cell<*mut Runtime> = const { Cell::new(ptr::null_mut()) };
    static ACTIVE_PID: Cell<u64> = const { Cell::new(0) };
}

type ActorResumeFn = unsafe extern "C" fn(*mut u8, u64, *const RtMessage) -> u32;
type ActorDropFn = unsafe extern "C" fn(*mut u8);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct RtFrameLayout {
    size: usize,
    align: usize,
    drop_fn: Option<ActorDropFn>,
}

impl RtFrameLayout {
    fn new(size: usize, align: usize, drop_fn: Option<ActorDropFn>) -> Self {
        Self {
            size,
            align: align.max(1),
            drop_fn,
        }
    }

    fn legacy() -> Self {
        Self::new(0, 8, None)
    }
}

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
enum RuntimeMessage {
    Bytes(Vec<u8>),
    I64(i64),
    Bool(bool),
    Pid(u64),
    Value(RtValue),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum HeapOwner {
    Local(u64),
    Shared,
}

enum HeapObjectKind {
    String(Vec<u8>),
    Tuple(Vec<RtValue>),
    List(Vec<RtValue>),
    Record {
        path: String,
        fields: Vec<(String, RtValue)>,
    },
}

struct HeapObject {
    marked: bool,
    owner: HeapOwner,
    kind: HeapObjectKind,
}

impl RuntimeMessage {
    fn as_rt_message(&self) -> RtMessage {
        match self {
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
            RuntimeMessage::Pid(value) => RtMessage {
                kind: MSG_PID,
                i64_value: *value as i64,
                bool_value: 0,
                value: value_pid(*value),
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

    fn root_value(&self) -> Option<RtValue> {
        match self {
            RuntimeMessage::Value(value) => Some(*value),
            _ => None,
        }
    }
}

impl HeapObject {
    fn owned_children(&self) -> Vec<RtValue> {
        match &self.kind {
            HeapObjectKind::Tuple(items) | HeapObjectKind::List(items) => items.clone(),
            HeapObjectKind::Record { fields, .. } => {
                fields.iter().map(|(_, value)| *value).collect()
            }
            HeapObjectKind::String(_) => Vec::new(),
        }
    }
}

#[derive(Default)]
struct Runtime {
    actors: Vec<ActorSlot>,
    heap: Vec<Option<HeapObject>>,
    roots: Vec<RtValue>,
}

struct ActorSlot {
    frame: usize,
    layout: RtFrameLayout,
    resume: ActorResumeFn,
    mailbox: VecDeque<RuntimeMessage>,
    monitors: Vec<u64>,
    links: Vec<u64>,
    terminated: bool,
}

impl Runtime {
    fn reset(&mut self) {
        self.shutdown();
        self.heap.clear();
        self.roots.clear();
    }

    fn spawn(&mut self, frame: *mut u8, resume: ActorResumeFn, layout: RtFrameLayout) -> u64 {
        if frame.is_null() {
            return 0;
        }

        self.actors.push(ActorSlot {
            frame: frame as usize,
            layout,
            resume,
            mailbox: VecDeque::new(),
            monitors: Vec::new(),
            links: Vec::new(),
            terminated: false,
        });
        self.actors.len() as u64
    }

    fn send(&mut self, pid: u64, message: RuntimeMessage) {
        let Some(index) = pid_index(pid) else {
            return;
        };
        let Some(actor) = self.actors.get_mut(index) else {
            return;
        };
        if !actor.terminated {
            actor.mailbox.push_back(message);
        }
    }

    fn monitor(&mut self, pid: u64) {
        let Some(index) = pid_index(pid) else {
            return;
        };
        if let Some(actor) = self.actors.get_mut(index) {
            actor.monitors.push(0);
        }
    }

    fn link(&mut self, pid: u64) {
        let Some(index) = pid_index(pid) else {
            return;
        };
        if let Some(actor) = self.actors.get_mut(index) {
            actor.links.push(0);
        }
    }

    fn shutdown(&mut self) {
        self.schedule_until_quiescent();

        for index in 0..self.actors.len() {
            self.terminate_actor(index);
        }

        for (index, actor) in self.actors.iter().enumerate() {
            let _linked = actor.links.len();
            let pid = index + 1;
            for _ in &actor.monitors {
                write_bytes_line(format!("down {pid}").as_bytes());
            }
        }

        self.actors.clear();
        self.roots.clear();
        self.collect_garbage();
    }

    fn alloc_value(&mut self, kind: HeapObjectKind, owner: HeapOwner) -> RtValue {
        let object = HeapObject {
            marked: false,
            owner,
            kind,
        };
        if let Some((index, slot)) = self
            .heap
            .iter_mut()
            .enumerate()
            .find(|(_, slot)| slot.is_none())
        {
            *slot = Some(object);
            return value_heap(index);
        }
        self.heap.push(Some(object));
        value_heap(self.heap.len() - 1)
    }

    fn push_root(&mut self, value: RtValue) {
        self.roots.push(value);
    }

    fn pop_roots(&mut self, count: usize) {
        let keep = self.roots.len().saturating_sub(count);
        self.roots.truncate(keep);
    }

    fn promote_shared(&mut self, value: RtValue) {
        let Some(index) = heap_index(value) else {
            return;
        };
        let children = {
            let Some(object) = self.heap.get_mut(index).and_then(Option::as_mut) else {
                return;
            };
            if object.owner == HeapOwner::Shared {
                return;
            }
            object.owner = HeapOwner::Shared;
            object.owned_children()
        };
        for child in children {
            self.promote_shared(child);
        }
    }

    fn collect_garbage(&mut self) -> usize {
        for object in self.heap.iter_mut().flatten() {
            object.marked = false;
        }

        let roots = self.roots.clone();
        for root in roots {
            self.mark_value(root);
        }

        let mailbox_roots = self
            .actors
            .iter()
            .flat_map(|actor| actor.mailbox.iter())
            .filter_map(RuntimeMessage::root_value)
            .collect::<Vec<_>>();
        for root in mailbox_roots {
            self.mark_value(root);
        }

        let mut freed = 0;
        for slot in &mut self.heap {
            if slot.as_ref().is_some_and(|object| !object.marked) {
                *slot = None;
                freed += 1;
            }
        }
        freed
    }

    fn mark_value(&mut self, value: RtValue) {
        let Some(index) = heap_index(value) else {
            return;
        };
        let Some(object) = self.heap.get_mut(index).and_then(Option::as_mut) else {
            return;
        };
        if object.marked {
            return;
        }
        object.marked = true;
        let children = object.owned_children();
        for child in children {
            self.mark_value(child);
        }
    }

    fn render_value(&self, value: RtValue) -> String {
        match value_tag(value) {
            VALUE_I64_TAG => value_i64_payload(value)
                .map(|value| value.to_string())
                .unwrap_or_else(|| "<invalid-int>".to_owned()),
            VALUE_BOOL_FALSE => "false".to_owned(),
            VALUE_BOOL_TRUE => "true".to_owned(),
            VALUE_UNIT => "()".to_owned(),
            VALUE_PID_TAG => value_pid_payload(value)
                .map(|value| value.to_string())
                .unwrap_or_else(|| "<invalid-pid>".to_owned()),
            VALUE_HEAP_TAG => self.render_heap_value(value),
            _ => "<invalid-value>".to_owned(),
        }
    }

    fn render_heap_value(&self, value: RtValue) -> String {
        let Some(index) = heap_index(value) else {
            return "<null>".to_owned();
        };
        let Some(object) = self.heap.get(index).and_then(Option::as_ref) else {
            return "<freed>".to_owned();
        };
        match &object.kind {
            HeapObjectKind::String(bytes) => String::from_utf8_lossy(bytes).into_owned(),
            HeapObjectKind::Tuple(items) => {
                let rendered = items
                    .iter()
                    .map(|item| self.render_value(*item))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({rendered})")
            }
            HeapObjectKind::List(items) => {
                let rendered = items
                    .iter()
                    .map(|item| self.render_value(*item))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{rendered}]")
            }
            HeapObjectKind::Record { path, fields } => {
                let rendered = fields
                    .iter()
                    .map(|(name, value)| format!("{name}: {}", self.render_value(*value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{path} {{ {rendered} }}")
            }
        }
    }

    fn value_bytes(&self, value: RtValue) -> Option<&[u8]> {
        let index = heap_index(value)?;
        let object = self.heap.get(index)?.as_ref()?;
        match &object.kind {
            HeapObjectKind::String(bytes) => Some(bytes.as_slice()),
            _ => None,
        }
    }

    fn values_equal(&self, lhs: RtValue, rhs: RtValue) -> bool {
        match (value_tag(lhs), value_tag(rhs)) {
            (VALUE_I64_TAG, VALUE_I64_TAG) => value_i64_payload(lhs) == value_i64_payload(rhs),
            (VALUE_BOOL_FALSE, VALUE_BOOL_FALSE) | (VALUE_BOOL_TRUE, VALUE_BOOL_TRUE) => true,
            (VALUE_UNIT, VALUE_UNIT) => true,
            (VALUE_PID_TAG, VALUE_PID_TAG) => value_pid_payload(lhs) == value_pid_payload(rhs),
            (VALUE_HEAP_TAG, VALUE_HEAP_TAG) => self.heap_values_equal(lhs, rhs),
            _ => false,
        }
    }

    fn heap_values_equal(&self, lhs: RtValue, rhs: RtValue) -> bool {
        let Some((lhs_object, rhs_object)) = self.heap_pair(lhs, rhs) else {
            return false;
        };

        match (&lhs_object.kind, &rhs_object.kind) {
            (HeapObjectKind::String(lhs), HeapObjectKind::String(rhs)) => lhs == rhs,
            (HeapObjectKind::Tuple(lhs), HeapObjectKind::Tuple(rhs))
            | (HeapObjectKind::List(lhs), HeapObjectKind::List(rhs)) => {
                lhs.len() == rhs.len()
                    && lhs
                        .iter()
                        .zip(rhs.iter())
                        .all(|(lhs, rhs)| self.values_equal(*lhs, *rhs))
            }
            (
                HeapObjectKind::Record {
                    path: lhs_path,
                    fields: lhs_fields,
                },
                HeapObjectKind::Record {
                    path: rhs_path,
                    fields: rhs_fields,
                },
            ) => {
                lhs_path == rhs_path
                    && lhs_fields.len() == rhs_fields.len()
                    && lhs_fields.iter().zip(rhs_fields.iter()).all(
                        |((lhs_name, lhs_value), (rhs_name, rhs_value))| {
                            lhs_name == rhs_name && self.values_equal(*lhs_value, *rhs_value)
                        },
                    )
            }
            _ => false,
        }
    }

    fn values_less_than(&self, lhs: RtValue, rhs: RtValue) -> bool {
        match (value_i64_payload(lhs), value_i64_payload(rhs)) {
            (Some(lhs), Some(rhs)) => return lhs < rhs,
            (Some(_), None) | (None, Some(_)) => return false,
            (None, None) => {}
        }

        let Some((lhs_object, rhs_object)) = self.heap_pair(lhs, rhs) else {
            return false;
        };
        match (&lhs_object.kind, &rhs_object.kind) {
            (HeapObjectKind::String(lhs), HeapObjectKind::String(rhs)) => lhs < rhs,
            _ => false,
        }
    }

    fn heap_pair(&self, lhs: RtValue, rhs: RtValue) -> Option<(&HeapObject, &HeapObject)> {
        let lhs_index = heap_index(lhs)?;
        let rhs_index = heap_index(rhs)?;
        let lhs_object = self.heap.get(lhs_index)?.as_ref()?;
        let rhs_object = self.heap.get(rhs_index)?.as_ref()?;
        Some((lhs_object, rhs_object))
    }

    fn schedule_until_quiescent(&mut self) {
        loop {
            let mut made_progress = false;
            let actor_count = self.actors.len();

            for index in 0..actor_count {
                if self.actors.get(index).is_none_or(|actor| actor.terminated) {
                    continue;
                }

                let pid = (index + 1) as u64;
                let (frame, resume, current_message) = {
                    let actor = &self.actors[index];
                    (
                        actor.frame as *mut u8,
                        actor.resume,
                        actor.mailbox.front().cloned(),
                    )
                };
                let message = current_message.as_ref().map(RuntimeMessage::as_rt_message);
                let message_ptr = message
                    .as_ref()
                    .map_or(ptr::null(), |message| message as *const RtMessage);

                let previous = ACTIVE_RUNTIME.with(|active| {
                    let previous = active.replace(self as *mut Runtime);
                    let previous_pid = ACTIVE_PID.with(|active_pid| active_pid.replace(pid));
                    let result = unsafe { resume(frame, pid, message_ptr) };
                    ACTIVE_PID.with(|active_pid| active_pid.set(previous_pid));
                    active.set(previous);
                    (previous, result)
                });
                let (_previous_runtime, result) = previous;

                made_progress |= result & (POLL_PROGRESS | POLL_YIELD) != 0;

                let mut terminate = false;
                if let Some(actor) = self.actors.get_mut(index) {
                    if result & POLL_CONSUMED != 0 && !actor.mailbox.is_empty() {
                        actor.mailbox.pop_front();
                    }
                    if result & POLL_DONE != 0 {
                        terminate = true;
                    } else if result & POLL_WAITING != 0 {
                        continue;
                    }
                }
                if terminate {
                    self.terminate_actor(index);
                    made_progress = true;
                }
            }

            if !made_progress {
                break;
            }
        }
    }

    fn terminate_actor(&mut self, index: usize) {
        let Some(actor) = self.actors.get_mut(index) else {
            return;
        };
        if actor.terminated {
            return;
        }
        actor.terminated = true;
        let frame = actor.frame as *mut u8;
        actor.frame = 0;
        unsafe { free_frame(frame, actor.layout) };
    }
}
#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_init() {
    with_runtime_mut(Runtime::reset);
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_shutdown() {
    with_runtime_mut(|runtime| runtime.shutdown());
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_println(ptr: *const u8, len: usize) {
    unsafe { write_stdout_line(ptr, len) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_prim_println(ptr: *const u8, len: usize) {
    unsafe { riot_rt_println(ptr, len) };
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_println_i64(value: i64) {
    write_bytes_line(value.to_string().as_bytes());
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_dbg(ptr: *const u8, len: usize) {
    unsafe { write_stdout_line(ptr, len) };
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_dbg_i64(value: i64) {
    write_bytes_line(value.to_string().as_bytes());
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_unit() -> RtValue {
    VALUE_UNIT
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_i64(value: i64) -> RtValue {
    value_i64(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_bool(value: bool) -> RtValue {
    value_bool(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_pid(value: u64) -> RtValue {
    value_pid(value)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_string(ptr: *const u8, len: usize) -> RtValue {
    let Some(bytes) = (unsafe { bytes_from_raw(ptr, len) }) else {
        return VALUE_NULL;
    };
    with_runtime_mut(|runtime| {
        runtime.alloc_value(
            HeapObjectKind::String(bytes.to_vec()),
            HeapOwner::Local(current_actor_pid()),
        )
    })
    .unwrap_or(VALUE_NULL)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_tuple(values: *const RtValue, len: usize) -> RtValue {
    let Some(values) = (unsafe { values_from_raw(values, len) }) else {
        return VALUE_NULL;
    };
    with_runtime_mut(|runtime| {
        runtime.alloc_value(
            HeapObjectKind::Tuple(values.to_vec()),
            HeapOwner::Local(current_actor_pid()),
        )
    })
    .unwrap_or(VALUE_NULL)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_list(values: *const RtValue, len: usize) -> RtValue {
    let Some(values) = (unsafe { values_from_raw(values, len) }) else {
        return VALUE_NULL;
    };
    with_runtime_mut(|runtime| {
        runtime.alloc_value(
            HeapObjectKind::List(values.to_vec()),
            HeapOwner::Local(current_actor_pid()),
        )
    })
    .unwrap_or(VALUE_NULL)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_record_begin(
    path_ptr: *const u8,
    path_len: usize,
    field_count: usize,
) -> RtValue {
    let Some(path) = (unsafe { bytes_from_raw(path_ptr, path_len) }) else {
        return VALUE_NULL;
    };
    let path = String::from_utf8_lossy(path).into_owned();
    with_runtime_mut(|runtime| {
        runtime.alloc_value(
            HeapObjectKind::Record {
                path,
                fields: vec![("".to_owned(), VALUE_UNIT); field_count],
            },
            HeapOwner::Local(current_actor_pid()),
        )
    })
    .unwrap_or(VALUE_NULL)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_record_set(
    record: RtValue,
    index: usize,
    name_ptr: *const u8,
    name_len: usize,
    value: RtValue,
) {
    let Some(name) = (unsafe { bytes_from_raw(name_ptr, name_len) }) else {
        return;
    };
    let name = String::from_utf8_lossy(name).into_owned();
    with_runtime_mut(|runtime| {
        let Some(heap_index) = heap_index(record) else {
            return;
        };
        let Some(object) = runtime.heap.get_mut(heap_index).and_then(Option::as_mut) else {
            return;
        };
        if let HeapObjectKind::Record { fields, .. } = &mut object.kind
            && let Some(field) = fields.get_mut(index)
        {
            *field = (name, value);
        }
    });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_record_get(
    record: RtValue,
    name_ptr: *const u8,
    name_len: usize,
) -> RtValue {
    let Some(name) = (unsafe { bytes_from_raw(name_ptr, name_len) }) else {
        return VALUE_NULL;
    };
    let name = String::from_utf8_lossy(name);
    with_runtime_mut(|runtime| {
        let Some(heap_index) = heap_index(record) else {
            return VALUE_NULL;
        };
        let Some(object) = runtime.heap.get(heap_index).and_then(Option::as_ref) else {
            return VALUE_NULL;
        };
        match &object.kind {
            HeapObjectKind::Record { fields, .. } => fields
                .iter()
                .find_map(|(field_name, value)| (field_name == name.as_ref()).then_some(*value))
                .unwrap_or(VALUE_NULL),
            _ => VALUE_NULL,
        }
    })
    .unwrap_or(VALUE_NULL)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_bytes_ptr(value: RtValue) -> *const u8 {
    with_runtime_mut(|runtime| {
        runtime
            .value_bytes(value)
            .map_or(ptr::null(), |bytes| bytes.as_ptr())
    })
    .unwrap_or(ptr::null())
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_bytes_len(value: RtValue) -> usize {
    with_runtime_mut(|runtime| runtime.value_bytes(value).map_or(0, <[u8]>::len)).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_eq(lhs: RtValue, rhs: RtValue) -> bool {
    with_runtime_mut(|runtime| runtime.values_equal(lhs, rhs)).unwrap_or(false)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_lt(lhs: RtValue, rhs: RtValue) -> bool {
    with_runtime_mut(|runtime| runtime.values_less_than(lhs, rhs)).unwrap_or(false)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_println_value(value: RtValue) {
    if let Some(rendered) = with_runtime_mut(|runtime| runtime.render_value(value)) {
        write_bytes_line(rendered.as_bytes());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_dbg_value(value: RtValue) {
    riot_rt_println_value(value);
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_gc_collect() -> usize {
    with_runtime_mut(Runtime::collect_garbage).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_gc_heap_len() -> usize {
    with_runtime_mut(|runtime| runtime.heap.iter().filter(|slot| slot.is_some()).count())
        .unwrap_or(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_root_push(value: RtValue) {
    with_runtime_mut(|runtime| runtime.push_root(value));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_root_pop(count: usize) {
    with_runtime_mut(|runtime| runtime.pop_roots(count));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_alloc_frame(size: usize) -> *mut u8 {
    riot_rt_alloc_frame_v2(size, 8, None)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_alloc_frame_v2(
    size: usize,
    align: usize,
    _drop_fn: Option<ActorDropFn>,
) -> *mut u8 {
    let Ok(layout) = Layout::from_size_align(size.max(1), align.max(1)) else {
        return ptr::null_mut();
    };
    unsafe { alloc_zeroed(layout) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_free_frame_v2(
    frame: *mut u8,
    size: usize,
    align: usize,
    drop_fn: Option<ActorDropFn>,
) {
    unsafe { free_frame(frame, RtFrameLayout::new(size, align, drop_fn)) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_spawn_actor(frame: *mut u8, resume: Option<ActorResumeFn>) -> u64 {
    let Some(resume) = resume else {
        return 0;
    };
    with_runtime_mut(|runtime| runtime.spawn(frame, resume, RtFrameLayout::legacy())).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_spawn_actor_v2(
    frame: *mut u8,
    resume: Option<ActorResumeFn>,
    size: usize,
    align: usize,
    drop_fn: Option<ActorDropFn>,
) -> u64 {
    let Some(resume) = resume else {
        return 0;
    };
    with_runtime_mut(|runtime| {
        runtime.spawn(frame, resume, RtFrameLayout::new(size, align, drop_fn))
    })
    .unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_send(pid: u64, ptr: *const u8, len: usize) {
    let Some(bytes) = (unsafe { bytes_from_raw(ptr, len) }) else {
        return;
    };

    with_runtime_mut(|runtime| runtime.send(pid, RuntimeMessage::Bytes(bytes.to_vec())));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_i64(pid: u64, value: i64) {
    with_runtime_mut(|runtime| runtime.send(pid, RuntimeMessage::I64(value)));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_bool(pid: u64, value: bool) {
    with_runtime_mut(|runtime| runtime.send(pid, RuntimeMessage::Bool(value)));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_pid(pid: u64, value: u64) {
    with_runtime_mut(|runtime| runtime.send(pid, RuntimeMessage::Pid(value)));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_value(pid: u64, value: RtValue) {
    with_runtime_mut(|runtime| {
        runtime.promote_shared(value);
        runtime.send(pid, RuntimeMessage::Value(value));
    });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_send_msg(pid: u64, message: *const RtMessage) {
    let Some(message) = (unsafe { runtime_message_from_raw(message) }) else {
        return;
    };

    with_runtime_mut(|runtime| runtime.send(pid, message));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_dbg_msg(message: *const RtMessage) {
    let Some(message) = (unsafe { runtime_message_from_raw(message) }) else {
        return;
    };

    write_bytes_line(render_message(&message).as_bytes());
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_monitor(pid: u64) {
    with_runtime_mut(|runtime| runtime.monitor(pid));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_link(pid: u64) {
    with_runtime_mut(|runtime| runtime.link(pid));
}

fn with_runtime_mut<R>(f: impl FnOnce(&mut Runtime) -> R) -> Option<R> {
    let active_runtime = ACTIVE_RUNTIME.with(Cell::get);
    if !active_runtime.is_null() {
        return Some(unsafe { f(&mut *active_runtime) });
    }

    runtime().lock().ok().map(|mut runtime| f(&mut runtime))
}

fn runtime() -> &'static Mutex<Runtime> {
    RUNTIME.get_or_init(|| Mutex::new(Runtime::default()))
}

unsafe fn free_frame(frame: *mut u8, layout: RtFrameLayout) {
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

fn pid_index(pid: u64) -> Option<usize> {
    usize::try_from(pid).ok()?.checked_sub(1)
}

unsafe fn write_stdout_line(ptr: *const u8, len: usize) {
    let Some(bytes) = (unsafe { bytes_from_raw(ptr, len) }) else {
        return;
    };
    write_bytes_line(bytes);
}

unsafe fn runtime_message_from_raw(message: *const RtMessage) -> Option<RuntimeMessage> {
    let message = unsafe { message.as_ref() }?;
    match message.kind {
        MSG_BYTES => unsafe { bytes_from_raw(message.ptr, message.len) }
            .map(|bytes| RuntimeMessage::Bytes(bytes.to_vec())),
        MSG_I64 => Some(RuntimeMessage::I64(message.i64_value)),
        MSG_BOOL => Some(RuntimeMessage::Bool(message.bool_value != 0)),
        MSG_PID => u64::try_from(message.i64_value)
            .ok()
            .map(RuntimeMessage::Pid),
        MSG_VALUE => Some(RuntimeMessage::Value(message.value)),
        _ => None,
    }
}

fn render_message(message: &RuntimeMessage) -> String {
    match message {
        RuntimeMessage::Bytes(bytes) => String::from_utf8_lossy(bytes).into_owned(),
        RuntimeMessage::I64(value) => value.to_string(),
        RuntimeMessage::Bool(true) => "true".to_owned(),
        RuntimeMessage::Bool(false) => "false".to_owned(),
        RuntimeMessage::Pid(value) => value.to_string(),
        RuntimeMessage::Value(value) => {
            with_runtime_mut(|runtime| runtime.render_value(*value)).unwrap_or_default()
        }
    }
}

fn current_actor_pid() -> u64 {
    ACTIVE_PID.with(Cell::get)
}

fn value_tag(value: RtValue) -> RtValue {
    value & VALUE_TAG_MASK
}

fn value_i64(value: i64) -> RtValue {
    ((value as RtValue) << 4) | VALUE_I64_TAG
}

fn value_i64_payload(value: RtValue) -> Option<i64> {
    (value_tag(value) == VALUE_I64_TAG).then_some((value as i64) >> 4)
}

fn value_bool(value: bool) -> RtValue {
    if value {
        VALUE_BOOL_TRUE
    } else {
        VALUE_BOOL_FALSE
    }
}

fn value_pid(value: u64) -> RtValue {
    (value << 4) | VALUE_PID_TAG
}

fn value_pid_payload(value: RtValue) -> Option<u64> {
    (value_tag(value) == VALUE_PID_TAG).then_some(value >> 4)
}

fn value_heap(index: usize) -> RtValue {
    (((index as RtValue) + 1) << 4) | VALUE_HEAP_TAG
}

fn heap_index(value: RtValue) -> Option<usize> {
    if value == VALUE_NULL || value_tag(value) != VALUE_HEAP_TAG {
        return None;
    }
    usize::try_from((value >> 4).checked_sub(1)?).ok()
}

unsafe fn bytes_from_raw<'a>(ptr: *const u8, len: usize) -> Option<&'a [u8]> {
    if len == 0 {
        return Some(&[]);
    }

    if ptr.is_null() && len != 0 {
        return None;
    }

    Some(unsafe { slice::from_raw_parts(ptr, len) })
}

unsafe fn values_from_raw<'a>(ptr: *const RtValue, len: usize) -> Option<&'a [RtValue]> {
    if len == 0 {
        return Some(&[]);
    }

    if ptr.is_null() && len != 0 {
        return None;
    }

    Some(unsafe { slice::from_raw_parts(ptr, len) })
}

fn write_bytes_line(bytes: &[u8]) {
    let mut stdout = io::stdout().lock();
    let _ = stdout.write_all(bytes);
    let _ = stdout.write_all(b"\n");
}

#[cfg(test)]
mod tests {
    use std::ptr;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Mutex, OnceLock};

    use super::*;

    static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);
    static MESSAGES: OnceLock<Mutex<Vec<String>>> = OnceLock::new();

    unsafe extern "C" fn count_drop(_frame: *mut u8) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    unsafe extern "C" fn record_resume(
        frame: *mut u8,
        _pid: u64,
        message: *const RtMessage,
    ) -> u32 {
        if message.is_null() {
            return POLL_WAITING;
        }
        let count = unsafe { &mut *(frame as *mut u64) };
        let message = unsafe { runtime_message_from_raw(message) }.unwrap();
        MESSAGES
            .get_or_init(|| Mutex::new(Vec::new()))
            .lock()
            .unwrap()
            .push(render_message(&message));
        *count += 1;
        let mut flags = POLL_CONSUMED | POLL_PROGRESS;
        if *count == 2 {
            flags |= POLL_DONE;
        }
        flags
    }

    #[test]
    fn frame_alloc_v2_honors_alignment() {
        let frame = riot_rt_alloc_frame_v2(64, 64, None);
        assert!(!frame.is_null());
        assert_eq!((frame as usize) % 64, 0);
        unsafe { riot_rt_free_frame_v2(frame, 64, 64, None) };
    }

    #[test]
    fn frame_free_v2_runs_drop_hook() {
        DROP_COUNT.store(0, Ordering::SeqCst);
        let frame = riot_rt_alloc_frame_v2(8, 8, Some(count_drop));
        assert!(!frame.is_null());
        unsafe { riot_rt_free_frame_v2(frame, 8, 8, Some(count_drop)) };
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn mailbox_consumes_in_fifo_order_without_front_removal_semantics() {
        riot_rt_init();
        MESSAGES
            .get_or_init(|| Mutex::new(Vec::new()))
            .lock()
            .unwrap()
            .clear();
        let frame = riot_rt_alloc_frame_v2(8, 8, None);
        assert!(!frame.is_null());
        unsafe { ptr::write(frame as *mut u64, 0) };
        let pid = unsafe { riot_rt_spawn_actor_v2(frame, Some(record_resume), 8, 8, None) };
        assert_eq!(pid, 1);
        unsafe { riot_rt_send(pid, b"one".as_ptr(), 3) };
        riot_rt_send_i64(pid, 2);
        riot_rt_shutdown();
        let messages = MESSAGES.get().unwrap().lock().unwrap().clone();
        assert_eq!(messages, ["one", "2"]);
    }

    #[test]
    fn value_heap_collects_unrooted_values_and_preserves_roots() {
        riot_rt_init();

        let unrooted = unsafe { riot_rt_value_string(b"temporary".as_ptr(), 9) };
        assert_ne!(unrooted, VALUE_NULL);
        assert_eq!(riot_rt_gc_heap_len(), 1);
        assert_eq!(riot_rt_gc_collect(), 1);
        assert_eq!(riot_rt_gc_heap_len(), 0);

        let rooted = unsafe { riot_rt_value_string(b"rooted".as_ptr(), 6) };
        riot_rt_root_push(rooted);
        assert_eq!(riot_rt_gc_collect(), 0);
        assert_eq!(riot_rt_gc_heap_len(), 1);
        riot_rt_root_pop(1);
        assert_eq!(riot_rt_gc_collect(), 1);
        assert_eq!(riot_rt_gc_heap_len(), 0);
    }

    #[test]
    fn value_equality_handles_nested_runtime_values() {
        riot_rt_init();

        assert!(riot_rt_value_eq(riot_rt_value_unit(), riot_rt_value_unit()));
        assert!(riot_rt_value_eq(riot_rt_value_bool(true), riot_rt_value_bool(true)));
        assert!(!riot_rt_value_eq(riot_rt_value_bool(true), riot_rt_value_bool(false)));
        assert!(riot_rt_value_eq(riot_rt_value_i64(42), riot_rt_value_i64(42)));
        assert!(!riot_rt_value_eq(riot_rt_value_i64(42), riot_rt_value_i64(7)));

        let lhs_label = unsafe { riot_rt_value_string(b"riot".as_ptr(), 4) };
        let rhs_label = unsafe { riot_rt_value_string(b"riot".as_ptr(), 4) };
        let other_label = unsafe { riot_rt_value_string(b"stage0".as_ptr(), 6) };
        assert!(riot_rt_value_eq(lhs_label, rhs_label));
        assert!(!riot_rt_value_eq(lhs_label, other_label));

        let lhs_items = [lhs_label, riot_rt_value_i64(1)];
        let rhs_items = [rhs_label, riot_rt_value_i64(1)];
        let other_items = [rhs_label, riot_rt_value_i64(2)];
        let lhs_tuple = unsafe { riot_rt_value_tuple(lhs_items.as_ptr(), lhs_items.len()) };
        let rhs_tuple = unsafe { riot_rt_value_tuple(rhs_items.as_ptr(), rhs_items.len()) };
        let other_tuple = unsafe { riot_rt_value_tuple(other_items.as_ptr(), other_items.len()) };
        assert!(riot_rt_value_eq(lhs_tuple, rhs_tuple));
        assert!(!riot_rt_value_eq(lhs_tuple, other_tuple));

        let lhs_list_items = [lhs_tuple, riot_rt_value_bool(false)];
        let rhs_list_items = [rhs_tuple, riot_rt_value_bool(false)];
        let lhs_list = unsafe { riot_rt_value_list(lhs_list_items.as_ptr(), lhs_list_items.len()) };
        let rhs_list = unsafe { riot_rt_value_list(rhs_list_items.as_ptr(), rhs_list_items.len()) };
        assert!(riot_rt_value_eq(lhs_list, rhs_list));

        let lhs_record = unsafe { riot_rt_value_record_begin(b"Box".as_ptr(), 3, 2) };
        let rhs_record = unsafe { riot_rt_value_record_begin(b"Box".as_ptr(), 3, 2) };
        unsafe {
            riot_rt_value_record_set(lhs_record, 0, b"items".as_ptr(), 5, lhs_list);
            riot_rt_value_record_set(lhs_record, 1, b"ok".as_ptr(), 2, riot_rt_value_bool(true));
            riot_rt_value_record_set(rhs_record, 0, b"items".as_ptr(), 5, rhs_list);
            riot_rt_value_record_set(rhs_record, 1, b"ok".as_ptr(), 2, riot_rt_value_bool(true));
        }
        assert!(riot_rt_value_eq(lhs_record, rhs_record));
        assert!(!riot_rt_value_eq(lhs_record, lhs_list));
    }

    #[test]
    fn value_ordering_handles_i64_and_strings() {
        riot_rt_init();

        assert!(riot_rt_value_lt(riot_rt_value_i64(1), riot_rt_value_i64(2)));
        assert!(!riot_rt_value_lt(riot_rt_value_i64(2), riot_rt_value_i64(1)));

        let alpha = unsafe { riot_rt_value_string(b"alpha".as_ptr(), 5) };
        let beta = unsafe { riot_rt_value_string(b"beta".as_ptr(), 4) };
        assert!(riot_rt_value_lt(alpha, beta));
        assert!(!riot_rt_value_lt(beta, alpha));

        let tuple_items = [alpha];
        let tuple = unsafe { riot_rt_value_tuple(tuple_items.as_ptr(), tuple_items.len()) };
        assert!(!riot_rt_value_lt(tuple, tuple));
    }

    #[test]
    fn value_rendering_handles_compound_values() {
        riot_rt_init();

        let label = unsafe { riot_rt_value_string(b"riot".as_ptr(), 4) };
        let values = [label, riot_rt_value_i64(42)];
        let tuple = unsafe { riot_rt_value_tuple(values.as_ptr(), values.len()) };
        assert_eq!(
            with_runtime_mut(|runtime| runtime.render_value(tuple)).unwrap(),
            "(riot, 42)"
        );

        let record = unsafe { riot_rt_value_record_begin(b"Point".as_ptr(), 5, 2) };
        unsafe {
            riot_rt_value_record_set(record, 0, b"x".as_ptr(), 1, riot_rt_value_i64(10));
            riot_rt_value_record_set(record, 1, b"y".as_ptr(), 1, riot_rt_value_i64(20));
        }
        assert_eq!(
            with_runtime_mut(|runtime| runtime.render_value(record)).unwrap(),
            "Point { x: 10, y: 20 }"
        );

        let x = unsafe { riot_rt_value_record_get(record, b"x".as_ptr(), 1) };
        assert!(riot_rt_value_eq(x, riot_rt_value_i64(10)));
        let missing = unsafe { riot_rt_value_record_get(record, b"z".as_ptr(), 1) };
        assert_eq!(missing, VALUE_NULL);
    }
}
