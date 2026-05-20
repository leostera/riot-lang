use std::cell::{Cell, UnsafeCell};
use std::io::{self, Write};
use std::ptr;

use crate::actor::{
    ActorResumeFn, ActorSlot, POLL_CONSUMED, POLL_DONE, POLL_PROGRESS, POLL_WAITING, POLL_YIELD,
    RtMessage, RuntimeMessage, actor_from_id,
};
use crate::actor_id::ActorId;
use crate::frame::RtFrameLayout;
use crate::value::{
    ClosureApplyFn, HeapObject, HeapObjectKind, HeapOwner, RtValue, VALUE_ACTOR_ID_TAG,
    VALUE_BOOL_FALSE, VALUE_BOOL_TRUE, VALUE_HEAP_TAG, VALUE_I64_TAG, VALUE_UNIT, heap_index,
    value_actor_id_payload, value_i64, value_i64_payload, value_tag,
};

const PRIMARY_SCHEDULER_ID: u32 = 0;
const DEFAULT_GC_THRESHOLD: usize = 1024;

struct SchedulerCell {
    scheduler: UnsafeCell<SchedulerLocal>,
}

impl SchedulerCell {
    fn new(scheduler_id: u32) -> Self {
        Self {
            scheduler: UnsafeCell::new(SchedulerLocal::new(scheduler_id)),
        }
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut SchedulerLocal) -> R) -> R {
        unsafe { f(&mut *self.scheduler.get()) }
    }
}

thread_local! {
    static LOCAL_SCHEDULER: SchedulerCell = SchedulerCell::new(PRIMARY_SCHEDULER_ID);
    static ACTIVE_SCHEDULER: Cell<*mut SchedulerLocal> = const { Cell::new(ptr::null_mut()) };
    static ACTIVE_ACTOR_ID: Cell<ActorId> = const { Cell::new(ActorId::NULL) };
}

pub(crate) struct SchedulerLocal {
    scheduler_id: u32,
    actors: Vec<Box<ActorSlot>>,
    retired_actors: Vec<Box<ActorSlot>>,
    pub(crate) heap: Vec<Option<HeapObject>>,
    roots: Vec<RtValue>,
    gc_threshold: usize,
    gc_collection_count: usize,
}

impl SchedulerLocal {
    fn new(scheduler_id: u32) -> Self {
        Self {
            scheduler_id,
            actors: Vec::new(),
            retired_actors: Vec::new(),
            heap: Vec::new(),
            roots: Vec::new(),
            gc_threshold: DEFAULT_GC_THRESHOLD,
            gc_collection_count: 0,
        }
    }

    pub(crate) fn reset(&mut self) {
        self.shutdown();
        self.heap.clear();
        self.roots.clear();
        self.gc_threshold = DEFAULT_GC_THRESHOLD;
        self.gc_collection_count = 0;
    }

    pub(crate) fn spawn(
        &mut self,
        frame: *mut u8,
        resume: ActorResumeFn,
        layout: RtFrameLayout,
    ) -> ActorId {
        if frame.is_null() {
            return ActorId::NULL;
        }

        let actor = ActorSlot::boxed(
            self.scheduler_id,
            (self.actors.len() + 1) as u64,
            frame,
            layout,
            resume,
        );
        let actor_id = actor.actor_id();
        self.actors.push(actor);
        actor_id
    }

    pub(crate) fn send(&mut self, actor_id: ActorId, message: RuntimeMessage) {
        let Some(actor) = (unsafe { actor_from_id(actor_id) }) else {
            return;
        };
        actor.send(message);
    }

    pub(crate) fn register_frame_roots(&mut self, actor_id: ActorId, offsets: &[usize]) {
        if let Some(actor) = unsafe { actor_from_id(actor_id) } {
            actor.set_frame_root_offsets(offsets);
        }
    }

    pub(crate) fn monitor(&mut self, actor_id: ActorId) {
        let watcher = current_actor_id();
        if watcher.is_null() {
            return;
        }
        let Some(actor) = (unsafe { actor_from_id(actor_id) }) else {
            return;
        };
        if actor.is_terminated() {
            self.send_monitor_down(watcher, actor.display_id());
        } else {
            actor.push_monitor(watcher);
        }
    }

    pub(crate) fn link(&mut self, actor_id: ActorId) {
        let peer = current_actor_id();
        if peer.is_null() {
            return;
        }
        let Some(actor) = (unsafe { actor_from_id(actor_id) }) else {
            return;
        };
        let Some(peer_actor) = (unsafe { actor_from_id(peer) }) else {
            return;
        };
        if actor.is_terminated() {
            self.terminate_actor(peer);
            return;
        }
        actor.push_link(peer);
        peer_actor.push_link(actor_id);
    }

    pub(crate) fn shutdown(&mut self) {
        self.schedule_until_quiescent();

        for index in 0..self.actors.len() {
            self.terminate_actor_at(index);
            self.schedule_until_quiescent();
        }

        self.schedule_until_quiescent();

        self.retired_actors.extend(self.actors.drain(..));
        self.roots.clear();
        self.collect_garbage();
    }

    fn terminate_actor_at(&mut self, index: usize) {
        let Some(actor) = self.actors.get(index) else {
            return;
        };
        self.terminate_actor(actor.actor_id());
    }

    fn terminate_actor(&mut self, actor_id: ActorId) {
        let Some(actor) = (unsafe { actor_from_id(actor_id) }) else {
            return;
        };
        let display_id = actor.display_id();
        let monitors = actor.monitor_ids();
        let links = actor.link_ids();
        if !actor.terminate() {
            return;
        }
        for watcher in monitors {
            self.send_monitor_down(watcher, display_id);
        }
        for peer in links {
            self.terminate_actor(peer);
        }
    }

    fn send_monitor_down(&mut self, watcher: ActorId, display_id: u64) {
        let value = self.alloc_value(
            HeapObjectKind::Variant {
                type_name: "monitor_down".to_owned(),
                constructor: "Down".to_owned(),
                payload: value_i64(display_id as i64),
            },
            HeapOwner::Shared,
        );
        self.send(watcher, RuntimeMessage::Value(value));
    }

    pub(crate) fn alloc_value(&mut self, kind: HeapObjectKind, owner: HeapOwner) -> RtValue {
        if self.heap_len() >= self.gc_threshold {
            self.collect_garbage();
        }
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
            return crate::value::value_heap(index);
        }
        self.heap.push(Some(object));
        crate::value::value_heap(self.heap.len() - 1)
    }

    pub(crate) fn push_root(&mut self, value: RtValue) {
        self.roots.push(value);
    }

    pub(crate) fn pop_roots(&mut self, count: usize) {
        let keep = self.roots.len().saturating_sub(count);
        self.roots.truncate(keep);
    }

    pub(crate) fn promote_shared(&mut self, value: RtValue) {
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

    pub(crate) fn collect_garbage(&mut self) -> usize {
        self.gc_collection_count += 1;
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
            .flat_map(|actor| actor.root_values())
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

    pub(crate) fn heap_len(&self) -> usize {
        self.heap.iter().filter(|slot| slot.is_some()).count()
    }

    pub(crate) fn set_gc_threshold(&mut self, threshold: usize) {
        self.gc_threshold = threshold.max(1);
    }

    pub(crate) fn gc_collection_count(&self) -> usize {
        self.gc_collection_count
    }

    pub(crate) fn retired_actor_count(&self) -> usize {
        self.retired_actors.len()
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

    pub(crate) fn render_value(&self, value: RtValue) -> String {
        match value_tag(value) {
            VALUE_I64_TAG => value_i64_payload(value)
                .map(|value| value.to_string())
                .unwrap_or_else(|| "<invalid-int>".to_owned()),
            VALUE_BOOL_FALSE => "false".to_owned(),
            VALUE_BOOL_TRUE => "true".to_owned(),
            VALUE_UNIT => "()".to_owned(),
            VALUE_ACTOR_ID_TAG => value_actor_id_payload(value)
                .map(|value| value.as_raw().to_string())
                .unwrap_or_else(|| "<invalid-actor-id>".to_owned()),
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
            HeapObjectKind::Closure { .. } => "<closure>".to_owned(),
            HeapObjectKind::Record { path, fields } => {
                let rendered = fields
                    .iter()
                    .map(|(name, value)| format!("{name}: {}", self.render_value(*value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{path} {{ {rendered} }}")
            }
            HeapObjectKind::Variant {
                constructor,
                payload,
                ..
            } => {
                if *payload == VALUE_UNIT {
                    constructor.clone()
                } else {
                    let rendered = self.render_value(*payload);
                    if matches!(
                        heap_index(*payload)
                            .and_then(|index| self.heap.get(index))
                            .and_then(Option::as_ref)
                            .map(|object| &object.kind),
                        Some(HeapObjectKind::Tuple(_))
                    ) {
                        format!("{constructor}{rendered}")
                    } else {
                        format!("{constructor}({rendered})")
                    }
                }
            }
        }
    }

    pub(crate) fn value_bytes(&self, value: RtValue) -> Option<&[u8]> {
        let index = heap_index(value)?;
        let object = self.heap.get(index)?.as_ref()?;
        match &object.kind {
            HeapObjectKind::String(bytes) => Some(bytes.as_slice()),
            _ => None,
        }
    }

    pub(crate) fn values_equal(&self, lhs: RtValue, rhs: RtValue) -> bool {
        if lhs == rhs {
            return true;
        }
        match (value_tag(lhs), value_tag(rhs)) {
            (VALUE_I64_TAG, VALUE_I64_TAG) => value_i64_payload(lhs) == value_i64_payload(rhs),
            (VALUE_BOOL_FALSE, VALUE_BOOL_FALSE) | (VALUE_BOOL_TRUE, VALUE_BOOL_TRUE) => true,
            (VALUE_UNIT, VALUE_UNIT) => true,
            (VALUE_ACTOR_ID_TAG, VALUE_ACTOR_ID_TAG) => {
                value_actor_id_payload(lhs) == value_actor_id_payload(rhs)
            }
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
            (
                HeapObjectKind::Variant {
                    type_name: lhs_type,
                    constructor: lhs_constructor,
                    payload: lhs_payload,
                },
                HeapObjectKind::Variant {
                    type_name: rhs_type,
                    constructor: rhs_constructor,
                    payload: rhs_payload,
                },
            ) => {
                lhs_type == rhs_type
                    && lhs_constructor == rhs_constructor
                    && self.values_equal(*lhs_payload, *rhs_payload)
            }
            _ => false,
        }
    }

    pub(crate) fn closure_parts(&self, value: RtValue) -> Option<(ClosureApplyFn, Vec<RtValue>)> {
        let index = heap_index(value)?;
        let object = self.heap.get(index)?.as_ref()?;
        match &object.kind {
            HeapObjectKind::Closure { apply, captures } => Some((*apply, captures.clone())),
            _ => None,
        }
    }

    pub(crate) fn values_less_than(&self, lhs: RtValue, rhs: RtValue) -> bool {
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
                if self
                    .actors
                    .get(index)
                    .is_none_or(|actor| actor.is_terminated())
                {
                    continue;
                }

                let (actor_id, frame, resume, current_messages) = {
                    let actor = &self.actors[index];
                    (
                        actor.actor_id(),
                        actor.frame_ptr(),
                        actor.resume(),
                        actor.current_messages(),
                    )
                };
                let mut consumed = None;
                let mut terminate = false;

                if current_messages.is_empty() {
                    let result = ACTIVE_SCHEDULER.with(|active| {
                        let previous = active.replace(self as *mut SchedulerLocal);
                        let previous_actor_id = ACTIVE_ACTOR_ID
                            .with(|active_actor_id| active_actor_id.replace(actor_id));
                        let result = unsafe { resume(frame, actor_id, ptr::null()) };
                        ACTIVE_ACTOR_ID
                            .with(|active_actor_id| active_actor_id.set(previous_actor_id));
                        active.set(previous);
                        result
                    });
                    made_progress |= result & (POLL_PROGRESS | POLL_YIELD) != 0;
                    terminate = result & POLL_DONE != 0;
                } else {
                    for (message_index, current_message) in current_messages.iter().enumerate() {
                        let message = current_message.as_rt_message();
                        let message_ptr = &message as *const RtMessage;
                        let result = ACTIVE_SCHEDULER.with(|active| {
                            let previous = active.replace(self as *mut SchedulerLocal);
                            let previous_actor_id = ACTIVE_ACTOR_ID
                                .with(|active_actor_id| active_actor_id.replace(actor_id));
                            let result = unsafe { resume(frame, actor_id, message_ptr) };
                            ACTIVE_ACTOR_ID
                                .with(|active_actor_id| active_actor_id.set(previous_actor_id));
                            active.set(previous);
                            result
                        });

                        made_progress |= result & (POLL_PROGRESS | POLL_YIELD) != 0;

                        if result & POLL_CONSUMED != 0 {
                            consumed = Some(message_index);
                        }
                        if result & POLL_DONE != 0 {
                            terminate = true;
                        }
                        if consumed.is_some()
                            || terminate
                            || result & (POLL_PROGRESS | POLL_YIELD) != 0
                            || result & POLL_WAITING == 0
                        {
                            break;
                        }
                    }
                }

                if let Some(actor) = self.actors.get(index) {
                    if let Some(message_index) = consumed {
                        actor.consume_message(message_index);
                    }
                    if !terminate && consumed.is_none() {
                        continue;
                    }
                }
                if terminate {
                    self.terminate_actor_at(index);
                    made_progress = true;
                }
            }

            if !made_progress {
                break;
            }
        }
    }
}

pub(crate) fn with_scheduler_mut<R>(f: impl FnOnce(&mut SchedulerLocal) -> R) -> R {
    let active_scheduler = ACTIVE_SCHEDULER.with(Cell::get);
    if !active_scheduler.is_null() {
        return unsafe { f(&mut *active_scheduler) };
    }

    LOCAL_SCHEDULER.with(|scheduler| scheduler.with_mut(f))
}

pub(crate) fn current_actor_id() -> ActorId {
    ACTIVE_ACTOR_ID.with(Cell::get)
}

pub(crate) fn runtime_abort(message: &str) -> ! {
    let _ = writeln!(io::stderr(), "riot runtime fatal: {message}");
    std::process::abort()
}

pub(crate) fn render_message(message: &RuntimeMessage) -> String {
    match message {
        RuntimeMessage::Bytes(bytes) => String::from_utf8_lossy(bytes).into_owned(),
        RuntimeMessage::I64(value) => value.to_string(),
        RuntimeMessage::Bool(true) => "true".to_owned(),
        RuntimeMessage::Bool(false) => "false".to_owned(),
        RuntimeMessage::Unit => "()".to_owned(),
        RuntimeMessage::ActorId(value) => value.as_raw().to_string(),
        RuntimeMessage::Value(value) => with_scheduler_mut(|runtime| runtime.render_value(*value)),
    }
}
