use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Mutex, OnceLock};

use crate::abi::{
    riot_rt_actor_frame_roots, riot_rt_alloc_frame_v2, riot_rt_free_frame_v2, riot_rt_gc_collect,
    riot_rt_gc_collection_count, riot_rt_gc_heap_len, riot_rt_gc_set_threshold, riot_rt_init,
    riot_rt_msg_value, riot_rt_root_pop, riot_rt_root_push, riot_rt_send, riot_rt_send_i64,
    riot_rt_shutdown, riot_rt_spawn_actor_v2, riot_rt_value_apply, riot_rt_value_as_bool,
    riot_rt_value_as_i64, riot_rt_value_bool, riot_rt_value_closure, riot_rt_value_eq,
    riot_rt_value_i64, riot_rt_value_list, riot_rt_value_list_get, riot_rt_value_list_len,
    riot_rt_value_record_begin, riot_rt_value_record_get, riot_rt_value_record_is,
    riot_rt_value_record_set, riot_rt_value_string, riot_rt_value_string_concat,
    riot_rt_value_string_len, riot_rt_value_tuple, riot_rt_value_tuple_arity_is,
    riot_rt_value_tuple_get, riot_rt_value_unit, riot_rt_value_variant,
    riot_rt_value_variant_get_payload, riot_rt_value_variant_is, riot_rt_value_variant_payload,
};
use crate::actor::{
    ActorSlot, POLL_CONSUMED, POLL_DONE, POLL_PROGRESS, POLL_WAITING, RtMessage, RuntimeMessage,
    actor_from_id, runtime_message_from_raw,
};
use crate::actor_id::ActorId;
use crate::frame::RtFrameLayout;
use crate::scheduler::{render_message, with_scheduler_mut};
use crate::value::{RtValue, VALUE_NULL, VALUE_TAG_MASK};

static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);
static MESSAGES: OnceLock<Mutex<Vec<String>>> = OnceLock::new();
static RUNTIME_TEST_LOCK: OnceLock<Mutex<()>> = OnceLock::new();

fn runtime_test_guard() -> std::sync::MutexGuard<'static, ()> {
    RUNTIME_TEST_LOCK
        .get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap()
}

unsafe extern "C" fn count_drop(_frame: *mut u8) {
    DROP_COUNT.fetch_add(1, Ordering::SeqCst);
}

unsafe extern "C" fn record_resume(
    frame: *mut u8,
    _actor_id: ActorId,
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

unsafe extern "C" fn idle_resume(
    _frame: *mut u8,
    _actor_id: ActorId,
    _message: *const RtMessage,
) -> u32 {
    POLL_WAITING
}

unsafe extern "C" fn return_first_capture_or_argument(
    captures: *const RtValue,
    len: usize,
    argument: RtValue,
) -> RtValue {
    if len == 0 {
        return argument;
    }
    unsafe { *captures }
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
    let _guard = runtime_test_guard();
    riot_rt_init();
    MESSAGES
        .get_or_init(|| Mutex::new(Vec::new()))
        .lock()
        .unwrap()
        .clear();
    let frame = riot_rt_alloc_frame_v2(8, 8, None);
    assert!(!frame.is_null());
    unsafe { ptr::write(frame as *mut u64, 0) };
    let actor_id = unsafe { riot_rt_spawn_actor_v2(frame, Some(record_resume), 8, 8, None) };
    assert_ne!(actor_id, ActorId::NULL);
    assert_eq!(actor_id.as_raw() & VALUE_TAG_MASK, 0);
    unsafe { riot_rt_send(actor_id, b"one".as_ptr(), 3) };
    riot_rt_send_i64(actor_id, 2);
    riot_rt_shutdown();
    let messages = MESSAGES.get().unwrap().lock().unwrap().clone();
    assert_eq!(messages, ["one", "2"]);
}

#[test]
fn value_heap_collects_unrooted_values_and_preserves_roots() {
    let _guard = runtime_test_guard();
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
fn value_roots_preserve_nested_children() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    let label = unsafe { riot_rt_value_string(b"rooted".as_ptr(), 6) };
    let values = [label];
    let tuple = unsafe { riot_rt_value_tuple(values.as_ptr(), values.len()) };
    riot_rt_root_push(tuple);
    assert_eq!(riot_rt_gc_collect(), 0);
    assert_eq!(riot_rt_gc_heap_len(), 2);

    riot_rt_root_pop(1);
    assert_eq!(riot_rt_gc_collect(), 2);
    assert_eq!(riot_rt_gc_heap_len(), 0);
}

#[test]
fn closure_values_apply_and_trace_captures() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    let captured = unsafe { riot_rt_value_string(b"captured".as_ptr(), 8) };
    let captures = [captured];
    let closure = unsafe {
        riot_rt_value_closure(
            Some(return_first_capture_or_argument),
            captures.as_ptr(),
            captures.len(),
        )
    };
    riot_rt_root_push(closure);

    assert_eq!(
        with_scheduler_mut(|scheduler| scheduler.render_value(closure)),
        "<closure>"
    );
    assert!(riot_rt_value_eq(
        riot_rt_value_apply(closure, riot_rt_value_unit()),
        captured
    ));
    assert_eq!(riot_rt_gc_collect(), 0);
    assert_eq!(riot_rt_gc_heap_len(), 2);

    riot_rt_root_pop(1);
    assert_eq!(riot_rt_gc_collect(), 2);
    assert_eq!(riot_rt_gc_heap_len(), 0);
}

#[test]
fn closure_values_apply_arguments_without_captures() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    let closure =
        unsafe { riot_rt_value_closure(Some(return_first_capture_or_argument), ptr::null(), 0) };

    assert!(riot_rt_value_eq(
        riot_rt_value_apply(closure, riot_rt_value_i64(7)),
        riot_rt_value_i64(7)
    ));

    assert_eq!(riot_rt_gc_collect(), 1);
    assert_eq!(riot_rt_gc_heap_len(), 0);
}

#[test]
fn scalar_runtime_values_unbox_through_the_abi() {
    assert_eq!(riot_rt_value_as_i64(riot_rt_value_i64(42)), 42);
    assert!(riot_rt_value_as_bool(riot_rt_value_bool(true)));
    assert!(!riot_rt_value_as_bool(riot_rt_value_bool(false)));
}

#[test]
fn allocation_pressure_collects_before_heap_growth() {
    let _guard = runtime_test_guard();
    riot_rt_init();
    riot_rt_gc_set_threshold(1);

    let rooted = unsafe { riot_rt_value_string(b"rooted".as_ptr(), 6) };
    riot_rt_root_push(rooted);
    let _garbage = unsafe { riot_rt_value_string(b"garbage".as_ptr(), 7) };
    let _more = unsafe { riot_rt_value_string(b"more".as_ptr(), 4) };

    assert!(riot_rt_gc_collection_count() >= 2);
    assert_eq!(riot_rt_gc_heap_len(), 2);
    riot_rt_root_pop(1);
    assert_eq!(riot_rt_gc_collect(), 2);
    assert_eq!(riot_rt_gc_heap_len(), 0);
}

#[test]
fn gc_traces_actor_frame_value_roots() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    let label = unsafe { riot_rt_value_string(b"frame-root".as_ptr(), 10) };
    let frame = riot_rt_alloc_frame_v2(16, 8, None);
    assert!(!frame.is_null());
    unsafe {
        ptr::write(frame.add(8) as *mut u64, label);
    }
    let actor_id = unsafe { riot_rt_spawn_actor_v2(frame, Some(idle_resume), 16, 8, None) };
    let offsets = [8usize];
    unsafe { riot_rt_actor_frame_roots(actor_id, offsets.as_ptr(), offsets.len()) };

    assert_eq!(riot_rt_gc_collect(), 0);
    assert_eq!(riot_rt_gc_heap_len(), 1);

    riot_rt_shutdown();
    assert_eq!(riot_rt_gc_heap_len(), 0);
}

#[test]
fn stale_actor_ids_are_terminated_tombstones_after_shutdown() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    let retired_before = with_scheduler_mut(|scheduler| scheduler.retired_actor_count());
    let frame = riot_rt_alloc_frame_v2(8, 8, None);
    assert!(!frame.is_null());
    let actor_id = unsafe { riot_rt_spawn_actor_v2(frame, Some(idle_resume), 8, 8, None) };
    assert!(!actor_id.is_null());

    riot_rt_shutdown();

    assert_eq!(
        with_scheduler_mut(|scheduler| scheduler.retired_actor_count()),
        retired_before + 1
    );
    let actor = unsafe { actor_from_id(actor_id) }.expect("retired actor slot remains valid");
    assert!(actor.is_terminated());
    riot_rt_send_i64(actor_id, 42);
    assert!(actor.current_message().is_none());
}

#[test]
fn actor_id_sends_push_directly_to_foreign_mailboxes() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    let frame = riot_rt_alloc_frame_v2(8, 8, None);
    assert!(!frame.is_null());
    let actor = ActorSlot::boxed(99, 1, frame, RtFrameLayout::new(8, 8, None), idle_resume);
    let actor_id = actor.actor_id();
    assert_eq!(actor.scheduler_id(), 99);

    with_scheduler_mut(|scheduler| scheduler.send(actor_id, RuntimeMessage::I64(42)));

    let message = actor
        .current_message()
        .expect("actor id send writes directly to the actor mailbox");
    assert_eq!(render_message(&message), "42");
    actor.terminate();
}

#[test]
fn value_equality_handles_nested_runtime_values() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    assert!(riot_rt_value_eq(riot_rt_value_unit(), riot_rt_value_unit()));
    assert!(riot_rt_value_eq(
        riot_rt_value_bool(true),
        riot_rt_value_bool(true)
    ));
    assert!(!riot_rt_value_eq(
        riot_rt_value_bool(true),
        riot_rt_value_bool(false)
    ));
    assert!(riot_rt_value_eq(
        riot_rt_value_i64(42),
        riot_rt_value_i64(42)
    ));
    assert!(!riot_rt_value_eq(
        riot_rt_value_i64(42),
        riot_rt_value_i64(7)
    ));

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

    let lhs_variant = unsafe { riot_rt_value_variant(b"color".as_ptr(), 5, b"Red".as_ptr(), 3) };
    let rhs_variant = unsafe { riot_rt_value_variant(b"color".as_ptr(), 5, b"Red".as_ptr(), 3) };
    let other_variant = unsafe { riot_rt_value_variant(b"color".as_ptr(), 5, b"Blue".as_ptr(), 4) };
    assert!(riot_rt_value_eq(lhs_variant, rhs_variant));
    assert!(!riot_rt_value_eq(lhs_variant, other_variant));

    let lhs_some = unsafe {
        riot_rt_value_variant_payload(
            b"option".as_ptr(),
            6,
            b"Some".as_ptr(),
            4,
            riot_rt_value_i64(1),
        )
    };
    let rhs_some = unsafe {
        riot_rt_value_variant_payload(
            b"option".as_ptr(),
            6,
            b"Some".as_ptr(),
            4,
            riot_rt_value_i64(1),
        )
    };
    let other_some = unsafe {
        riot_rt_value_variant_payload(
            b"option".as_ptr(),
            6,
            b"Some".as_ptr(),
            4,
            riot_rt_value_i64(2),
        )
    };
    assert!(riot_rt_value_eq(lhs_some, rhs_some));
    assert!(!riot_rt_value_eq(lhs_some, other_some));
}

#[test]
fn value_ordering_handles_i64_and_strings() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    assert!(crate::abi::riot_rt_value_lt(
        riot_rt_value_i64(1),
        riot_rt_value_i64(2)
    ));
    assert!(!crate::abi::riot_rt_value_lt(
        riot_rt_value_i64(2),
        riot_rt_value_i64(1)
    ));

    let alpha = unsafe { riot_rt_value_string(b"alpha".as_ptr(), 5) };
    let beta = unsafe { riot_rt_value_string(b"beta".as_ptr(), 4) };
    assert!(crate::abi::riot_rt_value_lt(alpha, beta));
    assert!(!crate::abi::riot_rt_value_lt(beta, alpha));

    let tuple_items = [alpha];
    let tuple = unsafe { riot_rt_value_tuple(tuple_items.as_ptr(), tuple_items.len()) };
    assert!(!crate::abi::riot_rt_value_lt(tuple, tuple));
}

#[test]
fn value_rendering_handles_compound_values() {
    let _guard = runtime_test_guard();
    riot_rt_init();

    let label = unsafe { riot_rt_value_string(b"riot".as_ptr(), 4) };
    let message = RuntimeMessage::I64(42).as_rt_message();
    assert!(riot_rt_value_eq(
        unsafe { riot_rt_msg_value(&message) },
        riot_rt_value_i64(42)
    ));
    let unit_message = RuntimeMessage::Unit.as_rt_message();
    assert!(riot_rt_value_eq(
        unsafe { riot_rt_msg_value(&unit_message) },
        riot_rt_value_unit()
    ));
    assert_eq!(render_message(&RuntimeMessage::Unit), "()");
    let values = [label, riot_rt_value_i64(42)];
    let tuple = unsafe { riot_rt_value_tuple(values.as_ptr(), values.len()) };
    assert_eq!(
        with_scheduler_mut(|scheduler| scheduler.render_value(tuple)),
        "(riot, 42)"
    );

    let record = unsafe { riot_rt_value_record_begin(b"Point".as_ptr(), 5, 2) };
    unsafe {
        riot_rt_value_record_set(record, 0, b"x".as_ptr(), 1, riot_rt_value_i64(10));
        riot_rt_value_record_set(record, 1, b"y".as_ptr(), 1, riot_rt_value_i64(20));
    }
    assert_eq!(
        with_scheduler_mut(|scheduler| scheduler.render_value(record)),
        "Point { x: 10, y: 20 }"
    );

    let x = unsafe { riot_rt_value_record_get(record, b"x".as_ptr(), 1) };
    assert!(riot_rt_value_eq(x, riot_rt_value_i64(10)));
    assert!(unsafe { riot_rt_value_record_is(record, b"Point".as_ptr(), 5) });
    assert!(!unsafe { riot_rt_value_record_is(record, b"Other".as_ptr(), 5) });

    assert!(riot_rt_value_eq(riot_rt_value_tuple_get(tuple, 0), label));
    assert!(riot_rt_value_eq(
        riot_rt_value_tuple_get(tuple, 1),
        riot_rt_value_i64(42)
    ));
    assert!(riot_rt_value_tuple_arity_is(tuple, 2));
    assert!(!riot_rt_value_tuple_arity_is(tuple, 3));

    let list_items = [label, riot_rt_value_i64(7)];
    let list = unsafe { riot_rt_value_list(list_items.as_ptr(), list_items.len()) };
    assert_eq!(riot_rt_value_list_len(list), 2);
    assert!(riot_rt_value_eq(riot_rt_value_list_get(list, 0), label));
    assert!(riot_rt_value_eq(
        riot_rt_value_list_get(list, 1),
        riot_rt_value_i64(7)
    ));

    let suffix = unsafe { riot_rt_value_string(b" lang".as_ptr(), 5) };
    let concatenated = riot_rt_value_string_concat(label, suffix);
    assert_eq!(riot_rt_value_string_len(label), 4);
    assert_eq!(riot_rt_value_string_len(concatenated), 9);
    assert_eq!(
        with_scheduler_mut(|scheduler| scheduler.render_value(concatenated)),
        "riot lang"
    );

    let variant = unsafe { riot_rt_value_variant(b"color".as_ptr(), 5, b"Green".as_ptr(), 5) };
    assert_eq!(
        with_scheduler_mut(|scheduler| scheduler.render_value(variant)),
        "Green"
    );

    let some = unsafe {
        riot_rt_value_variant_payload(
            b"option".as_ptr(),
            6,
            b"Some".as_ptr(),
            4,
            riot_rt_value_i64(42),
        )
    };
    assert_eq!(
        with_scheduler_mut(|scheduler| scheduler.render_value(some)),
        "Some(42)"
    );
    assert!(unsafe { riot_rt_value_variant_is(some, b"option".as_ptr(), 6, b"Some".as_ptr(), 4) });
    assert!(riot_rt_value_eq(
        riot_rt_value_variant_get_payload(some),
        riot_rt_value_i64(42)
    ));

    let pair_payload = unsafe { riot_rt_value_tuple(values.as_ptr(), values.len()) };
    let pair = unsafe {
        riot_rt_value_variant_payload(b"event".as_ptr(), 5, b"Pair".as_ptr(), 4, pair_payload)
    };
    assert_eq!(
        with_scheduler_mut(|scheduler| scheduler.render_value(pair)),
        "Pair(riot, 42)"
    );
}
