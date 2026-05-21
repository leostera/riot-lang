use std::ffi::{CStr, c_char};
use std::slice;

use crate::actor::{ActorResumeFn, RtMessage, RuntimeMessage, runtime_message_from_raw};
use crate::actor_id::ActorId;
use crate::frame::{ActorDropFn, RtFrameLayout, alloc_frame, free_frame};
use crate::io::{
    bytes_from_raw, usize_from_raw, values_from_raw, write_bytes_line, write_stdout_line,
};
use crate::scheduler::{
    SchedulerLocal, current_actor_id, render_message, runtime_abort, with_scheduler_mut,
};
use crate::value::{
    ClosureApplyFn, HeapObjectKind, HeapOwner, RtValue, VALUE_UNIT, heap_index, value_actor_id,
    value_actor_id_payload, value_bool, value_bool_payload, value_i64, value_i64_payload,
};

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_init() {
    with_scheduler_mut(SchedulerLocal::reset);
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_shutdown() {
    with_scheduler_mut(|scheduler| scheduler.shutdown());
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
pub extern "C" fn riot_rt_value_as_i64(value: RtValue) -> i64 {
    value_i64_payload(value).unwrap_or_else(|| runtime_abort("expected an i64 value"))
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_bool(value: bool) -> RtValue {
    value_bool(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_as_bool(value: RtValue) -> bool {
    value_bool_payload(value).unwrap_or_else(|| runtime_abort("expected a bool value"))
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_actor_id(value: ActorId) -> RtValue {
    value_actor_id(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_as_actor_id(value: RtValue) -> ActorId {
    value_actor_id_payload(value).unwrap_or_else(|| runtime_abort("expected an actor id value"))
}

fn prim_i64(value: RtValue, name: &str) -> i64 {
    value_i64_payload(value).unwrap_or_else(|| runtime_abort(name))
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_add(lhs: RtValue, rhs: RtValue) -> RtValue {
    value_i64(
        prim_i64(lhs, "add expected an i64 lhs")
            .wrapping_add(prim_i64(rhs, "add expected an i64 rhs")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_sub(lhs: RtValue, rhs: RtValue) -> RtValue {
    value_i64(
        prim_i64(lhs, "sub expected an i64 lhs")
            .wrapping_sub(prim_i64(rhs, "sub expected an i64 rhs")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_neg(value: RtValue) -> RtValue {
    value_i64(prim_i64(value, "neg expected an i64 value").wrapping_neg())
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_mul(lhs: RtValue, rhs: RtValue) -> RtValue {
    value_i64(
        prim_i64(lhs, "mul expected an i64 lhs")
            .wrapping_mul(prim_i64(rhs, "mul expected an i64 rhs")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_div(lhs: RtValue, rhs: RtValue) -> RtValue {
    let rhs = prim_i64(rhs, "div expected an i64 rhs");
    if rhs == 0 {
        runtime_abort("division by zero");
    }
    value_i64(
        prim_i64(lhs, "div expected an i64 lhs")
            .checked_div(rhs)
            .unwrap_or_else(|| runtime_abort("integer division overflow")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_mod(lhs: RtValue, rhs: RtValue) -> RtValue {
    let rhs = prim_i64(rhs, "mod expected an i64 rhs");
    if rhs == 0 {
        runtime_abort("modulo by zero");
    }
    value_i64(
        prim_i64(lhs, "mod expected an i64 lhs")
            .checked_rem(rhs)
            .unwrap_or_else(|| runtime_abort("integer modulo overflow")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_eq(lhs: RtValue, rhs: RtValue) -> bool {
    riot_rt_value_eq(lhs, rhs)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_lt(lhs: RtValue, rhs: RtValue) -> bool {
    riot_rt_value_lt(lhs, rhs)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_and(lhs: bool, rhs: bool) -> bool {
    lhs && rhs
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_or(lhs: bool, rhs: bool) -> bool {
    lhs || rhs
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_prim_not(value: bool) -> bool {
    !value
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_string(ptr: *const u8, len: usize) -> RtValue {
    let Some(bytes) = (unsafe { bytes_from_raw(ptr, len) }) else {
        runtime_abort("string value received an invalid pointer/length pair");
    };
    with_scheduler_mut(|scheduler| {
        scheduler.alloc_value(
            HeapObjectKind::String(bytes.to_vec()),
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_variant(
    type_ptr: *const u8,
    type_len: usize,
    constructor_ptr: *const u8,
    constructor_len: usize,
) -> RtValue {
    unsafe {
        riot_rt_value_variant_payload(
            type_ptr,
            type_len,
            constructor_ptr,
            constructor_len,
            VALUE_UNIT,
        )
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_variant_payload(
    type_ptr: *const u8,
    type_len: usize,
    constructor_ptr: *const u8,
    constructor_len: usize,
    payload: RtValue,
) -> RtValue {
    let Some(type_name) = (unsafe { bytes_from_raw(type_ptr, type_len) }) else {
        runtime_abort("variant value received an invalid type pointer/length pair");
    };
    let Some(constructor) = (unsafe { bytes_from_raw(constructor_ptr, constructor_len) }) else {
        runtime_abort("variant value received an invalid constructor pointer/length pair");
    };
    let type_name = String::from_utf8_lossy(type_name).into_owned();
    let constructor = String::from_utf8_lossy(constructor).into_owned();
    with_scheduler_mut(|scheduler| {
        scheduler.alloc_value(
            HeapObjectKind::Variant {
                type_name,
                constructor,
                payload,
            },
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_tuple(values: *const RtValue, len: usize) -> RtValue {
    let Some(values) = (unsafe { values_from_raw(values, len) }) else {
        runtime_abort("tuple value received an invalid pointer/length pair");
    };
    with_scheduler_mut(|scheduler| {
        scheduler.alloc_value(
            HeapObjectKind::Tuple(values.to_vec()),
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_list(values: *const RtValue, len: usize) -> RtValue {
    let Some(values) = (unsafe { values_from_raw(values, len) }) else {
        runtime_abort("list value received an invalid pointer/length pair");
    };
    with_scheduler_mut(|scheduler| {
        scheduler.alloc_value(
            HeapObjectKind::List(values.to_vec()),
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_argv(argc: i32, argv: *const *const c_char) -> RtValue {
    if argc < 0 {
        runtime_abort("argv received a negative argc");
    }
    let len = argc as usize;
    if len > 0 && argv.is_null() {
        runtime_abort("argv received a null argv pointer");
    }
    let raw_args: &[*const c_char] = if len == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(argv, len) }
    };
    with_scheduler_mut(|scheduler| {
        let mut values = Vec::with_capacity(len);
        for raw_arg in raw_args {
            if raw_arg.is_null() {
                runtime_abort("argv received a null argument pointer");
            }
            let bytes = unsafe { CStr::from_ptr(*raw_arg).to_bytes() };
            let value = scheduler.alloc_value(
                HeapObjectKind::String(bytes.to_vec()),
                HeapOwner::Local(current_actor_id()),
            );
            scheduler.push_root(value);
            values.push(value);
        }
        let list = scheduler.alloc_value(
            HeapObjectKind::List(values),
            HeapOwner::Local(current_actor_id()),
        );
        scheduler.pop_roots(len);
        list
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_closure(
    apply: Option<ClosureApplyFn>,
    captures: *const RtValue,
    len: usize,
) -> RtValue {
    let Some(apply) = apply else {
        runtime_abort("closure value received a null apply function");
    };
    let Some(captures) = (unsafe { values_from_raw(captures, len) }) else {
        runtime_abort("closure value received an invalid captures pointer/length pair");
    };
    with_scheduler_mut(|scheduler| {
        scheduler.alloc_value(
            HeapObjectKind::Closure {
                apply,
                captures: captures.to_vec(),
            },
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_apply(closure: RtValue, argument: RtValue) -> RtValue {
    let (apply, captures) = with_scheduler_mut(|scheduler| {
        scheduler
            .closure_parts(closure)
            .unwrap_or_else(|| runtime_abort("apply expected a closure value"))
    });
    unsafe { apply(captures.as_ptr(), captures.len(), argument) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_record_begin(
    path_ptr: *const u8,
    path_len: usize,
    field_count: usize,
) -> RtValue {
    let Some(path) = (unsafe { bytes_from_raw(path_ptr, path_len) }) else {
        runtime_abort("record value received an invalid path pointer/length pair");
    };
    let path = String::from_utf8_lossy(path).into_owned();
    with_scheduler_mut(|scheduler| {
        scheduler.alloc_value(
            HeapObjectKind::Record {
                path,
                fields: vec![("".to_owned(), VALUE_UNIT); field_count],
            },
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_record_set(
    record: RtValue,
    field_index: usize,
    name_ptr: *const u8,
    name_len: usize,
    value: RtValue,
) {
    let Some(name) = (unsafe { bytes_from_raw(name_ptr, name_len) }) else {
        runtime_abort("record field set received an invalid name pointer/length pair");
    };
    let name = String::from_utf8_lossy(name).into_owned();
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(record) else {
            runtime_abort("record field set expected a record value");
        };
        let Some(object) = scheduler.heap.get_mut(heap_index).and_then(Option::as_mut) else {
            runtime_abort("record field set received a stale record value");
        };
        let HeapObjectKind::Record { fields, .. } = &mut object.kind else {
            runtime_abort("record field set expected a record value");
        };
        let Some(field) = fields.get_mut(field_index) else {
            runtime_abort("record field set index out of bounds");
        };
        *field = (name, value);
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_string_len(string: RtValue) -> i64 {
    with_scheduler_mut(|scheduler| {
        scheduler.value_bytes(string).map_or_else(
            || runtime_abort("string_len expected a string value"),
            |bytes| bytes.len() as i64,
        )
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_string_concat(lhs: RtValue, rhs: RtValue) -> RtValue {
    with_scheduler_mut(|scheduler| {
        let Some(lhs_bytes) = scheduler.value_bytes(lhs) else {
            runtime_abort("string_concat left argument was not a string value");
        };
        let Some(rhs_bytes) = scheduler.value_bytes(rhs) else {
            runtime_abort("string_concat right argument was not a string value");
        };
        let mut bytes = Vec::with_capacity(lhs_bytes.len() + rhs_bytes.len());
        bytes.extend_from_slice(lhs_bytes);
        bytes.extend_from_slice(rhs_bytes);
        scheduler.alloc_value(
            HeapObjectKind::String(bytes),
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_list_cons(item: RtValue, list: RtValue) -> RtValue {
    with_scheduler_mut(|scheduler| {
        let Some(index) = heap_index(list) else {
            runtime_abort("list_cons expected a list value");
        };
        let Some(object) = scheduler.heap.get(index).and_then(Option::as_ref) else {
            runtime_abort("list_cons received a stale list value");
        };
        let mut items = match &object.kind {
            HeapObjectKind::List(items) => Vec::with_capacity(items.len() + 1),
            _ => runtime_abort("list_cons expected a list value"),
        };
        items.push(item);
        if let HeapObjectKind::List(tail) = &object.kind {
            items.extend_from_slice(tail);
        }
        scheduler.alloc_value(
            HeapObjectKind::List(items),
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_list_len(list: RtValue) -> i64 {
    with_scheduler_mut(|scheduler| {
        let Some(index) = heap_index(list) else {
            runtime_abort("list_len expected a list value");
        };
        let Some(object) = scheduler.heap.get(index).and_then(Option::as_ref) else {
            runtime_abort("list_len received a stale list value");
        };
        match &object.kind {
            HeapObjectKind::List(items) => items.len() as i64,
            _ => runtime_abort("list_len expected a list value"),
        }
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_list_get(list: RtValue, index: i64) -> RtValue {
    if index < 0 {
        runtime_abort("list_get received a negative index");
    }
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(list) else {
            runtime_abort("list_get expected a list value");
        };
        let Some(object) = scheduler.heap.get(heap_index).and_then(Option::as_ref) else {
            runtime_abort("list_get received a stale list value");
        };
        match &object.kind {
            HeapObjectKind::List(items) => items
                .get(index as usize)
                .copied()
                .unwrap_or_else(|| runtime_abort("list_get index out of bounds")),
            _ => runtime_abort("list_get expected a list value"),
        }
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_list_drop(list: RtValue, count: i64) -> RtValue {
    if count < 0 {
        runtime_abort("list_drop received a negative count");
    }
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(list) else {
            runtime_abort("list_drop expected a list value");
        };
        let Some(object) = scheduler.heap.get(heap_index).and_then(Option::as_ref) else {
            runtime_abort("list_drop received a stale list value");
        };
        let items = match &object.kind {
            HeapObjectKind::List(items) => items,
            _ => runtime_abort("list_drop expected a list value"),
        };
        let count = count as usize;
        if count > items.len() {
            runtime_abort("list_drop count out of bounds");
        }
        scheduler.alloc_value(
            HeapObjectKind::List(items[count..].to_vec()),
            HeapOwner::Local(current_actor_id()),
        )
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_tuple_get(tuple: RtValue, index: usize) -> RtValue {
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(tuple) else {
            runtime_abort("tuple projection expected a tuple value");
        };
        let Some(object) = scheduler.heap.get(heap_index).and_then(Option::as_ref) else {
            runtime_abort("tuple projection received a stale tuple value");
        };
        match &object.kind {
            HeapObjectKind::Tuple(items) => items
                .get(index)
                .copied()
                .unwrap_or_else(|| runtime_abort("tuple projection index out of bounds")),
            _ => runtime_abort("tuple projection expected a tuple value"),
        }
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_tuple_arity_is(tuple: RtValue, len: usize) -> bool {
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(tuple) else {
            return false;
        };
        let Some(object) = scheduler.heap.get(heap_index).and_then(Option::as_ref) else {
            return false;
        };
        match &object.kind {
            HeapObjectKind::Tuple(items) => items.len() == len,
            _ => false,
        }
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_record_get(
    record: RtValue,
    name_ptr: *const u8,
    name_len: usize,
) -> RtValue {
    let Some(name) = (unsafe { bytes_from_raw(name_ptr, name_len) }) else {
        runtime_abort("record field access received an invalid name pointer/length pair");
    };
    let name = String::from_utf8_lossy(name);
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(record) else {
            runtime_abort("record field access expected a record value");
        };
        let Some(object) = scheduler.heap.get(heap_index).and_then(Option::as_ref) else {
            runtime_abort("record field access received a stale record value");
        };
        match &object.kind {
            HeapObjectKind::Record { fields, .. } => fields
                .iter()
                .find_map(|(field_name, value)| (field_name == name.as_ref()).then_some(*value))
                .unwrap_or_else(|| runtime_abort("record field access found no matching field")),
            _ => runtime_abort("record field access expected a record value"),
        }
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_record_is(
    record: RtValue,
    path_ptr: *const u8,
    path_len: usize,
) -> bool {
    let Some(path) = (unsafe { bytes_from_raw(path_ptr, path_len) }) else {
        runtime_abort("record tag test received an invalid path pointer/length pair");
    };
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(record) else {
            return false;
        };
        let Some(object) = scheduler.heap.get(heap_index).and_then(Option::as_ref) else {
            return false;
        };
        matches!(&object.kind, HeapObjectKind::Record { path: actual_path, .. } if actual_path.as_bytes() == path)
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_bytes_ptr(value: RtValue) -> *const u8 {
    with_scheduler_mut(|scheduler| {
        scheduler.value_bytes(value).map_or_else(
            || runtime_abort("bytes_ptr expected a string value"),
            |bytes| bytes.as_ptr(),
        )
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_bytes_len(value: RtValue) -> usize {
    with_scheduler_mut(|scheduler| {
        scheduler.value_bytes(value).map_or_else(
            || runtime_abort("bytes_len expected a string value"),
            <[u8]>::len,
        )
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_eq(lhs: RtValue, rhs: RtValue) -> bool {
    with_scheduler_mut(|scheduler| scheduler.values_equal(lhs, rhs))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_value_variant_is(
    value: RtValue,
    type_ptr: *const u8,
    type_len: usize,
    constructor_ptr: *const u8,
    constructor_len: usize,
) -> bool {
    let Some(type_name) = (unsafe { bytes_from_raw(type_ptr, type_len) }) else {
        runtime_abort("variant tag test received an invalid type pointer/length pair");
    };
    let Some(constructor) = (unsafe { bytes_from_raw(constructor_ptr, constructor_len) }) else {
        runtime_abort("variant tag test received an invalid constructor pointer/length pair");
    };
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(value) else {
            return false;
        };
        let Some(object) = scheduler.heap.get(heap_index).and_then(Option::as_ref) else {
            return false;
        };
        matches!(
            &object.kind,
            HeapObjectKind::Variant {
                type_name: actual_type,
                constructor: actual_constructor,
                ..
            } if actual_type.as_bytes() == type_name
                && actual_constructor.as_bytes() == constructor
        )
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_variant_get_payload(value: RtValue) -> RtValue {
    with_scheduler_mut(|scheduler| {
        let Some(heap_index) = heap_index(value) else {
            runtime_abort("variant payload expected a variant value");
        };
        let Some(object) = scheduler.heap.get(heap_index).and_then(Option::as_ref) else {
            runtime_abort("variant payload received a stale variant value");
        };
        match &object.kind {
            HeapObjectKind::Variant { payload, .. } => *payload,
            _ => runtime_abort("variant payload expected a variant value"),
        }
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_option_is_some(value: RtValue) -> bool {
    unsafe { riot_rt_value_variant_is(value, b"Option".as_ptr(), 6, b"Some".as_ptr(), 4) }
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_option_is_none(value: RtValue) -> bool {
    unsafe { riot_rt_value_variant_is(value, b"Option".as_ptr(), 6, b"None".as_ptr(), 4) }
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_option_unwrap_or(value: RtValue, fallback: RtValue) -> RtValue {
    if riot_rt_option_is_some(value) {
        riot_rt_value_variant_get_payload(value)
    } else if riot_rt_option_is_none(value) {
        fallback
    } else {
        runtime_abort("Option.unwrap_or expected an option value");
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_result_is_ok(value: RtValue) -> bool {
    unsafe { riot_rt_value_variant_is(value, b"Result".as_ptr(), 6, b"Ok".as_ptr(), 2) }
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_result_is_err(value: RtValue) -> bool {
    unsafe { riot_rt_value_variant_is(value, b"Result".as_ptr(), 6, b"Err".as_ptr(), 3) }
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_result_unwrap_or(value: RtValue, fallback: RtValue) -> RtValue {
    if riot_rt_result_is_ok(value) {
        riot_rt_value_variant_get_payload(value)
    } else if riot_rt_result_is_err(value) {
        fallback
    } else {
        runtime_abort("Result.unwrap_or expected a result value");
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_result_exit_code(value: RtValue) -> i32 {
    if riot_rt_result_is_ok(value) {
        return 0;
    }
    if riot_rt_result_is_err(value) {
        let payload = riot_rt_value_variant_get_payload(value);
        return value_i64_payload(payload).unwrap_or_else(|| {
            runtime_abort("Result error exit code must be an i32-compatible integer")
        }) as i32;
    }
    runtime_abort("main returned a value that is not a Result");
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_value_lt(lhs: RtValue, rhs: RtValue) -> bool {
    with_scheduler_mut(|scheduler| scheduler.values_less_than(lhs, rhs))
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_println_value(value: RtValue) {
    let rendered = with_scheduler_mut(|scheduler| scheduler.render_value(value));
    write_bytes_line(rendered.as_bytes());
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_dbg_value(value: RtValue) {
    riot_rt_println_value(value);
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_gc_collect() -> usize {
    with_scheduler_mut(SchedulerLocal::collect_garbage)
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_gc_heap_len() -> usize {
    with_scheduler_mut(|scheduler| scheduler.heap_len())
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_gc_set_threshold(threshold: usize) {
    with_scheduler_mut(|scheduler| scheduler.set_gc_threshold(threshold));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_gc_collection_count() -> usize {
    with_scheduler_mut(|scheduler| scheduler.gc_collection_count())
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_root_push(value: RtValue) {
    with_scheduler_mut(|scheduler| scheduler.push_root(value));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_root_pop(count: usize) {
    with_scheduler_mut(|scheduler| scheduler.pop_roots(count));
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
    alloc_frame(size, align)
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
pub unsafe extern "C" fn riot_rt_spawn_actor(
    frame: *mut u8,
    resume: Option<ActorResumeFn>,
) -> ActorId {
    let Some(resume) = resume else {
        return ActorId::NULL;
    };
    with_scheduler_mut(|scheduler| scheduler.spawn(frame, resume, RtFrameLayout::legacy()))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_spawn_actor_v2(
    frame: *mut u8,
    resume: Option<ActorResumeFn>,
    size: usize,
    align: usize,
    drop_fn: Option<ActorDropFn>,
) -> ActorId {
    let Some(resume) = resume else {
        return ActorId::NULL;
    };
    with_scheduler_mut(|scheduler| {
        scheduler.spawn(frame, resume, RtFrameLayout::new(size, align, drop_fn))
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_actor_frame_roots(
    actor_id: ActorId,
    offsets: *const usize,
    len: usize,
) {
    let Some(offsets) = (unsafe { usize_from_raw(offsets, len) }) else {
        runtime_abort("actor frame roots received an invalid pointer/length pair");
    };
    with_scheduler_mut(|scheduler| scheduler.register_frame_roots(actor_id, offsets));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_send(actor_id: ActorId, ptr: *const u8, len: usize) {
    let Some(bytes) = (unsafe { bytes_from_raw(ptr, len) }) else {
        runtime_abort("send received an invalid pointer/length pair");
    };

    with_scheduler_mut(|scheduler| scheduler.send(actor_id, RuntimeMessage::Bytes(bytes.to_vec())));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_i64(actor_id: ActorId, value: i64) {
    with_scheduler_mut(|scheduler| scheduler.send(actor_id, RuntimeMessage::I64(value)));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_bool(actor_id: ActorId, value: bool) {
    with_scheduler_mut(|scheduler| scheduler.send(actor_id, RuntimeMessage::Bool(value)));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_unit(actor_id: ActorId) {
    with_scheduler_mut(|scheduler| scheduler.send(actor_id, RuntimeMessage::Unit));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_actor_id(actor_id: ActorId, value: ActorId) {
    with_scheduler_mut(|scheduler| scheduler.send(actor_id, RuntimeMessage::ActorId(value)));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_send_value(actor_id: ActorId, value: RtValue) {
    with_scheduler_mut(|scheduler| {
        scheduler.promote_shared(value);
        scheduler.send(actor_id, RuntimeMessage::Value(value));
    });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_send_msg(actor_id: ActorId, message: *const RtMessage) {
    let Some(message) = (unsafe { runtime_message_from_raw(message) }) else {
        return;
    };

    with_scheduler_mut(|scheduler| scheduler.send(actor_id, message));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_dbg_msg(message: *const RtMessage) {
    let Some(message) = (unsafe { runtime_message_from_raw(message) }) else {
        return;
    };

    write_bytes_line(render_message(&message).as_bytes());
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_msg_value(message: *const RtMessage) -> RtValue {
    let Some(message) = (unsafe { runtime_message_from_raw(message) }) else {
        runtime_abort("message value access received an invalid message pointer");
    };
    match message {
        RuntimeMessage::Bytes(bytes) => with_scheduler_mut(|scheduler| {
            scheduler.alloc_value(
                HeapObjectKind::String(bytes),
                HeapOwner::Local(current_actor_id()),
            )
        }),
        RuntimeMessage::I64(value) => value_i64(value),
        RuntimeMessage::Bool(value) => value_bool(value),
        RuntimeMessage::Unit => VALUE_UNIT,
        RuntimeMessage::ActorId(value) => value_actor_id(value),
        RuntimeMessage::Value(value) => value,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_monitor(actor_id: ActorId) {
    with_scheduler_mut(|scheduler| scheduler.monitor(actor_id));
}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_link(actor_id: ActorId) {
    with_scheduler_mut(|scheduler| scheduler.link(actor_id));
}
