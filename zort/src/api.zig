const std = @import("std");
const runtime = @import("runtime.zig");
const value = @import("value.zig");

/// Legacy shim only.
/// Prefer the idiomatic API from `lib.zig` (`Runtime`, `Value`, `Tag`).
pub const Value = value.Value;
pub const Tag = value.Tag;
pub const Runtime = runtime.Runtime;
pub const Error = runtime.Error;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var global_runtime = Runtime.init(gpa.allocator());

fn runtimeRef() *Runtime {
    return &global_runtime;
}

fn oom() noreturn {
    @panic("zort legacy API out of memory");
}

pub fn caml_alloc(size: usize, tag: Tag) Value {
    return runtimeRef().alloc(size, tag) catch oom();
}

pub fn caml_alloc_tuple(size: usize) Value {
    return runtimeRef().allocTuple(size) catch oom();
}

pub fn caml_alloc_string(len: usize) Value {
    return runtimeRef().alloc(len, .string) catch oom();
}

pub fn caml_set_field(value: Value, index: usize, item: Value) void {
    runtimeRef().setField(value, index, item) catch oom();
}

pub fn caml_field(value: Value, index: usize) Value {
    return runtimeRef().field(value, index) catch oom();
}

pub fn caml_string_length(value: Value) usize {
    return runtimeRef().stringLength(value) catch oom();
}

pub fn caml_string_contents(value: Value) []const u8 {
    return runtimeRef().stringSlice(value) catch oom();
}

pub fn caml_gc_collect() void {
    runtimeRef().collect();
}

pub fn caml_gc_get_heap_size() usize {
    return runtimeRef().objectCount();
}

pub fn caml_gc_init() void {}

pub fn caml_shutdown() void {
    runtimeRef().deinit();
    global_runtime = Runtime.init(gpa.allocator());
}
