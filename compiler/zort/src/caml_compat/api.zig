const std = @import("std");
pub const build_options = @import("build_options");
const compat = @import("codec.zig");
const primitive_registry = @import("../primitive_registry.zig");
const runtime = @import("../runtime.zig");
const value = @import("../value.zig");

/// Legacy shim only.
/// Prefer the idiomatic API from `lib.zig` (`Runtime`, `Value`, `Tag`).
/// This file is the compatibility boundary only.
pub const CompatValue = compat.CompatValue;
pub const Tag = value.Tag;
pub const Runtime = runtime.Runtime;
pub const Error = compat.Error || primitive_registry.Error;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

const ShimState = struct {
    runtime: Runtime,
    handles: compat.HandleTable,
    primitives: primitive_registry.PrimitiveRegistry,

    fn init(allocator: std.mem.Allocator) !ShimState {
        var state = ShimState{
            .runtime = Runtime.init(allocator),
            .handles = compat.HandleTable.init(allocator),
            .primitives = primitive_registry.PrimitiveRegistry.init(allocator),
        };
        try state.installBuiltinPrimitives();
        return state;
    }

    fn deinit(self: *ShimState) void {
        self.handles.deinit(&self.runtime);
        self.primitives.deinit();
        self.runtime.deinit();
    }

    fn installBuiltinPrimitives(self: *ShimState) !void {
        try self.primitives.register("zort.add_i64", 2, primitiveAddI64);
        try self.primitives.register("zort.identity", 1, primitiveIdentity);
    }
};

var global_state: ?ShimState = null;

fn stateRef() *ShimState {
    if (global_state == null) {
        global_state = ShimState.init(gpa.allocator()) catch oom();
    }
    return &global_state.?;
}

fn oom() noreturn {
    @panic("zort legacy API out of memory");
}

fn encode(internal_value: value.Value) CompatValue {
    const state = stateRef();
    return state.handles.encodeValue(&state.runtime, internal_value) catch oom();
}

fn decode(raw: CompatValue) value.Value {
    return stateRef().handles.decodeValue(raw) catch oom();
}

pub fn caml_alloc(size: usize, tag: Tag) CompatValue {
    return encode(stateRef().runtime.alloc(size, tag) catch oom());
}

pub fn caml_alloc_tuple(size: usize) CompatValue {
    return encode(stateRef().runtime.allocTuple(size) catch oom());
}

pub fn caml_alloc_string(len: usize) CompatValue {
    return encode(stateRef().runtime.allocStringWithFill(len, 0) catch oom());
}

pub fn caml_set_field(raw_value: CompatValue, index: usize, raw_item: CompatValue) void {
    stateRef().runtime.setField(decode(raw_value), index, decode(raw_item)) catch oom();
}

pub fn caml_field(raw_value: CompatValue, index: usize) CompatValue {
    return encode(stateRef().runtime.field(decode(raw_value), index) catch oom());
}

pub fn caml_string_length(raw_value: CompatValue) usize {
    return stateRef().runtime.stringLength(decode(raw_value)) catch oom();
}

pub fn caml_string_contents(raw_value: CompatValue) []const u8 {
    return stateRef().runtime.stringSlice(decode(raw_value)) catch oom();
}

pub fn caml_release_value(raw_value: CompatValue) void {
    stateRef().handles.releaseHandle(&stateRef().runtime, raw_value) catch {};
}

pub fn caml_gc_collect() void {
    stateRef().runtime.collect();
}

pub fn caml_gc_get_heap_size() usize {
    return stateRef().runtime.objectCount();
}

pub fn caml_gc_init() void {}

fn primitiveCall0(name: []const u8) CompatValue {
    const state = stateRef();
    return encode(state.primitives.callWithBoundary(&state.runtime, name, &.{}) catch oom());
}

fn primitiveCall1(name: []const u8, arg0: CompatValue) CompatValue {
    const state = stateRef();
    return encode(state.primitives.callWithBoundary(&state.runtime, name, &.{decode(arg0)}) catch oom());
}

fn primitiveCall2(name: []const u8, arg0: CompatValue, arg1: CompatValue) CompatValue {
    const state = stateRef();
    return encode(state.primitives.callWithBoundary(&state.runtime, name, &.{ decode(arg0), decode(arg1) }) catch oom());
}

pub fn zort_primitive_call0(name_ptr: [*]const u8, name_len: usize) CompatValue {
    return primitiveCall0(name_ptr[0..name_len]);
}

pub fn zort_primitive_call1(name_ptr: [*]const u8, name_len: usize, arg0: CompatValue) CompatValue {
    return primitiveCall1(name_ptr[0..name_len], arg0);
}

pub fn zort_primitive_call2(name_ptr: [*]const u8, name_len: usize, arg0: CompatValue, arg1: CompatValue) CompatValue {
    return primitiveCall2(name_ptr[0..name_len], arg0, arg1);
}

pub fn caml_shutdown() void {
    if (global_state) |*state| {
        state.deinit();
    }
    global_state = null;
}

fn primitiveAddI64(rt: *Runtime, args: []const value.Value) primitive_registry.Error!value.Value {
    return rt.allocI64((try rt.unboxI64(args[0])) + (try rt.unboxI64(args[1])));
}

fn primitiveIdentity(_: *Runtime, args: []const value.Value) primitive_registry.Error!value.Value {
    return args[0];
}

test "api: compat handles keep block values alive until released" {
    defer caml_shutdown();

    const tuple = caml_alloc_tuple(1);
    caml_gc_collect();
    try std.testing.expect(caml_gc_get_heap_size() >= 1);

    caml_release_value(tuple);
    caml_gc_collect();
    try std.testing.expectEqual(@as(usize, 0), caml_gc_get_heap_size());
}

test "api: typed primitive calls use compat boundary values" {
    defer caml_shutdown();

    const left = encode(stateRef().runtime.allocI64(20) catch oom());
    const right = encode(stateRef().runtime.allocI64(22) catch oom());
    const name = "zort.add_i64";
    const result = zort_primitive_call2(name.ptr, name.len, left, right);

    const decoded = decode(result);
    try std.testing.expectEqual(@as(i64, 42), try stateRef().runtime.unboxI64(decoded));
    caml_release_value(left);
    caml_release_value(right);
    caml_release_value(result);
}
