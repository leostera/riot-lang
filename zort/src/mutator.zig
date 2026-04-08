const std = @import("std");
const event_sink = @import("event_sink.zig");
const heap_store = @import("heap_store.zig");
const value = @import("value.zig");

pub const Error = error{
    OutOfMemory,
    InvalidValue,
};

pub const Value = value.Value;
pub const Tag = value.Tag;
pub const HeapRef = value.HeapRef;
pub const HeapStore = heap_store.HeapStore;
pub const Object = heap_store.Object;
pub const EventSink = event_sink.EventSink;

const StorePhase = enum {
    initialize,
    mutate,
};

pub const Mutator = struct {
    allocator: std.mem.Allocator,
    heap_store: *HeapStore,
    event_sink: EventSink,

    pub fn init(allocator: std.mem.Allocator, store: *HeapStore, sink: EventSink) Mutator {
        return .{
            .allocator = allocator,
            .heap_store = store,
            .event_sink = sink,
        };
    }

    pub fn allocCompat(self: *Mutator, arity: usize, tag: Tag) Error!Value {
        return switch (tag) {
            .tuple => self.allocTuple(arity),
            .string => self.allocStringLen(arity),
            .int64 => self.allocBoxedI64(0),
            .double => self.allocBoxedF64(0),
            .custom => self.allocCustomBytes(arity),
        };
    }

    pub fn allocTuple(self: *Mutator, len: usize) Error!Value {
        const fields = if (len == 0)
            @constCast(&[_]Value{})
        else blk: {
            const allocated = try self.allocator.alloc(Value, len);
            @memset(allocated, Value.fromInt(0));
            break :blk allocated;
        };
        return self.insertObject(Object.initTuple(fields));
    }

    pub fn allocStringLen(self: *Mutator, len: usize) Error!Value {
        const bytes = try self.allocator.alloc(u8, len + 1);
        @memset(bytes, 0);
        return self.insertObject(Object.initString(len, bytes));
    }

    pub fn allocBoxedI64(self: *Mutator, number: i64) Error!Value {
        return self.insertObject(Object.initBoxedI64(number));
    }

    pub fn allocBoxedF64(self: *Mutator, number: f64) Error!Value {
        return self.insertObject(Object.initBoxedF64(number));
    }

    pub fn allocCustomBytes(self: *Mutator, len: usize) Error!Value {
        const bytes = if (len == 0)
            @constCast(&[_]u8{})
        else blk: {
            const allocated = try self.allocator.alloc(u8, len);
            @memset(allocated, 0);
            break :blk allocated;
        };
        return self.insertObject(Object.initCustom(bytes));
    }

    pub fn initTupleFromSlice(self: *Mutator, tuple_value: Value, fields: []const Value) Error!void {
        var i: usize = 0;
        while (i < fields.len) : (i += 1) {
            try self.storeField(tuple_value, i, fields[i], .initialize);
        }
    }

    pub fn initField(self: *Mutator, tuple_value: Value, index: usize, next: Value) Error!void {
        try self.storeField(tuple_value, index, next, .initialize);
    }

    pub fn writeField(self: *Mutator, tuple_value: Value, index: usize, next: Value) Error!void {
        try self.storeField(tuple_value, index, next, .mutate);
    }

    pub fn fillString(self: *Mutator, string_value: Value, fill: u8) Error!void {
        const handle = string_value.asHeapRef() orelse return Error.InvalidValue;
        const buffer = try self.stringBufferForWrite(string_value);
        if (buffer.len > 1) {
            @memset(buffer[0 .. buffer.len - 1], fill);
        }
        buffer[buffer.len - 1] = 0;
        self.event_sink.emit(.{ .bytes_write = .{
            .target = handle,
            .len = if (buffer.len > 0) buffer.len - 1 else 0,
            .phase = .initialize,
        } });
    }

    pub fn initStringBytes(self: *Mutator, string_value: Value, bytes: []const u8) Error!void {
        try self.storeStringBytes(string_value, bytes, .initialize);
    }

    pub fn writeStringBytes(self: *Mutator, string_value: Value, bytes: []const u8) Error!void {
        try self.storeStringBytes(string_value, bytes, .mutate);
    }

    fn insertObject(self: *Mutator, object: Object) Error!Value {
        const handle = try self.heap_store.add(object);
        self.event_sink.emit(.{ .alloc = .{
            .handle = handle,
            .kind = object.kind().?,
            .size = object.wosize(),
        } });
        return Value.fromHeapRef(handle);
    }

    fn objectForWrite(self: *Mutator, block_value: Value) Error!*Object {
        const handle = block_value.asHeapRef() orelse return Error.InvalidValue;
        return self.heap_store.get(handle) orelse Error.InvalidValue;
    }

    fn tupleFieldsForWrite(self: *Mutator, tuple_value: Value) Error![]Value {
        const obj = try self.objectForWrite(tuple_value);
        return obj.tupleFields() orelse Error.InvalidValue;
    }

    fn stringBufferForWrite(self: *Mutator, string_value: Value) Error![]u8 {
        const obj = try self.objectForWrite(string_value);
        return obj.stringBufferMut() orelse Error.InvalidValue;
    }

    fn storeField(self: *Mutator, tuple_value: Value, index: usize, next: Value, comptime phase: StorePhase) Error!void {
        const handle = tuple_value.asHeapRef() orelse return Error.InvalidValue;
        const fields = try self.tupleFieldsForWrite(tuple_value);
        if (index >= fields.len) return Error.InvalidValue;

        // All GC-relevant tuple stores funnel through this one path so future
        // barriers or mutation verification only need to hook here.
        fields[index] = next;
        self.event_sink.emit(.{ .field_write = .{
            .target = handle,
            .index = index,
            .phase = switch (phase) {
                .initialize => .initialize,
                .mutate => .mutate,
            },
        } });
    }

    fn storeStringBytes(self: *Mutator, string_value: Value, bytes: []const u8, comptime phase: StorePhase) Error!void {
        const handle = string_value.asHeapRef() orelse return Error.InvalidValue;
        const obj = try self.objectForWrite(string_value);
        const len = obj.stringSlice() orelse return Error.InvalidValue;
        if (bytes.len > len.len) return Error.OutOfMemory;

        const storage = obj.stringBufferMut() orelse return Error.InvalidValue;
        if (bytes.len < storage.len - 1) {
            @memset(storage[bytes.len .. storage.len - 1], 0);
        }
        @memcpy(storage[0..bytes.len], bytes);
        storage[storage.len - 1] = 0;
        self.event_sink.emit(.{ .bytes_write = .{
            .target = handle,
            .len = bytes.len,
            .phase = switch (phase) {
                .initialize => .initialize,
                .mutate => .mutate,
            },
        } });
    }
};

test "mutator: tuple allocation and field writes use typed store path" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var mutator = Mutator.init(std.testing.allocator, &store, EventSink.noop());

    const tuple = try mutator.allocTuple(2);
    try mutator.initField(tuple, 0, Value.fromInt(1));
    try mutator.writeField(tuple, 1, Value.fromInt(2));

    const object = store.get(tuple.asHeapRef().?).?;
    const fields = object.tupleFields().?;
    try std.testing.expectEqual(Value.fromInt(1), fields[0]);
    try std.testing.expectEqual(Value.fromInt(2), fields[1]);
}

test "mutator: string writes preserve sentinel" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var mutator = Mutator.init(std.testing.allocator, &store, EventSink.noop());

    const string = try mutator.allocStringLen(4);
    try mutator.fillString(string, 'x');
    try mutator.writeStringBytes(string, "ok");

    const object = store.get(string.asHeapRef().?).?;
    const bytes = object.stringBuffer().?;
    try std.testing.expectEqualSlices(u8, "ok", bytes[0..2]);
    try std.testing.expectEqual(@as(u8, 0), bytes[2]);
    try std.testing.expectEqual(@as(u8, 0), bytes[bytes.len - 1]);
}

test "mutator: field writes reject non-tuples" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var mutator = Mutator.init(std.testing.allocator, &store, EventSink.noop());

    const number = try mutator.allocBoxedI64(7);
    try std.testing.expectError(Error.InvalidValue, mutator.writeField(number, 0, Value.fromInt(1)));
}

test "mutator: emits allocation and mutation events" {
    var recorder = event_sink.Recorder{};
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var mutator = Mutator.init(std.testing.allocator, &store, recorder.sink());

    const tuple = try mutator.allocTuple(1);
    try mutator.initField(tuple, 0, Value.fromInt(1));
    const string = try mutator.allocStringLen(2);
    try mutator.writeStringBytes(string, "ok");

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 2), counters.allocations);
    try std.testing.expectEqual(@as(usize, 1), counters.field_writes);
    try std.testing.expectEqual(@as(usize, 1), counters.bytes_writes);
}
