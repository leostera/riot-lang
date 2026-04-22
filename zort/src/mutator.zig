const std = @import("std");
const event_sink = @import("event_sink.zig");
const heap_store = @import("heap_store.zig");
const remembered_set_mod = @import("remembered_set.zig");
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
pub const RememberedSet = remembered_set_mod.RememberedSet;

const StorePhase = enum {
    initialize,
    mutate,
};

pub const Mutator = struct {
    allocator: std.mem.Allocator,
    heap_store: *HeapStore,
    event_sink: EventSink,
    remembered_set: ?*RememberedSet,

    pub fn init(allocator: std.mem.Allocator, store: *HeapStore, sink: EventSink, remembered_set: ?*RememberedSet) Mutator {
        return .{
            .allocator = allocator,
            .heap_store = store,
            .event_sink = sink,
            .remembered_set = remembered_set,
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
        const storage = try self.heap_store.allocTupleFields(self.allocator, len);
        return self.insertObject(Object.initTupleOwned(storage.fields, storage.owner));
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
        if (len == 0) {
            return self.insertObject(Object.initCustomOwned(@constCast(&[_]u8{}), .static));
        }

        const bytes = blk: {
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
        const metrics = object.sizeMetrics();
        self.event_sink.emit(.{ .alloc = .{
            .handle = handle,
            .kind = object.kind().?,
            .payload_bytes = metrics.payload_bytes,
            .storage_bytes = metrics.storage_bytes,
            .scan_words = metrics.scan_words,
            .allocation_cost_units = metrics.allocation_cost_units,
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
        try self.recordBarrier(handle, next, phase);
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

    fn recordBarrier(self: *Mutator, target: HeapRef, next: Value, comptime phase: StorePhase) Error!void {
        var recorded = false;
        if (self.remembered_set) |set| {
            const target_space = self.heap_store.spaceOf(target) orelse return;
            const next_handle = next.asHeapRef() orelse return;
            const next_space = self.heap_store.spaceOf(next_handle) orelse return;
            if (target_space != .major or next_space != .nursery) return;
            recorded = try set.record(target);
        }
        if (!recorded) return;
        self.event_sink.emit(.{ .barrier = .{
            .target = target,
            .value_is_block = true,
        } });
        _ = phase;
    }
};

test "mutator: tuple allocation and field writes use typed store path" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var mutator = Mutator.init(std.testing.allocator, &store, EventSink.noop(), null);

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
    var mutator = Mutator.init(std.testing.allocator, &store, EventSink.noop(), null);

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
    var mutator = Mutator.init(std.testing.allocator, &store, EventSink.noop(), null);

    const number = try mutator.allocBoxedI64(7);
    try std.testing.expectError(Error.InvalidValue, mutator.writeField(number, 0, Value.fromInt(1)));
}

test "mutator: zero-length payloads use static storage ownership" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var mutator = Mutator.init(std.testing.allocator, &store, EventSink.noop(), null);

    const empty_tuple = try mutator.allocTuple(0);
    const empty_custom = try mutator.allocCustomBytes(0);

    try std.testing.expectEqual(heap_store.StorageOwner.static, store.get(empty_tuple.asHeapRef().?).?.storageOwner().?);
    try std.testing.expectEqual(heap_store.StorageOwner.static, store.get(empty_custom.asHeapRef().?).?.storageOwner().?);
}

test "mutator: nursery tuples allocate pinned page-backed field storage" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    store.configureNursery(.{
        .enabled = true,
        .max_object_units = 8,
    });
    var mutator = Mutator.init(std.testing.allocator, &store, EventSink.noop(), null);

    const tuple = try mutator.allocTuple(2);
    const obj = store.get(tuple.asHeapRef().?).?;
    const fields = obj.tupleFields().?;

    try std.testing.expectEqual(heap_store.StorageOwner.nursery_page, obj.storageOwner().?);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expectEqual(Value.fromInt(0), fields[0]);
    try std.testing.expectEqual(Value.fromInt(0), fields[1]);
}

test "mutator: emits allocation and mutation events" {
    var recorder = event_sink.Recorder{};
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var mutator = Mutator.init(std.testing.allocator, &store, recorder.sink(), null);

    const tuple = try mutator.allocTuple(1);
    try mutator.initField(tuple, 0, Value.fromInt(1));
    const string = try mutator.allocStringLen(2);
    try mutator.writeStringBytes(string, "ok");

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 2), counters.allocations);
    try std.testing.expectEqual(@as(usize, 1), counters.field_writes);
    try std.testing.expectEqual(@as(usize, 1), counters.bytes_writes);
}

test "mutator: mutate records remembered major targets for nursery children" {
    var recorder = event_sink.Recorder{};
    var remembered = RememberedSet.init(std.testing.allocator);
    defer remembered.deinit();
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    store.configureNursery(.{
        .enabled = true,
        .max_object_units = 2,
    });
    var mutator = Mutator.init(std.testing.allocator, &store, recorder.sink(), &remembered);

    const fields = try std.testing.allocator.alloc(Value, 1);
    fields[0] = Value.fromInt(0);
    const target = Value.fromHeapRef(try store.addInSpace(Object.initTuple(fields), .major));
    const child = try mutator.allocTuple(0);
    try mutator.writeField(target, 0, child);
    try mutator.writeField(target, 0, Value.fromInt(0));

    try std.testing.expectEqual(@as(usize, 1), remembered.count());
    try std.testing.expectEqual(@as(usize, 1), recorder.snapshot().barrier_records);
}

test "mutator: duplicate nursery stores only remember a major target once" {
    var recorder = event_sink.Recorder{};
    var remembered = RememberedSet.init(std.testing.allocator);
    defer remembered.deinit();
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    store.configureNursery(.{
        .enabled = true,
        .max_object_units = 2,
    });
    var mutator = Mutator.init(std.testing.allocator, &store, recorder.sink(), &remembered);

    const fields = try std.testing.allocator.alloc(Value, 1);
    fields[0] = Value.fromInt(0);
    const target = Value.fromHeapRef(try store.addInSpace(Object.initTuple(fields), .major));
    const first = try mutator.allocTuple(0);
    const second = try mutator.allocTuple(0);
    try mutator.writeField(target, 0, first);
    try mutator.writeField(target, 0, second);

    try std.testing.expectEqual(@as(usize, 1), remembered.count());
    try std.testing.expectEqual(@as(usize, 1), recorder.snapshot().barrier_records);
}

test "mutator: initialize records remembered major targets for nursery fields" {
    var recorder = event_sink.Recorder{};
    var remembered = RememberedSet.init(std.testing.allocator);
    defer remembered.deinit();
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    store.configureNursery(.{
        .enabled = true,
        .max_object_units = 2,
    });
    var mutator = Mutator.init(std.testing.allocator, &store, recorder.sink(), &remembered);

    const fields = try std.testing.allocator.alloc(Value, 1);
    fields[0] = Value.fromInt(0);
    const target = Value.fromHeapRef(try store.addInSpace(Object.initTuple(fields), .major));
    const child = try mutator.allocTuple(0);
    try mutator.initField(target, 0, child);

    try std.testing.expectEqual(@as(usize, 1), remembered.count());
    try std.testing.expectEqual(@as(usize, 1), recorder.snapshot().barrier_records);
}
