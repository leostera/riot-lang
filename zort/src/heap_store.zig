const std = @import("std");
const value = @import("value.zig");

pub const HeapRef = value.HeapRef;
pub const ObjectKind = enum {
    tuple,
    string,
    boxed_i64,
    boxed_f64,
    custom,
};

pub const StringStorage = struct {
    len: usize,
    buffer: []u8,
};

const Payload = union(enum) {
    none,
    tuple: []value.Value,
    string: StringStorage,
    boxed_i64: i64,
    boxed_f64: f64,
    custom: []u8,
};

pub const Object = struct {
    marked: bool,
    payload: Payload,

    pub fn empty() Object {
        return .{
            .marked = false,
            .payload = .none,
        };
    }

    pub fn initTuple(fields: []value.Value) Object {
        return .{
            .marked = false,
            .payload = .{ .tuple = fields },
        };
    }

    pub fn initString(len: usize, buffer: []u8) Object {
        return .{
            .marked = false,
            .payload = .{ .string = .{
                .len = len,
                .buffer = buffer,
            } },
        };
    }

    pub fn initBoxedI64(number: i64) Object {
        return .{
            .marked = false,
            .payload = .{ .boxed_i64 = number },
        };
    }

    pub fn initBoxedF64(number: f64) Object {
        return .{
            .marked = false,
            .payload = .{ .boxed_f64 = number },
        };
    }

    pub fn initCustom(bytes: []u8) Object {
        return .{
            .marked = false,
            .payload = .{ .custom = bytes },
        };
    }

    pub fn kind(self: *const Object) ?ObjectKind {
        return switch (self.payload) {
            .none => null,
            .tuple => .tuple,
            .string => .string,
            .boxed_i64 => .boxed_i64,
            .boxed_f64 => .boxed_f64,
            .custom => .custom,
        };
    }

    pub fn compatTag(self: *const Object) value.Tag {
        return switch (self.payload) {
            .none => unreachable,
            .tuple => .tuple,
            .string => .string,
            .boxed_i64 => .int64,
            .boxed_f64 => .double,
            .custom => .custom,
        };
    }

    pub fn wosize(self: *const Object) usize {
        return switch (self.payload) {
            .none => 0,
            .tuple => |fields| fields.len,
            .string => |storage| storage.len,
            .boxed_i64, .boxed_f64 => 1,
            .custom => |bytes| bytes.len,
        };
    }

    pub fn tupleFields(self: *Object) ?[]value.Value {
        return switch (self.payload) {
            .tuple => |fields| fields,
            else => null,
        };
    }

    pub fn stringSlice(self: *const Object) ?[]const u8 {
        return switch (self.payload) {
            .string => |storage| storage.buffer[0..storage.len],
            else => null,
        };
    }

    pub fn stringBuffer(self: *const Object) ?[]const u8 {
        return switch (self.payload) {
            .string => |storage| storage.buffer,
            else => null,
        };
    }

    pub fn stringBufferMut(self: *Object) ?[]u8 {
        return switch (self.payload) {
            .string => |*storage| storage.buffer,
            else => null,
        };
    }

    pub fn boxedI64(self: *const Object) ?i64 {
        return switch (self.payload) {
            .boxed_i64 => |number| number,
            else => null,
        };
    }

    pub fn boxedF64(self: *const Object) ?f64 {
        return switch (self.payload) {
            .boxed_f64 => |number| number,
            else => null,
        };
    }

    pub fn customBytes(self: *const Object) ?[]const u8 {
        return switch (self.payload) {
            .custom => |bytes| bytes,
            else => null,
        };
    }

    pub fn deinit(self: *Object, allocator: std.mem.Allocator, fixed_arena: bool) void {
        if (!fixed_arena) {
            switch (self.payload) {
                .tuple => |fields| if (fields.len > 0) allocator.free(fields),
                .string => |storage| allocator.free(storage.buffer),
                .custom => |bytes| if (bytes.len > 0) allocator.free(bytes),
                .none, .boxed_i64, .boxed_f64 => {},
            }
        }
        self.* = Object.empty();
    }
};

const HeapSlot = struct {
    generation: u32,
    alive: bool,
    object: Object,
};

pub const HeapStore = struct {
    allocator: std.mem.Allocator,
    slots: std.ArrayListUnmanaged(HeapSlot) = .{},
    free_indices: std.ArrayListUnmanaged(u32) = .{},
    object_count: usize = 0,

    pub fn init(allocator: std.mem.Allocator) HeapStore {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *HeapStore, fixed_arena: bool) void {
        self.clear(fixed_arena);
        self.slots.deinit(self.allocator);
        self.free_indices.deinit(self.allocator);
    }

    pub fn count(self: *const HeapStore) usize {
        return self.object_count;
    }

    pub fn slotsRef(self: *const HeapStore) []const HeapSlot {
        return self.slots.items;
    }

    pub fn slotsMut(self: *HeapStore) []HeapSlot {
        return self.slots.items;
    }

    pub fn add(self: *HeapStore, object: Object) !value.HeapRef {
        const slot_index: usize = if (self.free_indices.items.len > 0) blk: {
            const reused = self.free_indices.pop() orelse unreachable;
            break :blk @intCast(reused);
        } else self.slots.items.len;

        if (slot_index < self.slots.items.len) {
            const slot = &self.slots.items[slot_index];
            slot.alive = true;
            slot.object = object;
            self.object_count += 1;
            return .{ .index = @intCast(slot_index), .generation = slot.generation };
        }

        try self.slots.append(self.allocator, .{
            .generation = 1,
            .alive = true,
            .object = object,
        });
        self.object_count += 1;
        return .{ .index = @intCast(slot_index), .generation = 1 };
    }

    pub fn get(self: *const HeapStore, handle: value.HeapRef) ?*Object {
        if (handle.index >= self.slots.items.len) return null;
        const slot = &self.slots.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return &slot.object;
    }

    pub fn reclaim(self: *HeapStore, handle: value.HeapRef, fixed_arena: bool) bool {
        if (handle.index >= self.slots.items.len) return false;
        const slot = &self.slots.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return false;
        self.reclaimSlot(handle.index, fixed_arena);
        return true;
    }

    pub fn reclaimSlot(self: *HeapStore, slot_index: usize, fixed_arena: bool) void {
        if (slot_index >= self.slots.items.len) return;
        const slot = &self.slots.items[slot_index];
        if (!slot.alive) return;

        slot.object.deinit(self.allocator, fixed_arena);
        slot.alive = false;
        slot.generation +%= 1;
        self.object_count -%= 1;
        self.free_indices.append(self.allocator, @intCast(slot_index)) catch {
            @panic("zort: out of memory while storing reclaimed slot");
        };
    }

    pub fn clear(self: *HeapStore, fixed_arena: bool) void {
        var i: usize = 0;
        while (i < self.slots.items.len) : (i += 1) {
            self.reclaimSlot(i, fixed_arena);
        }
    }
};

test "heap_store: add and get object" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);

    const fields = try std.testing.allocator.alloc(value.Value, 1);
    fields[0] = value.Value.fromInt(7);

    const handle = try store.add(Object.initTuple(fields));
    const got = store.get(handle).?;

    try std.testing.expectEqual(@as(u32, 1), handle.generation);
    try std.testing.expectEqual(@as(usize, 1), store.count());
    try std.testing.expectEqual(@as(?ObjectKind, .tuple), got.kind());
    try std.testing.expectEqual(value.Value.fromInt(7), got.tupleFields().?[0]);
}

test "heap_store: reclaim enables deterministic LIFO slot reuse" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);

    const fields = try std.testing.allocator.alloc(value.Value, 1);
    fields[0] = value.Value.fromInt(1);
    _ = try store.add(Object.initTuple(fields));

    const h1 = try store.add(Object.initBoxedF64(12.5));
    try std.testing.expectEqual(@as(usize, 2), store.count());
    try std.testing.expect(store.reclaim(h1, false));

    const h2 = try store.add(Object.initBoxedI64(17));
    try std.testing.expectEqual(h1.index, h2.index);
    try std.testing.expectEqual(h1.generation +% 1, h2.generation);
    try std.testing.expect(store.get(h1) == null);
    try std.testing.expectEqual(@as(usize, 2), store.count());
    try std.testing.expectEqual(@as(i64, 17), store.get(h2).?.boxedI64().?);
}

test "heap_store: clear drops all objects" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);

    const left = try std.testing.allocator.alloc(value.Value, 1);
    left[0] = value.Value.fromInt(1);
    _ = try store.add(Object.initTuple(left));

    const buffer = try std.testing.allocator.alloc(u8, 3);
    @memcpy(buffer, "hi\x00");
    _ = try store.add(Object.initString(2, buffer));

    try std.testing.expectEqual(@as(usize, 2), store.count());
    store.clear(false);
    try std.testing.expectEqual(@as(usize, 0), store.count());

    const handle = try store.add(Object.initBoxedI64(42));
    try std.testing.expectEqual(@as(u32, 1), handle.index);
    try std.testing.expectEqual(@as(usize, 1), store.count());
}
