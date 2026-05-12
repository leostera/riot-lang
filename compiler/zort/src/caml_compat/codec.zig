const std = @import("std");
const runtime_mod = @import("../runtime.zig");
const value_mod = @import("../value.zig");

pub const Runtime = runtime_mod.Runtime;
pub const Value = value_mod.Value;
pub const Atom = value_mod.Atom;
pub const CompatValue = u64;

const IntTag: u64 = 0b01;
const HandleTag: u64 = 0b00;
const AtomTag: u64 = 0b11;
const ReservedTag: u64 = 0b10;

pub const Error = runtime_mod.Error || error{
    CompatIntOutOfRange,
    InvalidCompatValue,
    StaleHandle,
    NotHandleValue,
};

const HandleSlot = struct {
    generation: u32 = 1,
    live: bool = false,
    value: Value = value_mod.Unit,
    root: ?runtime_mod.RootHandle = null,
};

pub const HandleTable = struct {
    allocator: std.mem.Allocator,
    slots: std.ArrayListUnmanaged(*HandleSlot) = .{},
    free_indices: std.ArrayListUnmanaged(u32) = .{},

    pub fn init(allocator: std.mem.Allocator) HandleTable {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *HandleTable, runtime: *Runtime) void {
        _ = runtime;
        for (self.slots.items) |slot| {
            releaseSlotRoot(slot);
            self.allocator.destroy(slot);
        }
        self.slots.deinit(self.allocator);
        self.free_indices.deinit(self.allocator);
    }

    pub fn encodeValue(self: *HandleTable, runtime: *Runtime, value: Value) Error!CompatValue {
        return switch (value) {
            .immediate => |imm| switch (imm) {
                .int => |number| encodeInt(number),
                .atom => |atom| encodeAtom(atom),
            },
            .block => try self.encodeBlockHandle(runtime, value),
        };
    }

    pub fn decodeValue(self: *HandleTable, raw: CompatValue) Error!Value {
        return switch (raw & 0b11) {
            HandleTag => try self.decodeHandle(raw),
            IntTag => decodeInt(raw),
            AtomTag => decodeAtom(raw),
            ReservedTag => Error.InvalidCompatValue,
            else => unreachable,
        };
    }

    pub fn releaseHandle(self: *HandleTable, runtime: *Runtime, raw: CompatValue) Error!void {
        _ = runtime;
        if ((raw & 0b11) != HandleTag) return Error.NotHandleValue;
        const handle = decodeHandleParts(raw);
        const slot = self.slots.items[handle.slot_index];
        if (!slot.live or slot.generation != handle.generation) return Error.StaleHandle;

        releaseSlotRoot(slot);
        slot.value = value_mod.Unit;
        slot.live = false;
        slot.generation +%= 1;
        try self.free_indices.append(self.allocator, @intCast(handle.slot_index));
    }

    pub fn activeHandleCount(self: *const HandleTable) usize {
        var active: usize = 0;
        for (self.slots.items) |slot| {
            if (slot.live) active += 1;
        }
        return active;
    }

    pub fn verify(self: *const HandleTable, runtime: *Runtime) Error!void {
        for (self.slots.items) |slot| {
            if (!slot.live) continue;
            if (slot.value.isBlock()) {
                const rooted = runtime.objectFromDebug(slot.value) orelse return Error.StaleHandle;
                _ = rooted;
            }
        }
    }

    fn encodeBlockHandle(self: *HandleTable, runtime: *Runtime, value: Value) Error!CompatValue {
        const allocated = try self.allocateSlot();
        errdefer self.rollbackAllocatedSlot(allocated);

        allocated.slot.value = value;
        allocated.slot.root = try runtime.scopedInteropRoot(&allocated.slot.value);
        errdefer releaseSlotRoot(allocated.slot);
        allocated.slot.live = true;
        return encodeHandle(@intCast(allocated.index), allocated.slot.generation);
    }

    fn decodeHandle(self: *HandleTable, raw: CompatValue) Error!Value {
        const handle = decodeHandleParts(raw);
        if (handle.slot_index >= self.slots.items.len) return Error.StaleHandle;
        const slot = self.slots.items[handle.slot_index];
        if (!slot.live or slot.generation != handle.generation) return Error.StaleHandle;
        return slot.value;
    }

    const AllocatedSlot = struct {
        index: usize,
        slot: *HandleSlot,
        reused: bool,
    };

    fn allocateSlot(self: *HandleTable) !AllocatedSlot {
        if (self.free_indices.items.len > 0) {
            const reused_index = self.free_indices.pop().?;
            const slot = self.slots.items[reused_index];
            slot.value = value_mod.Unit;
            slot.live = false;
            releaseSlotRoot(slot);
            return .{
                .index = reused_index,
                .slot = slot,
                .reused = true,
            };
        }

        const slot = try self.allocator.create(HandleSlot);
        slot.* = .{};
        try self.slots.append(self.allocator, slot);
        return .{
            .index = self.slots.items.len - 1,
            .slot = slot,
            .reused = false,
        };
    }

    fn rollbackAllocatedSlot(self: *HandleTable, allocated: AllocatedSlot) void {
        releaseSlotRoot(allocated.slot);
        if (allocated.reused) {
            self.free_indices.append(self.allocator, @intCast(allocated.index)) catch {};
            allocated.slot.value = value_mod.Unit;
            allocated.slot.live = false;
            return;
        }

        const slot = self.slots.pop().?;
        self.allocator.destroy(slot);
    }
};

fn releaseSlotRoot(slot: *HandleSlot) void {
    if (slot.root) |*root| root.deinit();
    slot.root = null;
}

fn encodeAtom(atom: Atom) CompatValue {
    return (@as(u64, @intFromEnum(atom)) << 2) | AtomTag;
}

fn decodeAtom(raw: CompatValue) Error!Value {
    const atom_code: u8 = @intCast(raw >> 2);
    const atom = std.meta.intToEnum(Atom, atom_code) catch return Error.InvalidCompatValue;
    return Value.fromAtom(atom);
}

fn encodeInt(number: i64) Error!CompatValue {
    if (number < std.math.minInt(i62) or number > std.math.maxInt(i62)) {
        return Error.CompatIntOutOfRange;
    }
    const narrowed: i62 = @intCast(number);
    const bits: u62 = @bitCast(narrowed);
    return (@as(u64, bits) << 2) | IntTag;
}

fn decodeInt(raw: CompatValue) Value {
    const bits: u62 = @intCast(raw >> 2);
    const narrowed: i62 = @bitCast(bits);
    return Value.fromInt(@as(i64, narrowed));
}

const HandleParts = struct {
    slot_index: usize,
    generation: u32,
};

fn encodeHandle(slot_index: u32, generation: u32) CompatValue {
    return (@as(u64, slot_index) << 33) | (@as(u64, generation) << 2) | HandleTag;
}

fn decodeHandleParts(raw: CompatValue) HandleParts {
    return .{
        .slot_index = @intCast(raw >> 33),
        .generation = @intCast((raw >> 2) & 0x7fff_ffff),
    };
}

test "compat: immediate ints and atoms round-trip" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();
    var handles = HandleTable.init(std.testing.allocator);
    defer handles.deinit(&rt);

    const int_raw = try handles.encodeValue(&rt, Value.fromInt(42));
    try std.testing.expectEqual(Value.fromInt(42), try handles.decodeValue(int_raw));

    const atom_raw = try handles.encodeValue(&rt, value_mod.True);
    try std.testing.expectEqual(value_mod.True, try handles.decodeValue(atom_raw));
}

test "compat: block handles root values until released" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();
    var handles = HandleTable.init(std.testing.allocator);
    defer handles.deinit(&rt);

    const tuple = try rt.allocTuple(1);
    const raw = try handles.encodeValue(&rt, tuple);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());
    try std.testing.expectEqual(tuple, try handles.decodeValue(raw));

    try handles.releaseHandle(&rt, raw);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
}

test "compat: stale handles are rejected after release" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();
    var handles = HandleTable.init(std.testing.allocator);
    defer handles.deinit(&rt);

    const tuple = try rt.allocTuple(0);
    const raw = try handles.encodeValue(&rt, tuple);
    try handles.releaseHandle(&rt, raw);
    try std.testing.expectError(Error.StaleHandle, handles.decodeValue(raw));
}

test "compat: verify accepts active rooted handles" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();
    var handles = HandleTable.init(std.testing.allocator);
    defer handles.deinit(&rt);

    const tuple = try rt.allocTuple(0);
    _ = try handles.encodeValue(&rt, tuple);
    try handles.verify(&rt);
}
