const std = @import("std");
const collector_mod = @import("collector.zig");
const root_provider = @import("root_provider.zig");
const value = @import("value.zig");

pub const Collector = collector_mod.Collector;
pub const RootProvider = root_provider.RootProvider;
pub const RootVisitor = root_provider.RootVisitor;
pub const Value = value.Value;

pub const WeakRefHandle = struct {
    index: u32,
    generation: u32,
};

pub const EphemeronHandle = struct {
    index: u32,
    generation: u32,
};

pub const FinalizerHandle = struct {
    index: u32,
    generation: u32,
};

pub const FinalizerMode = enum {
    first,
    last,
};

pub const ReadyFinalizer = struct {
    handle: FinalizerHandle,
    callback: Value,
    argument: ?Value,
    mode: FinalizerMode,
};

const WeakSlot = struct {
    generation: u32,
    alive: bool,
    target: ?Value,
};

const EphemeronSlot = struct {
    generation: u32,
    alive: bool,
    keys: []Value,
    data: ?Value,
};

const FinalizerSlot = struct {
    generation: u32,
    alive: bool,
    target: Value,
    callback: Value,
    mode: FinalizerMode,
    queued: bool = false,
};

pub const ManagedLiveness = struct {
    allocator: std.mem.Allocator,
    weak_slots: std.ArrayListUnmanaged(WeakSlot) = .{},
    weak_free: std.ArrayListUnmanaged(u32) = .{},
    ephemerons: std.ArrayListUnmanaged(EphemeronSlot) = .{},
    ephemeron_free: std.ArrayListUnmanaged(u32) = .{},
    finalizers: std.ArrayListUnmanaged(FinalizerSlot) = .{},
    finalizer_free: std.ArrayListUnmanaged(u32) = .{},
    ready_finalizers: std.ArrayListUnmanaged(ReadyFinalizer) = .{},

    pub const Error = error{
        OutOfMemory,
        InvalidWeakRef,
        InvalidEphemeron,
        InvalidFinalizer,
    };

    pub fn init(allocator: std.mem.Allocator) ManagedLiveness {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ManagedLiveness) void {
        for (self.ephemerons.items) |slot| {
            if (slot.alive) self.allocator.free(slot.keys);
        }
        self.weak_slots.deinit(self.allocator);
        self.weak_free.deinit(self.allocator);
        self.ephemerons.deinit(self.allocator);
        self.ephemeron_free.deinit(self.allocator);
        self.finalizers.deinit(self.allocator);
        self.finalizer_free.deinit(self.allocator);
        self.ready_finalizers.deinit(self.allocator);
    }

    pub fn provider(self: *ManagedLiveness) RootProvider {
        return .{
            .name = "managed_liveness",
            .ctx = self,
            .count_fn = countRoots,
            .visit_fn = visitRoots,
        };
    }

    pub fn ownerCount(self: *const ManagedLiveness, needle: Value) usize {
        var count: usize = 0;
        for (self.finalizers.items) |slot| {
            if (!slot.alive) continue;
            if (slot.callback.isBlock() and std.meta.eql(slot.callback, needle)) count += 1;
        }
        for (self.ready_finalizers.items) |ready| {
            if (ready.callback.isBlock() and std.meta.eql(ready.callback, needle)) count += 1;
            if (ready.argument) |arg| {
                if (arg.isBlock() and std.meta.eql(arg, needle)) count += 1;
            }
        }
        return count;
    }

    pub fn createWeakRef(self: *ManagedLiveness, target: ?Value) Error!WeakRefHandle {
        const index: usize = if (self.weak_free.items.len > 0) blk: {
            const reused = self.weak_free.pop() orelse unreachable;
            break :blk @intCast(reused);
        } else self.weak_slots.items.len;

        if (index < self.weak_slots.items.len) {
            const slot = &self.weak_slots.items[index];
            slot.alive = true;
            slot.target = target;
            return .{ .index = @intCast(index), .generation = slot.generation };
        }

        try self.weak_slots.append(self.allocator, .{
            .generation = 1,
            .alive = true,
            .target = target,
        });
        return .{ .index = @intCast(index), .generation = 1 };
    }

    pub fn weakGet(self: *const ManagedLiveness, handle: WeakRefHandle) Error!?Value {
        const slot = self.weakSlot(handle) orelse return error.InvalidWeakRef;
        return slot.target;
    }

    pub fn weakSet(self: *ManagedLiveness, handle: WeakRefHandle, target: ?Value) Error!void {
        const slot = self.weakSlotMut(handle) orelse return error.InvalidWeakRef;
        slot.target = target;
    }

    pub fn createEphemeron(self: *ManagedLiveness, keys: []const Value, data: ?Value) Error!EphemeronHandle {
        const copied_keys = try self.allocator.dupe(Value, keys);
        errdefer self.allocator.free(copied_keys);

        const index: usize = if (self.ephemeron_free.items.len > 0) blk: {
            const reused = self.ephemeron_free.pop() orelse unreachable;
            break :blk @intCast(reused);
        } else self.ephemerons.items.len;

        if (index < self.ephemerons.items.len) {
            const slot = &self.ephemerons.items[index];
            if (slot.alive) self.allocator.free(slot.keys);
            slot.alive = true;
            slot.keys = copied_keys;
            slot.data = data;
            return .{ .index = @intCast(index), .generation = slot.generation };
        }

        try self.ephemerons.append(self.allocator, .{
            .generation = 1,
            .alive = true,
            .keys = copied_keys,
            .data = data,
        });
        return .{ .index = @intCast(index), .generation = 1 };
    }

    pub fn ephemeronData(self: *const ManagedLiveness, handle: EphemeronHandle) Error!?Value {
        const slot = self.ephemeron(handle) orelse return error.InvalidEphemeron;
        return slot.data;
    }

    pub fn ephemeronSetData(self: *ManagedLiveness, handle: EphemeronHandle, data: ?Value) Error!void {
        const slot = self.ephemeronMut(handle) orelse return error.InvalidEphemeron;
        slot.data = data;
    }

    pub fn registerFinalizer(
        self: *ManagedLiveness,
        target: Value,
        callback: Value,
        mode: FinalizerMode,
    ) Error!FinalizerHandle {
        const index: usize = if (self.finalizer_free.items.len > 0) blk: {
            const reused = self.finalizer_free.pop() orelse unreachable;
            break :blk @intCast(reused);
        } else self.finalizers.items.len;

        if (index < self.finalizers.items.len) {
            const slot = &self.finalizers.items[index];
            slot.alive = true;
            slot.target = target;
            slot.callback = callback;
            slot.mode = mode;
            slot.queued = false;
            return .{ .index = @intCast(index), .generation = slot.generation };
        }

        try self.finalizers.append(self.allocator, .{
            .generation = 1,
            .alive = true,
            .target = target,
            .callback = callback,
            .mode = mode,
        });
        return .{ .index = @intCast(index), .generation = 1 };
    }

    pub fn readyFinalizerCount(self: *const ManagedLiveness) usize {
        return self.ready_finalizers.items.len;
    }

    pub fn peekReadyFinalizer(self: *const ManagedLiveness) ?ReadyFinalizer {
        if (self.ready_finalizers.items.len == 0) return null;
        return self.ready_finalizers.items[0];
    }

    pub fn acknowledgeReadyFinalizer(self: *ManagedLiveness, handle: FinalizerHandle) bool {
        if (self.ready_finalizers.items.len == 0) return false;
        const ready = self.ready_finalizers.items[0];
        if (ready.handle.index != handle.index or ready.handle.generation != handle.generation) return false;
        _ = self.ready_finalizers.orderedRemove(0);
        return true;
    }

    pub fn drainReadyFinalizers(self: *ManagedLiveness, allocator: std.mem.Allocator) Error![]ReadyFinalizer {
        const drained = try allocator.dupe(ReadyFinalizer, self.ready_finalizers.items);
        self.ready_finalizers.clearRetainingCapacity();
        return drained;
    }

    pub fn processWeak(self: *ManagedLiveness, collector: *Collector) usize {
        var processed: usize = 0;
        var changed = true;
        while (changed) {
            changed = false;
            for (self.ephemerons.items) |*slot| {
                if (!slot.alive) continue;
                if (slot.data == null) continue;
                if (self.anyDeadKey(collector, slot.keys)) {
                    slot.data = null;
                    processed += 1;
                    continue;
                }
                if (!self.allKeysLive(collector, slot.keys)) continue;
                if (collector.markValue(slot.data.?)) {
                    processed += 1;
                    changed = true;
                }
            }
        }

        for (self.weak_slots.items) |*slot| {
            if (!slot.alive) continue;
            if (slot.target) |target| {
                if (!isReachable(collector, target)) {
                    slot.target = null;
                    processed += 1;
                }
            }
        }
        return processed;
    }

    pub fn processFinalizers(self: *ManagedLiveness, collector: *Collector) usize {
        var processed: usize = 0;
        for (self.finalizers.items, 0..) |*slot, slot_index| {
            if (!slot.alive or slot.queued) continue;
            if (isReachable(collector, slot.target)) continue;

            const handle = FinalizerHandle{
                .index = @intCast(slot_index),
                .generation = slot.generation,
            };
            const argument = switch (slot.mode) {
                .first => slot.target,
                .last => null,
            };
            if (argument) |target| {
                _ = collector.markValue(target);
            }
            self.ready_finalizers.append(self.allocator, .{
                .handle = handle,
                .callback = slot.callback,
                .argument = argument,
                .mode = slot.mode,
            }) catch @panic("zort: out of memory while queueing finalizer");
            slot.queued = true;
            slot.alive = false;
            slot.generation +%= 1;
            self.finalizer_free.append(self.allocator, @intCast(slot_index)) catch {
                @panic("zort: out of memory while freeing finalizer slot");
            };
            processed += 1;
        }
        return processed;
    }

    fn countRoots(ctx: ?*anyopaque) usize {
        const self: *ManagedLiveness = @ptrCast(@alignCast(ctx.?));
        var count: usize = 0;
        for (self.finalizers.items) |slot| {
            if (!slot.alive) continue;
            if (slot.callback.isBlock()) count += 1;
        }
        for (self.ready_finalizers.items) |ready| {
            if (ready.callback.isBlock()) count += 1;
            if (ready.argument) |arg| {
                if (arg.isBlock()) count += 1;
            }
        }
        return count;
    }

    fn visitRoots(ctx: ?*anyopaque, visitor: RootVisitor) void {
        const self: *ManagedLiveness = @ptrCast(@alignCast(ctx.?));
        for (self.finalizers.items) |slot| {
            if (!slot.alive) continue;
            if (slot.callback.isBlock()) visitor.visit(slot.callback);
        }
        for (self.ready_finalizers.items) |ready| {
            if (ready.callback.isBlock()) visitor.visit(ready.callback);
            if (ready.argument) |arg| {
                if (arg.isBlock()) visitor.visit(arg);
            }
        }
    }

    fn weakSlot(self: *const ManagedLiveness, handle: WeakRefHandle) ?*const WeakSlot {
        if (handle.index >= self.weak_slots.items.len) return null;
        const slot = &self.weak_slots.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return slot;
    }

    fn weakSlotMut(self: *ManagedLiveness, handle: WeakRefHandle) ?*WeakSlot {
        if (handle.index >= self.weak_slots.items.len) return null;
        const slot = &self.weak_slots.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return slot;
    }

    fn ephemeron(self: *const ManagedLiveness, handle: EphemeronHandle) ?*const EphemeronSlot {
        if (handle.index >= self.ephemerons.items.len) return null;
        const slot = &self.ephemerons.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return slot;
    }

    fn ephemeronMut(self: *ManagedLiveness, handle: EphemeronHandle) ?*EphemeronSlot {
        if (handle.index >= self.ephemerons.items.len) return null;
        const slot = &self.ephemerons.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return slot;
    }

    fn allKeysLive(self: *const ManagedLiveness, collector: *Collector, keys: []const Value) bool {
        _ = self;
        for (keys) |key| {
            if (!isReachable(collector, key)) return false;
        }
        return true;
    }

    fn anyDeadKey(self: *const ManagedLiveness, collector: *Collector, keys: []const Value) bool {
        _ = self;
        for (keys) |key| {
            if (!isReachable(collector, key)) return true;
        }
        return false;
    }
};

fn isReachable(collector: *Collector, rooted: Value) bool {
    if (!rooted.isBlock()) return true;
    return collector.isMarkedValue(rooted);
}

test "managed_liveness: weak refs clear dead targets" {
    var heap = collector_mod.HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;
    var liveness = ManagedLiveness.init(std.testing.allocator);
    defer liveness.deinit();

    const live = try heap.add(collector_mod.Object.initBoxedI64(1));
    const dead = try heap.add(collector_mod.Object.initBoxedI64(2));
    heap.get(live).?.marked = true;

    const weak_live = try liveness.createWeakRef(Value.fromHeapRef(live));
    const weak_dead = try liveness.createWeakRef(Value.fromHeapRef(dead));
    var collector = Collector.init(&heap, null, null, &.{}, &fixed_arena, null, .mark_sweep, collector_mod.EventSink.noop(), Collector.PhaseHooks.noop());
    try std.testing.expectEqual(@as(usize, 1), liveness.processWeak(&collector));
    try std.testing.expectEqual(Value.fromHeapRef(live), (try liveness.weakGet(weak_live)).?);
    try std.testing.expectEqual(@as(?Value, null), try liveness.weakGet(weak_dead));
}

test "managed_liveness: ephemerons mark data only while keys live" {
    var heap = collector_mod.HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;
    var liveness = ManagedLiveness.init(std.testing.allocator);
    defer liveness.deinit();

    const key = Value.fromHeapRef(try heap.add(collector_mod.Object.initBoxedI64(1)));
    const data = Value.fromHeapRef(try heap.add(collector_mod.Object.initBoxedI64(2)));
    const eph = try liveness.createEphemeron(&.{key}, data);

    heap.get(key.asHeapRef().?).?.marked = true;
    var collector = Collector.init(&heap, null, null, &.{}, &fixed_arena, null, .mark_sweep, collector_mod.EventSink.noop(), Collector.PhaseHooks.noop());
    try std.testing.expectEqual(@as(usize, 1), liveness.processWeak(&collector));
    try std.testing.expect(collector.isMarkedValue(data));

    heap.get(key.asHeapRef().?).?.marked = false;
    heap.get(data.asHeapRef().?).?.marked = false;
    try std.testing.expectEqual(@as(usize, 1), liveness.processWeak(&collector));
    try std.testing.expectEqual(@as(?Value, null), try liveness.ephemeronData(eph));
}

test "managed_liveness: first finalizers re-root targets until drained" {
    var heap = collector_mod.HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;
    var liveness = ManagedLiveness.init(std.testing.allocator);
    defer liveness.deinit();

    const target = Value.fromHeapRef(try heap.add(collector_mod.Object.initBoxedI64(1)));
    const callback = Value.fromHeapRef(try heap.add(collector_mod.Object.initBoxedI64(2)));
    _ = try liveness.registerFinalizer(target, callback, .first);

    var collector = Collector.init(&heap, null, null, &.{}, &fixed_arena, null, .mark_sweep, collector_mod.EventSink.noop(), Collector.PhaseHooks.noop());
    try std.testing.expectEqual(@as(usize, 1), liveness.processFinalizers(&collector));
    try std.testing.expect(collector.isMarkedValue(target));
    try std.testing.expectEqual(@as(usize, 1), liveness.readyFinalizerCount());

    const ready = try liveness.drainReadyFinalizers(std.testing.allocator);
    defer std.testing.allocator.free(ready);
    try std.testing.expectEqual(@as(usize, 1), ready.len);
    try std.testing.expectEqual(callback, ready[0].callback);
    try std.testing.expectEqual(target, ready[0].argument.?);
}

test "managed_liveness: ready finalizers can be acknowledged one by one" {
    var heap = collector_mod.HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;
    var liveness = ManagedLiveness.init(std.testing.allocator);
    defer liveness.deinit();

    const target = Value.fromHeapRef(try heap.add(collector_mod.Object.initBoxedI64(1)));
    const callback = Value.fromHeapRef(try heap.add(collector_mod.Object.initBoxedI64(2)));
    _ = try liveness.registerFinalizer(target, callback, .first);

    var collector = Collector.init(&heap, null, null, &.{}, &fixed_arena, null, .mark_sweep, collector_mod.EventSink.noop(), Collector.PhaseHooks.noop());
    _ = liveness.processFinalizers(&collector);

    const ready = liveness.peekReadyFinalizer().?;
    try std.testing.expectEqual(callback, ready.callback);
    try std.testing.expectEqual(target, ready.argument.?);
    try std.testing.expect(liveness.acknowledgeReadyFinalizer(ready.handle));
    try std.testing.expectEqual(@as(usize, 0), liveness.readyFinalizerCount());
    try std.testing.expect(!liveness.acknowledgeReadyFinalizer(ready.handle));
}
