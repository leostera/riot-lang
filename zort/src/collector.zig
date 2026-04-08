const std = @import("std");
const heap_store = @import("heap_store.zig");
const root_registry = @import("root_registry.zig");
const value = @import("value.zig");

pub const Value = value.Value;
pub const HeapStore = heap_store.HeapStore;
pub const Object = heap_store.Object;
pub const RootRegistry = root_registry.RootRegistry;

pub const GcStrategy = enum {
    mark_sweep,
    bump,
};

pub const Collector = struct {
    heap_store: *HeapStore,
    explicit_roots: []const *const Value,
    gc_strategy: GcStrategy,
    fixed_arena: *?std.heap.FixedBufferAllocator,
    fixed_arena_buffer: ?[]u8,

    pub fn init(
        heap: *HeapStore,
        explicit_roots: []const *const Value,
        fixed_arena: *?std.heap.FixedBufferAllocator,
        fixed_arena_buffer: ?[]u8,
        gc_strategy: GcStrategy,
    ) Collector {
        return .{
            .heap_store = heap,
            .explicit_roots = explicit_roots,
            .gc_strategy = gc_strategy,
            .fixed_arena = fixed_arena,
            .fixed_arena_buffer = fixed_arena_buffer,
        };
    }

    pub fn collect(self: *Collector) void {
        switch (self.gc_strategy) {
            .mark_sweep => self.collectMarkSweep(),
            .bump => self.collectBump(),
        }
    }

    fn collectMarkSweep(self: *Collector) void {
        for (self.explicit_roots) |slot| {
            self.mark(slot.*);
        }

        const fixed_arena = self.fixed_arena_buffer != null;
        const slots = self.heap_store.slotsMut();
        for (slots, 0..) |*slot, slot_index| {
            if (!slot.alive) continue;
            if (!slot.object.marked) {
                self.heap_store.reclaimSlot(slot_index, fixed_arena);
            } else {
                slot.object.marked = false;
            }
        }
    }

    fn collectBump(self: *Collector) void {
        if (self.fixed_arena_buffer) |buffer| {
            self.fixed_arena.* = std.heap.FixedBufferAllocator.init(buffer);
            self.heap_store.clear(true);
            return;
        }

        self.heap_store.clear(false);
    }

    fn objectFrom(self: *Collector, block_value: Value) ?*Object {
        const handle = block_value.asHeapRef() orelse return null;
        return self.heap_store.get(handle);
    }

    fn mark(self: *Collector, block_value: Value) void {
        const obj = self.objectFrom(block_value) orelse return;
        if (obj.marked) return;
        obj.marked = true;

        if (obj.tupleFields()) |fields| {
            for (fields) |child| {
                self.mark(child);
            }
        }
    }
};

test "collector: mark-sweep keeps rooted graph and reclaims unreachable objects" {
    const mutator_mod = @import("mutator.zig");

    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var roots = RootRegistry.init(std.testing.allocator);
    defer roots.deinit();
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;

    var writer = mutator_mod.Mutator.init(std.testing.allocator, &heap);
    var root = try writer.allocTuple(1);
    const child = try writer.allocTuple(0);
    _ = try writer.allocTuple(0);
    try writer.writeField(root, 0, child);

    try roots.register(&root);

    var collector = Collector.init(&heap, roots.items(), &fixed_arena, null, .mark_sweep);
    collector.collect();
    try std.testing.expectEqual(@as(usize, 2), heap.count());

    roots.unregister(&root);
    collector = Collector.init(&heap, roots.items(), &fixed_arena, null, .mark_sweep);
    collector.collect();
    try std.testing.expectEqual(@as(usize, 0), heap.count());
}

test "collector: bump strategy resets fixed arena allocation state" {
    const mutator_mod = @import("mutator.zig");

    var arena_bytes = [_]u8{0} ** 256;
    var fixed_arena: ?std.heap.FixedBufferAllocator = std.heap.FixedBufferAllocator.init(arena_bytes[0..]);
    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(true);
    var roots = RootRegistry.init(std.testing.allocator);
    defer roots.deinit();

    {
        var writer = mutator_mod.Mutator.init(fixed_arena.?.allocator(), &heap);
        _ = try writer.allocTuple(2);
    }
    try std.testing.expectEqual(@as(usize, 1), heap.count());

    var collector = Collector.init(&heap, roots.items(), &fixed_arena, arena_bytes[0..], .bump);
    collector.collect();
    try std.testing.expectEqual(@as(usize, 0), heap.count());

    {
        var writer = mutator_mod.Mutator.init(fixed_arena.?.allocator(), &heap);
        _ = try writer.allocTuple(2);
    }
    try std.testing.expectEqual(@as(usize, 1), heap.count());
}
