const std = @import("std");
const event_sink = @import("event_sink.zig");
const heap_store = @import("heap_store.zig");
const root_provider = @import("root_provider.zig");
const root_registry = @import("root_registry.zig");
const value = @import("value.zig");

pub const Value = value.Value;
pub const HeapStore = heap_store.HeapStore;
pub const Object = heap_store.Object;
pub const RootProvider = root_provider.RootProvider;
pub const RootVisitor = root_provider.RootVisitor;
pub const RootRegistry = root_registry.RootRegistry;
pub const EventSink = event_sink.EventSink;

pub const GcStrategy = enum {
    mark_sweep,
    bump,
};

pub const Collector = struct {
    heap_store: *HeapStore,
    root_providers: []const RootProvider,
    gc_strategy: GcStrategy,
    fixed_arena: *?std.heap.FixedBufferAllocator,
    fixed_arena_buffer: ?[]u8,
    event_sink: EventSink,

    pub fn init(
        heap: *HeapStore,
        root_providers: []const RootProvider,
        fixed_arena: *?std.heap.FixedBufferAllocator,
        fixed_arena_buffer: ?[]u8,
        gc_strategy: GcStrategy,
        sink: EventSink,
    ) Collector {
        return .{
            .heap_store = heap,
            .root_providers = root_providers,
            .gc_strategy = gc_strategy,
            .fixed_arena = fixed_arena,
            .fixed_arena_buffer = fixed_arena_buffer,
            .event_sink = sink,
        };
    }

    pub fn collect(self: *Collector) void {
        const root_count = self.rootCount();
        self.event_sink.emit(.{ .collect = .{
            .phase = .start,
            .strategy = self.collectStrategy(),
            .root_count = root_count,
            .reclaimed = 0,
        } });
        var reclaimed: usize = 0;
        switch (self.gc_strategy) {
            .mark_sweep => reclaimed = self.collectMarkSweep(),
            .bump => reclaimed = self.collectBump(),
        }
        self.event_sink.emit(.{ .collect = .{
            .phase = .end,
            .strategy = self.collectStrategy(),
            .root_count = root_count,
            .reclaimed = reclaimed,
        } });
    }

    fn collectMarkSweep(self: *Collector) usize {
        const visitor = RootVisitor{
            .ctx = self,
            .visit_fn = visitRoot,
        };
        for (self.root_providers) |provider| {
            provider.visit(visitor);
        }

        const fixed_arena = self.fixed_arena_buffer != null;
        const slots = self.heap_store.slotsMut();
        var reclaimed: usize = 0;
        for (slots, 0..) |*slot, slot_index| {
            if (!slot.alive) continue;
            if (!slot.object.marked) {
                self.event_sink.emit(.{ .reclaim = .{
                    .handle = .{
                        .index = @intCast(slot_index),
                        .generation = slot.generation,
                    },
                    .kind = slot.object.kind().?,
                } });
                self.heap_store.reclaimSlot(slot_index, fixed_arena);
                reclaimed += 1;
            } else {
                slot.object.marked = false;
            }
        }
        return reclaimed;
    }

    fn collectBump(self: *Collector) usize {
        const reclaimed = self.emitLiveReclaims();
        if (self.fixed_arena_buffer) |buffer| {
            self.fixed_arena.* = std.heap.FixedBufferAllocator.init(buffer);
            self.heap_store.clear(true);
            return reclaimed;
        }

        self.heap_store.clear(false);
        return reclaimed;
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

    fn emitLiveReclaims(self: *Collector) usize {
        var reclaimed: usize = 0;
        for (self.heap_store.slotsRef(), 0..) |slot, slot_index| {
            if (!slot.alive) continue;
            self.event_sink.emit(.{ .reclaim = .{
                .handle = .{
                    .index = @intCast(slot_index),
                    .generation = slot.generation,
                },
                .kind = slot.object.kind().?,
            } });
            reclaimed += 1;
        }
        return reclaimed;
    }

    fn collectStrategy(self: *const Collector) event_sink.CollectStrategy {
        return switch (self.gc_strategy) {
            .mark_sweep => .mark_sweep,
            .bump => .bump,
        };
    }

    fn rootCount(self: *const Collector) usize {
        var count: usize = 0;
        for (self.root_providers) |provider| {
            count += provider.count();
        }
        return count;
    }

    fn visitRoot(ctx: ?*anyopaque, rooted: Value) void {
        const self: *Collector = @ptrCast(@alignCast(ctx.?));
        self.mark(rooted);
    }
};

test "collector: mark-sweep keeps rooted graph and reclaims unreachable objects" {
    const mutator_mod = @import("mutator.zig");

    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var roots = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer roots.deinit();
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;

    var writer = mutator_mod.Mutator.init(std.testing.allocator, &heap, EventSink.noop());
    var root = try writer.allocTuple(1);
    const child = try writer.allocTuple(0);
    _ = try writer.allocTuple(0);
    try writer.writeField(root, 0, child);

    try roots.register(&root);

    var providers = [_]RootProvider{roots.provider()};
    var collector = Collector.init(&heap, providers[0..], &fixed_arena, null, .mark_sweep, EventSink.noop());
    collector.collect();
    try std.testing.expectEqual(@as(usize, 2), heap.count());

    roots.unregister(&root);
    providers[0] = roots.provider();
    collector = Collector.init(&heap, providers[0..], &fixed_arena, null, .mark_sweep, EventSink.noop());
    collector.collect();
    try std.testing.expectEqual(@as(usize, 0), heap.count());
}

test "collector: bump strategy resets fixed arena allocation state" {
    const mutator_mod = @import("mutator.zig");

    var arena_bytes = [_]u8{0} ** 256;
    var fixed_arena: ?std.heap.FixedBufferAllocator = std.heap.FixedBufferAllocator.init(arena_bytes[0..]);
    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(true);
    var roots = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer roots.deinit();

    {
        var writer = mutator_mod.Mutator.init(fixed_arena.?.allocator(), &heap, EventSink.noop());
        _ = try writer.allocTuple(2);
    }
    try std.testing.expectEqual(@as(usize, 1), heap.count());

    var providers = [_]RootProvider{roots.provider()};
    var collector = Collector.init(&heap, providers[0..], &fixed_arena, arena_bytes[0..], .bump, EventSink.noop());
    collector.collect();
    try std.testing.expectEqual(@as(usize, 0), heap.count());

    {
        var writer = mutator_mod.Mutator.init(fixed_arena.?.allocator(), &heap, EventSink.noop());
        _ = try writer.allocTuple(2);
    }
    try std.testing.expectEqual(@as(usize, 1), heap.count());
}

test "collector: emits collection and reclaim events" {
    const mutator_mod = @import("mutator.zig");

    var recorder = event_sink.Recorder{};
    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var roots = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer roots.deinit();
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;

    var writer = mutator_mod.Mutator.init(std.testing.allocator, &heap, EventSink.noop());
    _ = try writer.allocTuple(0);

    var providers = [_]RootProvider{roots.provider()};
    var gc = Collector.init(&heap, providers[0..], &fixed_arena, null, .mark_sweep, recorder.sink());
    gc.collect();

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.collections);
    try std.testing.expectEqual(@as(usize, 1), counters.reclaims);
}
