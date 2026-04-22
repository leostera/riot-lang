const std = @import("std");
const event_sink = @import("event_sink.zig");
const heap_store = @import("heap_store.zig");
const memprof_mod = @import("memprof.zig");
const remembered_set_mod = @import("remembered_set.zig");
const root_provider = @import("root_provider.zig");
const root_registry = @import("root_registry.zig");
const value = @import("value.zig");

pub const Value = value.Value;
pub const HeapStore = heap_store.HeapStore;
pub const Object = heap_store.Object;
pub const ObjectKind = heap_store.ObjectKind;
pub const Space = heap_store.Space;
pub const MemprofState = memprof_mod.MemprofState;
pub const RememberedSet = remembered_set_mod.RememberedSet;
pub const RootProvider = root_provider.RootProvider;
pub const RootVisitor = root_provider.RootVisitor;
pub const RootRegistry = root_registry.RootRegistry;
pub const EventSink = event_sink.EventSink;
pub const CollectTimings = event_sink.CollectTimings;
pub const GcPhase = event_sink.GcPhase;
pub const GcPhaseEvent = event_sink.GcPhaseEvent;
pub const GcSnapshotEvent = event_sink.GcSnapshotEvent;
pub const ObjectKindCounts = event_sink.ObjectKindCounts;

pub const GcStrategy = enum {
    mark_sweep,
    generational,
    bump,
};

const MarkScope = enum {
    all,
    nursery_only,
};

const MarkVisitorContext = struct {
    collector: *Collector,
    worklist: *std.ArrayListUnmanaged(value.HeapRef),
    scope: MarkScope,
};

pub const Collector = struct {
    pub const PhaseHooks = struct {
        ctx: ?*anyopaque = null,
        process_weak_fn: *const fn (?*anyopaque, *Collector) usize = noopPhaseHook,
        process_finalizers_fn: *const fn (?*anyopaque, *Collector) usize = noopPhaseHook,

        pub fn processWeak(self: PhaseHooks, collector: *Collector) usize {
            return self.process_weak_fn(self.ctx, collector);
        }

        pub fn processFinalizers(self: PhaseHooks, collector: *Collector) usize {
            return self.process_finalizers_fn(self.ctx, collector);
        }

        pub fn noop() PhaseHooks {
            return .{};
        }

        fn noopPhaseHook(_: ?*anyopaque, _: *Collector) usize {
            return 0;
        }
    };

    heap_store: *HeapStore,
    remembered_set: ?*RememberedSet,
    memprof: ?*MemprofState,
    root_providers: []const RootProvider,
    gc_strategy: GcStrategy,
    fixed_arena: *?std.heap.FixedBufferAllocator,
    fixed_arena_buffer: ?[]u8,
    event_sink: EventSink,
    hooks: PhaseHooks,

    pub fn init(
        heap: *HeapStore,
        remembered_set: ?*RememberedSet,
        memprof: ?*MemprofState,
        root_providers: []const RootProvider,
        fixed_arena: *?std.heap.FixedBufferAllocator,
        fixed_arena_buffer: ?[]u8,
        gc_strategy: GcStrategy,
        sink: EventSink,
        hooks: PhaseHooks,
    ) Collector {
        return .{
            .heap_store = heap,
            .remembered_set = remembered_set,
            .memprof = memprof,
            .root_providers = root_providers,
            .gc_strategy = gc_strategy,
            .fixed_arena = fixed_arena,
            .fixed_arena_buffer = fixed_arena_buffer,
            .event_sink = sink,
            .hooks = hooks,
        };
    }

    pub fn collect(self: *Collector) void {
        switch (self.gc_strategy) {
            .generational => self.collectMinor(),
            else => self.collectMajor(),
        }
    }

    pub fn collectMajor(self: *Collector) void {
        var total_timer = std.time.Timer.start() catch unreachable;
        var root_timer = std.time.Timer.start() catch unreachable;
        var summary = GcSnapshotEvent{
            .strategy = self.collectStrategy(),
            .root_count = 0,
        };
        summary.root_count = self.rootCount();

        self.event_sink.emit(.{ .collect = .{
            .phase = .start,
            .strategy = self.collectStrategy(),
            .root_count = summary.root_count,
            .reclaimed = 0,
        } });
        _ = self.emitRootProviders();
        summary.timings.root_enumeration_ns = root_timer.read();
        self.emitPhase(.enumerate_roots, summary.timings.root_enumeration_ns);
        switch (self.gc_strategy) {
            .mark_sweep, .generational => self.traceMarkSweep(&summary),
            .bump => {
                self.emitPhase(.mark, 0);
                summary.timings.mark_ns = 0;
            },
        }
        var timer = std.time.Timer.start() catch unreachable;
        summary.weak_processed = self.hooks.processWeak(self);
        summary.timings.weak_ns = timer.read();
        self.emitPhase(.weak, summary.timings.weak_ns);

        timer = std.time.Timer.start() catch unreachable;
        summary.finalizers_ready = self.hooks.processFinalizers(self);
        summary.timings.finalizers_ns = timer.read();
        self.emitPhase(.finalizers, summary.timings.finalizers_ns);

        var reclaimed: usize = 0;
        switch (self.gc_strategy) {
            .mark_sweep, .generational => reclaimed = self.sweepMarked(&summary),
            .bump => reclaimed = self.collectBump(&summary),
        }
        summary.timings.total_ns = total_timer.read();
        self.emitPhase(.done, summary.timings.total_ns);
        self.event_sink.emit(.{ .collect = .{
            .phase = .end,
            .strategy = self.collectStrategy(),
            .root_count = summary.root_count,
            .reclaimed = reclaimed,
        } });
        self.event_sink.emit(.{ .gc_snapshot = summary });
    }

    pub fn collectMinor(self: *Collector) void {
        var total_timer = std.time.Timer.start() catch unreachable;
        var root_timer = std.time.Timer.start() catch unreachable;
        var summary = GcSnapshotEvent{
            .strategy = self.collectStrategy(),
            .root_count = self.rootCount(),
        };

        self.event_sink.emit(.{ .collect = .{
            .phase = .start,
            .strategy = self.collectStrategy(),
            .root_count = summary.root_count,
            .reclaimed = 0,
        } });
        _ = self.emitRootProviders();
        summary.timings.root_enumeration_ns = root_timer.read();
        self.emitPhase(.enumerate_roots, summary.timings.root_enumeration_ns);

        self.traceMinorGenerational(&summary);

        var timer = std.time.Timer.start() catch unreachable;
        summary.weak_processed = self.hooks.processWeak(self);
        summary.timings.weak_ns = timer.read();
        self.emitPhase(.weak, summary.timings.weak_ns);

        timer = std.time.Timer.start() catch unreachable;
        summary.finalizers_ready = self.hooks.processFinalizers(self);
        summary.timings.finalizers_ns = timer.read();
        self.emitPhase(.finalizers, summary.timings.finalizers_ns);
        const reclaimed = self.sweepMinorGenerational(&summary);
        summary.timings.total_ns = total_timer.read();
        self.emitPhase(.done, summary.timings.total_ns);
        self.event_sink.emit(.{ .collect = .{
            .phase = .end,
            .strategy = self.collectStrategy(),
            .root_count = summary.root_count,
            .reclaimed = reclaimed,
        } });
        self.event_sink.emit(.{ .gc_snapshot = summary });
    }

    fn traceMarkSweep(self: *Collector, summary: *GcSnapshotEvent) void {
        var timer = std.time.Timer.start() catch unreachable;
        var worklist = std.ArrayListUnmanaged(value.HeapRef){};
        defer worklist.deinit(self.heap_store.hostAllocator());
        var ctx = MarkVisitorContext{
            .collector = self,
            .worklist = &worklist,
            .scope = .all,
        };
        const visitor = RootVisitor{
            .ctx = &ctx,
            .visit_fn = visitMarkRoot,
        };
        for (self.root_providers) |provider| {
            provider.visit(visitor);
        }
        self.drainMarkWorklist(&worklist, .all);
        summary.timings.mark_ns = timer.read();
        self.emitPhase(.mark, summary.timings.mark_ns);
    }

    fn collectBump(self: *Collector, summary: *GcSnapshotEvent) usize {
        var timer = std.time.Timer.start() catch unreachable;
        const reclaimed = self.emitLiveReclaims(summary);
        summary.timings.sweep_ns = timer.read();
        self.emitPhase(.mark, 0);
        self.emitPhase(.sweep, summary.timings.sweep_ns);
        if (self.fixed_arena_buffer) |buffer| {
            self.fixed_arena.* = std.heap.FixedBufferAllocator.init(buffer);
            self.heap_store.clear(true);
            self.captureSpaceStats(summary);
            return reclaimed;
        }

        self.heap_store.clear(false);
        self.captureSpaceStats(summary);
        return reclaimed;
    }

    fn objectFrom(self: *Collector, block_value: Value) ?*Object {
        const handle = block_value.asHeapRef() orelse return null;
        return self.heap_store.get(handle);
    }

    pub fn markValue(self: *Collector, block_value: Value) bool {
        var worklist = std.ArrayListUnmanaged(value.HeapRef){};
        defer worklist.deinit(self.heap_store.hostAllocator());
        const seeded = self.enqueueForMark(&worklist, block_value, .all);
        self.drainMarkWorklist(&worklist, .all);
        return seeded;
    }

    pub fn isMarkedValue(self: *const Collector, rooted: Value) bool {
        const handle = rooted.asHeapRef() orelse return false;
        const obj = self.heap_store.get(handle) orelse return false;
        return obj.marked;
    }

    fn emitLiveReclaims(self: *Collector, summary: *GcSnapshotEvent) usize {
        const Context = struct {
            collector: *Collector,
            summary: *GcSnapshotEvent,
            reclaimed: usize = 0,

            fn visit(ctx: *@This(), handle: value.HeapRef, _: Space, obj: *const Object) void {
                const kind = obj.kind().?;
                ctx.summary.reclaimed.bump(kind);
                ctx.collector.event_sink.emit(.{ .reclaim = .{
                    .handle = handle,
                    .kind = kind,
                } });
                if (ctx.collector.memprof) |memprof| {
                    memprof.noteReclaim(handle);
                }
                ctx.reclaimed += 1;
            }
        };

        var ctx = Context{
            .collector = self,
            .summary = summary,
        };
        self.heap_store.visitLiveConst(&ctx, Context.visit);
        return ctx.reclaimed;
    }

    fn traceMinorGenerational(self: *Collector, summary: *GcSnapshotEvent) void {
        var timer = std.time.Timer.start() catch unreachable;
        var worklist = std.ArrayListUnmanaged(value.HeapRef){};
        defer worklist.deinit(self.heap_store.hostAllocator());
        var ctx = MarkVisitorContext{
            .collector = self,
            .worklist = &worklist,
            .scope = .nursery_only,
        };
        const visitor = RootVisitor{
            .ctx = &ctx,
            .visit_fn = visitMarkRoot,
        };
        for (self.root_providers) |provider| {
            provider.visit(visitor);
        }
        if (self.remembered_set) |set| {
            for (set.targetsSlice()) |target| {
                const obj = self.heap_store.get(target) orelse continue;
                const fields = obj.tupleFields() orelse continue;
                for (fields) |field| {
                    _ = self.enqueueForMark(&worklist, field, .nursery_only);
                }
            }
        }
        self.drainMarkWorklist(&worklist, .nursery_only);
        summary.timings.mark_ns = timer.read();
        self.emitPhase(.mark, summary.timings.mark_ns);
    }

    fn sweepMarked(self: *Collector, summary: *GcSnapshotEvent) usize {
        var timer = std.time.Timer.start() catch unreachable;
        const fixed_arena = self.fixed_arena_buffer != null;
        const Context = struct {
            collector: *Collector,
            summary: *GcSnapshotEvent,

            fn onReclaim(ctx: *@This(), handle: value.HeapRef, kind: ObjectKind) void {
                ctx.summary.reclaimed.bump(kind);
                ctx.collector.event_sink.emit(.{ .reclaim = .{
                    .handle = handle,
                    .kind = kind,
                } });
                if (ctx.collector.memprof) |memprof| {
                    memprof.noteReclaim(handle);
                }
            }

            fn onKeep(ctx: *@This(), obj: *Object, kind: ObjectKind, _: Space) void {
                ctx.summary.marked.bump(kind);
                obj.marked = false;
            }
        };

        var ctx = Context{
            .collector = self,
            .summary = summary,
        };
        const reclaimed = self.heap_store.sweepMarked(fixed_arena, &ctx, Context.onReclaim, Context.onKeep);
        if (self.remembered_set) |set| set.compact(self.heap_store);
        self.captureSpaceStats(summary);
        summary.timings.sweep_ns = timer.read();
        self.emitPhase(.sweep, summary.timings.sweep_ns);
        return reclaimed;
    }

    fn sweepMinorGenerational(self: *Collector, summary: *GcSnapshotEvent) usize {
        var timer = std.time.Timer.start() catch unreachable;
        const fixed_arena = self.fixed_arena_buffer != null;
        const Context = struct {
            collector: *Collector,
            summary: *GcSnapshotEvent,

            fn onReclaim(ctx: *@This(), handle: value.HeapRef, kind: ObjectKind) void {
                ctx.summary.reclaimed.bump(kind);
                ctx.collector.event_sink.emit(.{ .reclaim = .{
                    .handle = handle,
                    .kind = kind,
                } });
                if (ctx.collector.memprof) |memprof| {
                    memprof.noteReclaim(handle);
                }
            }

            fn onPromote(ctx: *@This(), handle: value.HeapRef, obj: *Object, kind: ObjectKind) void {
                obj.marked = false;
                ctx.summary.promoted.bump(kind);
                ctx.summary.promoted_allocation_units += obj.allocationCostUnits();
                ctx.summary.marked.bump(kind);
                if (ctx.collector.memprof) |memprof| {
                    memprof.notePromotion(handle, .major);
                }
            }
        };

        var ctx = Context{
            .collector = self,
            .summary = summary,
        };
        const reclaimed = self.heap_store.sweepNurseryMarked(fixed_arena, &ctx, Context.onReclaim, Context.onPromote);
        if (self.remembered_set) |set| set.compact(self.heap_store);
        self.captureSpaceStats(summary);
        summary.timings.sweep_ns = timer.read();
        self.emitPhase(.sweep, summary.timings.sweep_ns);
        return reclaimed;
    }

    fn collectStrategy(self: *const Collector) event_sink.CollectStrategy {
        return switch (self.gc_strategy) {
            .mark_sweep => .mark_sweep,
            .generational => .generational,
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

    fn emitRootProviders(self: *Collector) usize {
        var count: usize = 0;
        for (self.root_providers) |provider| {
            const provider_count = provider.count();
            count += provider_count;
            self.event_sink.emit(.{ .root_provider = .{
                .name = provider.name,
                .count = provider_count,
            } });
        }
        return count;
    }

    fn emitPhase(self: *Collector, phase: GcPhase, elapsed_ns: u64) void {
        self.event_sink.emit(.{ .gc_phase = .{
            .strategy = self.collectStrategy(),
            .phase = phase,
            .elapsed_ns = elapsed_ns,
        } });
    }

    fn captureSpaceStats(self: *Collector, summary: *GcSnapshotEvent) void {
        const nursery = self.heap_store.spaceStats(.nursery);
        const major = self.heap_store.spaceStats(.major);
        summary.nursery_objects = nursery.objects;
        summary.nursery_allocation_units = nursery.allocation_units;
        summary.major_objects = major.objects;
        summary.major_allocation_units = major.allocation_units;
    }

    fn enqueueForMark(
        self: *Collector,
        worklist: *std.ArrayListUnmanaged(value.HeapRef),
        rooted: Value,
        scope: MarkScope,
    ) bool {
        const handle = rooted.asHeapRef() orelse return false;
        const obj = self.heap_store.get(handle) orelse return false;
        switch (scope) {
            .all => {},
            .nursery_only => {
                const space = self.heap_store.spaceOf(handle) orelse return false;
                if (space != .nursery) return false;
            },
        }
        if (obj.marked) return false;
        obj.marked = true;
        worklist.append(self.heap_store.hostAllocator(), handle) catch {
            @panic("zort: out of memory while growing mark stack");
        };
        return true;
    }

    fn drainMarkWorklist(
        self: *Collector,
        worklist: *std.ArrayListUnmanaged(value.HeapRef),
        scope: MarkScope,
    ) void {
        while (worklist.items.len > 0) {
            const handle = worklist.pop() orelse unreachable;
            const obj = self.heap_store.get(handle) orelse continue;
            if (obj.tupleFields()) |fields| {
                for (fields) |child| {
                    _ = self.enqueueForMark(worklist, child, scope);
                }
            }
        }
    }

    fn visitMarkRoot(ctx: ?*anyopaque, rooted: Value) void {
        const mark_ctx: *MarkVisitorContext = @ptrCast(@alignCast(ctx.?));
        _ = mark_ctx.collector.enqueueForMark(mark_ctx.worklist, rooted, mark_ctx.scope);
    }
};

test "collector: mark-sweep keeps rooted graph and reclaims unreachable objects" {
    const mutator_mod = @import("mutator.zig");

    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var roots = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer roots.deinit();
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;

    var writer = mutator_mod.Mutator.init(std.testing.allocator, &heap, EventSink.noop(), null);
    var root = try writer.allocTuple(1);
    const child = try writer.allocTuple(0);
    _ = try writer.allocTuple(0);
    try writer.writeField(root, 0, child);

    try roots.register(&root);

    var providers = [_]RootProvider{roots.provider()};
    var collector = Collector.init(&heap, null, null, providers[0..], &fixed_arena, null, .mark_sweep, EventSink.noop(), Collector.PhaseHooks.noop());
    collector.collect();
    try std.testing.expectEqual(@as(usize, 2), heap.count());

    roots.unregister(&root);
    providers[0] = roots.provider();
    collector = Collector.init(&heap, null, null, providers[0..], &fixed_arena, null, .mark_sweep, EventSink.noop(), Collector.PhaseHooks.noop());
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
        var writer = mutator_mod.Mutator.init(fixed_arena.?.allocator(), &heap, EventSink.noop(), null);
        _ = try writer.allocTuple(2);
    }
    try std.testing.expectEqual(@as(usize, 1), heap.count());

    var providers = [_]RootProvider{roots.provider()};
    var collector = Collector.init(&heap, null, null, providers[0..], &fixed_arena, arena_bytes[0..], .bump, EventSink.noop(), Collector.PhaseHooks.noop());
    collector.collect();
    try std.testing.expectEqual(@as(usize, 0), heap.count());

    {
        var writer = mutator_mod.Mutator.init(fixed_arena.?.allocator(), &heap, EventSink.noop(), null);
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

    var writer = mutator_mod.Mutator.init(std.testing.allocator, &heap, EventSink.noop(), null);
    _ = try writer.allocTuple(0);

    var providers = [_]RootProvider{roots.provider()};
    var gc = Collector.init(&heap, null, null, providers[0..], &fixed_arena, null, .mark_sweep, recorder.sink(), Collector.PhaseHooks.noop());
    gc.collect();

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.collections);
    try std.testing.expectEqual(@as(usize, 1), counters.reclaims);
}

test "collector: phase hooks run between tracing and completion" {
    const mutator_mod = @import("mutator.zig");

    const Hooks = struct {
        var weak_calls: usize = 0;
        var finalizer_calls: usize = 0;

        fn processWeak(_: ?*anyopaque, _: *Collector) usize {
            weak_calls += 1;
            return 2;
        }

        fn processFinalizers(_: ?*anyopaque, _: *Collector) usize {
            finalizer_calls += 1;
            return 1;
        }
    };
    Hooks.weak_calls = 0;
    Hooks.finalizer_calls = 0;

    var trace = event_sink.TraceRecorder.init(std.testing.allocator, .{
        .capture_events = true,
    });
    defer trace.deinit();
    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    var roots = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer roots.deinit();
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;

    var writer = mutator_mod.Mutator.init(std.testing.allocator, &heap, EventSink.noop(), null);
    var rooted = try writer.allocTuple(0);
    try roots.register(&rooted);

    var providers = [_]RootProvider{roots.provider()};
    var gc = Collector.init(&heap, null, null, providers[0..], &fixed_arena, null, .mark_sweep, trace.sink(), .{
        .process_weak_fn = Hooks.processWeak,
        .process_finalizers_fn = Hooks.processFinalizers,
    });
    gc.collect();

    try std.testing.expectEqual(@as(usize, 1), Hooks.weak_calls);
    try std.testing.expectEqual(@as(usize, 1), Hooks.finalizer_calls);
    try std.testing.expectEqual(@as(usize, 2), trace.last_gc_snapshot.?.weak_processed);
    try std.testing.expectEqual(@as(usize, 1), trace.last_gc_snapshot.?.finalizers_ready);
}

test "collector: generational minor collection promotes reachable nursery objects" {
    const mutator_mod = @import("mutator.zig");

    var trace = event_sink.TraceRecorder.init(std.testing.allocator, .{
        .capture_events = true,
    });
    defer trace.deinit();
    var remembered = RememberedSet.init(std.testing.allocator);
    defer remembered.deinit();
    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    heap.configureNursery(.{
        .enabled = true,
        .max_object_units = 2,
    });
    var roots = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer roots.deinit();
    var fixed_arena: ?std.heap.FixedBufferAllocator = null;

    var writer = mutator_mod.Mutator.init(std.testing.allocator, &heap, EventSink.noop(), &remembered);
    var root = try writer.allocTuple(1);
    try roots.register(&root);
    const child = try writer.allocTuple(0);
    try writer.initField(root, 0, child);
    _ = try writer.allocTuple(0);

    try std.testing.expectEqual(@as(?Space, .nursery), heap.spaceOf(root.asHeapRef().?));
    try std.testing.expectEqual(@as(?Space, .nursery), heap.spaceOf(child.asHeapRef().?));
    try std.testing.expectEqual(@as(usize, 3), heap.nurseryCount());

    var providers = [_]RootProvider{roots.provider()};
    var gc = Collector.init(&heap, &remembered, null, providers[0..], &fixed_arena, null, .generational, trace.sink(), Collector.PhaseHooks.noop());
    gc.collectMinor();

    try std.testing.expectEqual(@as(usize, 0), heap.nurseryCount());
    try std.testing.expectEqual(@as(?Space, .major), heap.spaceOf(root.asHeapRef().?));
    try std.testing.expectEqual(@as(?Space, .major), heap.spaceOf(child.asHeapRef().?));
    try std.testing.expectEqual(@as(usize, 2), heap.count());
    try std.testing.expectEqual(@as(usize, 2), trace.last_gc_snapshot.?.promoted.tuple);
    try std.testing.expectEqual(@as(usize, 1), trace.last_gc_snapshot.?.promoted_allocation_units);
    try std.testing.expectEqual(@as(usize, 0), trace.last_gc_snapshot.?.nursery_objects);
    try std.testing.expectEqual(@as(usize, 0), trace.last_gc_snapshot.?.nursery_allocation_units);
    try std.testing.expectEqual(@as(usize, 2), trace.last_gc_snapshot.?.major_objects);
    try std.testing.expectEqual(@as(usize, 1), trace.last_gc_snapshot.?.major_allocation_units);
}
