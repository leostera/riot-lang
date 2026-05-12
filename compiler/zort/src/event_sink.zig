const std = @import("std");
const heap_store = @import("heap_store.zig");
const value = @import("value.zig");

pub const HeapRef = value.HeapRef;
pub const ObjectKind = heap_store.ObjectKind;

pub const HandleRef = struct {
    index: u32,
    generation: u32,
};

pub const WritePhase = enum {
    initialize,
    mutate,
};

pub const RootAction = enum {
    register,
    unregister,
};

pub const CollectPhase = enum {
    start,
    end,
};

pub const CollectStrategy = enum {
    mark_sweep,
    generational,
    bump,
};

pub const MemprofAction = enum {
    sampled_alloc,
    promoted,
    reclaimed,
};

pub const ControlAction = enum {
    fiber_activate,
    fiber_enqueue,
    fiber_yield,
    fiber_park,
    fiber_unpark,
    continuation_capture,
    continuation_resume,
    continuation_drop,
    effect_unhandled,
    callback_enter,
    callback_exit,
    stw_request,
    stw_pause,
    stw_resume,
};

pub const ObjectKindCounts = struct {
    tuple: usize = 0,
    string: usize = 0,
    boxed_i64: usize = 0,
    boxed_f64: usize = 0,
    custom: usize = 0,

    pub fn bump(self: *ObjectKindCounts, kind: ObjectKind) void {
        switch (kind) {
            .tuple => self.tuple +%= 1,
            .string => self.string +%= 1,
            .boxed_i64 => self.boxed_i64 +%= 1,
            .boxed_f64 => self.boxed_f64 +%= 1,
            .custom => self.custom +%= 1,
        }
    }
};

pub const CollectTimings = struct {
    root_enumeration_ns: u64 = 0,
    mark_ns: u64 = 0,
    weak_ns: u64 = 0,
    finalizers_ns: u64 = 0,
    sweep_ns: u64 = 0,
    total_ns: u64 = 0,
};

pub const GcPhase = enum {
    enumerate_roots,
    mark,
    weak,
    finalizers,
    sweep,
    done,
};

pub const AllocEvent = struct {
    handle: HeapRef,
    kind: ObjectKind,
    payload_bytes: usize,
    storage_bytes: usize,
    scan_words: usize,
    allocation_cost_units: usize,
};

pub const FieldWriteEvent = struct {
    target: HeapRef,
    index: usize,
    phase: WritePhase,
};

pub const BytesWriteEvent = struct {
    target: HeapRef,
    len: usize,
    phase: WritePhase,
};

pub const BarrierEvent = struct {
    target: HeapRef,
    value_is_block: bool,
};

pub const RootEvent = struct {
    action: RootAction,
    is_block: bool,
};

pub const CollectEvent = struct {
    phase: CollectPhase,
    strategy: CollectStrategy,
    root_count: usize,
    reclaimed: usize,
};

pub const ReclaimEvent = struct {
    handle: HeapRef,
    kind: ObjectKind,
};

pub const MemprofEvent = struct {
    action: MemprofAction,
    handle: HeapRef,
    sample_ordinal: u64,
    kind: ObjectKind,
    payload_bytes: usize,
    storage_bytes: usize,
    scan_words: usize,
    allocation_cost_units: usize,
    space: heap_store.Space,
    promotion_count: usize = 0,
    backtrace_depth: usize = 0,
};

pub const RootProviderEvent = struct {
    name: []const u8,
    count: usize,
};

pub const GcSnapshotEvent = struct {
    strategy: CollectStrategy,
    root_count: usize,
    marked: ObjectKindCounts = .{},
    promoted: ObjectKindCounts = .{},
    reclaimed: ObjectKindCounts = .{},
    promoted_allocation_units: usize = 0,
    weak_processed: usize = 0,
    finalizers_ready: usize = 0,
    nursery_objects: usize = 0,
    nursery_allocation_units: usize = 0,
    major_objects: usize = 0,
    major_allocation_units: usize = 0,
    timings: CollectTimings = .{},
};

pub const GcPhaseEvent = struct {
    strategy: CollectStrategy,
    phase: GcPhase,
    elapsed_ns: u64,
};

pub const ControlEvent = struct {
    action: ControlAction,
    site_id: u32 = 0,
    effect: ?u32 = null,
    fiber: ?HandleRef = null,
    continuation: ?HandleRef = null,
    handler_fiber: ?HandleRef = null,
    handler_index: ?usize = null,
    parent_depth: usize = 0,
};

pub const Event = union(enum) {
    alloc: AllocEvent,
    field_write: FieldWriteEvent,
    bytes_write: BytesWriteEvent,
    barrier: BarrierEvent,
    root: RootEvent,
    root_provider: RootProviderEvent,
    collect: CollectEvent,
    gc_phase: GcPhaseEvent,
    gc_snapshot: GcSnapshotEvent,
    reclaim: ReclaimEvent,
    memprof: MemprofEvent,
    control: ControlEvent,
};

pub const Counters = struct {
    allocations: usize = 0,
    field_writes: usize = 0,
    bytes_writes: usize = 0,
    barrier_records: usize = 0,
    root_registrations: usize = 0,
    root_unregistrations: usize = 0,
    collections: usize = 0,
    reclaims: usize = 0,
    fiber_activations: usize = 0,
    continuation_captures: usize = 0,
    continuation_resumes: usize = 0,
    continuation_drops: usize = 0,
    unhandled_effects: usize = 0,
    memprof_samples: usize = 0,
    memprof_promotions: usize = 0,
    memprof_reclaims: usize = 0,

    pub fn diff(after: Counters, before: Counters) Counters {
        return .{
            .allocations = after.allocations - before.allocations,
            .field_writes = after.field_writes - before.field_writes,
            .bytes_writes = after.bytes_writes - before.bytes_writes,
            .barrier_records = after.barrier_records - before.barrier_records,
            .root_registrations = after.root_registrations - before.root_registrations,
            .root_unregistrations = after.root_unregistrations - before.root_unregistrations,
            .collections = after.collections - before.collections,
            .reclaims = after.reclaims - before.reclaims,
            .fiber_activations = after.fiber_activations - before.fiber_activations,
            .continuation_captures = after.continuation_captures - before.continuation_captures,
            .continuation_resumes = after.continuation_resumes - before.continuation_resumes,
            .continuation_drops = after.continuation_drops - before.continuation_drops,
            .unhandled_effects = after.unhandled_effects - before.unhandled_effects,
            .memprof_samples = after.memprof_samples - before.memprof_samples,
            .memprof_promotions = after.memprof_promotions - before.memprof_promotions,
            .memprof_reclaims = after.memprof_reclaims - before.memprof_reclaims,
        };
    }
};

pub const Recorder = struct {
    counters: Counters = .{},
    last_collect_root_count: usize = 0,
    last_collect_reclaimed: usize = 0,
    last_gc_snapshot: ?GcSnapshotEvent = null,
    last_root_providers: [8]RootProviderEvent = undefined,
    last_root_provider_count: usize = 0,

    pub fn sink(self: *Recorder) EventSink {
        return .{
            .ctx = self,
            .on_event = onEvent,
        };
    }

    pub fn snapshot(self: *const Recorder) Counters {
        return self.counters;
    }

    fn onEvent(ctx: ?*anyopaque, event: Event) void {
        const self: *Recorder = @ptrCast(@alignCast(ctx.?));
        switch (event) {
            .alloc => self.counters.allocations +%= 1,
            .field_write => self.counters.field_writes +%= 1,
            .bytes_write => self.counters.bytes_writes +%= 1,
            .barrier => self.counters.barrier_records +%= 1,
            .root => |root_event| switch (root_event.action) {
                .register => self.counters.root_registrations +%= 1,
                .unregister => self.counters.root_unregistrations +%= 1,
            },
            .root_provider => |provider_event| {
                if (self.last_root_provider_count < self.last_root_providers.len) {
                    self.last_root_providers[self.last_root_provider_count] = provider_event;
                    self.last_root_provider_count += 1;
                }
            },
            .collect => |collect_event| {
                if (collect_event.phase == .start) {
                    self.last_root_provider_count = 0;
                    self.last_gc_snapshot = null;
                }
                if (collect_event.phase == .end) {
                    self.counters.collections +%= 1;
                    self.last_collect_root_count = collect_event.root_count;
                    self.last_collect_reclaimed = collect_event.reclaimed;
                }
            },
            .gc_snapshot => |gc_snapshot| self.last_gc_snapshot = gc_snapshot,
            .gc_phase => {},
            .reclaim => self.counters.reclaims +%= 1,
            .memprof => |memprof_event| switch (memprof_event.action) {
                .sampled_alloc => self.counters.memprof_samples +%= 1,
                .promoted => self.counters.memprof_promotions +%= 1,
                .reclaimed => self.counters.memprof_reclaims +%= 1,
            },
            .control => |control_event| switch (control_event.action) {
                .fiber_activate => self.counters.fiber_activations +%= 1,
                .fiber_enqueue, .fiber_yield, .fiber_park, .fiber_unpark => {},
                .continuation_capture => self.counters.continuation_captures +%= 1,
                .continuation_resume => self.counters.continuation_resumes +%= 1,
                .continuation_drop => self.counters.continuation_drops +%= 1,
                .effect_unhandled => self.counters.unhandled_effects +%= 1,
                .callback_enter, .callback_exit, .stw_request, .stw_pause, .stw_resume => {},
            },
        }
    }
};

pub const TraceEntry = struct {
    timestamp_ms: i64,
    event: Event,
};

pub const ObjectLastEvent = union(enum) {
    alloc: AllocEvent,
    field_write: FieldWriteEvent,
    bytes_write: BytesWriteEvent,
    reclaim: ReclaimEvent,
};

pub const TraceRecorder = struct {
    pub const Options = struct {
        capture_events: bool = false,
        track_object_events: bool = false,
    };

    allocator: std.mem.Allocator,
    options: Options,
    counters: Counters = .{},
    last_collect_root_count: usize = 0,
    last_collect_reclaimed: usize = 0,
    last_gc_snapshot: ?GcSnapshotEvent = null,
    root_providers: std.ArrayListUnmanaged(RootProviderEvent) = .{},
    traces: std.ArrayListUnmanaged(TraceEntry) = .{},
    object_events: std.AutoHashMapUnmanaged(u64, ObjectLastEvent) = .{},

    pub fn init(allocator: std.mem.Allocator, options: Options) TraceRecorder {
        return .{
            .allocator = allocator,
            .options = options,
        };
    }

    pub fn deinit(self: *TraceRecorder) void {
        self.root_providers.deinit(self.allocator);
        self.traces.deinit(self.allocator);
        self.object_events.deinit(self.allocator);
    }

    pub fn sink(self: *TraceRecorder) EventSink {
        return .{
            .ctx = self,
            .on_event = onEvent,
        };
    }

    pub fn snapshot(self: *const TraceRecorder) Counters {
        return self.counters;
    }

    pub fn traceEntries(self: *const TraceRecorder) []const TraceEntry {
        return self.traces.items;
    }

    pub fn rootProviderEntries(self: *const TraceRecorder) []const RootProviderEvent {
        return self.root_providers.items;
    }

    pub fn lastObjectEvent(self: *const TraceRecorder, handle: HeapRef) ?ObjectLastEvent {
        return self.object_events.get(handleKey(handle));
    }

    pub fn clearCase(self: *TraceRecorder) void {
        self.last_collect_root_count = 0;
        self.last_collect_reclaimed = 0;
        self.last_gc_snapshot = null;
        self.root_providers.clearRetainingCapacity();
        self.traces.clearRetainingCapacity();
    }

    fn onEvent(ctx: ?*anyopaque, event: Event) void {
        const self: *TraceRecorder = @ptrCast(@alignCast(ctx.?));
        if (self.options.capture_events) {
            self.traces.append(self.allocator, .{
                .timestamp_ms = std.time.milliTimestamp(),
                .event = event,
            }) catch @panic("zort: out of memory while recording trace");
        }

        switch (event) {
            .alloc => |alloc_event| {
                self.counters.allocations +%= 1;
                self.trackObjectEvent(alloc_event.handle, .{ .alloc = alloc_event });
            },
            .field_write => |field_event| {
                self.counters.field_writes +%= 1;
                self.trackObjectEvent(field_event.target, .{ .field_write = field_event });
            },
            .bytes_write => |bytes_event| {
                self.counters.bytes_writes +%= 1;
                self.trackObjectEvent(bytes_event.target, .{ .bytes_write = bytes_event });
            },
            .barrier => self.counters.barrier_records +%= 1,
            .root => |root_event| switch (root_event.action) {
                .register => self.counters.root_registrations +%= 1,
                .unregister => self.counters.root_unregistrations +%= 1,
            },
            .root_provider => |provider_event| {
                self.root_providers.append(self.allocator, provider_event) catch {
                    @panic("zort: out of memory while recording root provider");
                };
            },
            .collect => |collect_event| {
                if (collect_event.phase == .start) {
                    self.root_providers.clearRetainingCapacity();
                    self.last_gc_snapshot = null;
                } else {
                    self.counters.collections +%= 1;
                    self.last_collect_root_count = collect_event.root_count;
                    self.last_collect_reclaimed = collect_event.reclaimed;
                }
            },
            .gc_phase => {},
            .gc_snapshot => |gc_snapshot| self.last_gc_snapshot = gc_snapshot,
            .reclaim => |reclaim_event| {
                self.counters.reclaims +%= 1;
                self.trackObjectEvent(reclaim_event.handle, .{ .reclaim = reclaim_event });
            },
            .memprof => |memprof_event| switch (memprof_event.action) {
                .sampled_alloc => self.counters.memprof_samples +%= 1,
                .promoted => self.counters.memprof_promotions +%= 1,
                .reclaimed => self.counters.memprof_reclaims +%= 1,
            },
            .control => |control_event| switch (control_event.action) {
                .fiber_activate => self.counters.fiber_activations +%= 1,
                .fiber_enqueue, .fiber_yield, .fiber_park, .fiber_unpark => {},
                .continuation_capture => self.counters.continuation_captures +%= 1,
                .continuation_resume => self.counters.continuation_resumes +%= 1,
                .continuation_drop => self.counters.continuation_drops +%= 1,
                .effect_unhandled => self.counters.unhandled_effects +%= 1,
                .callback_enter, .callback_exit, .stw_request, .stw_pause, .stw_resume => {},
            },
        }
    }

    fn trackObjectEvent(self: *TraceRecorder, handle: HeapRef, event: ObjectLastEvent) void {
        if (!self.options.track_object_events) return;
        self.object_events.put(self.allocator, handleKey(handle), event) catch {
            @panic("zort: out of memory while recording object event");
        };
    }

    fn handleKey(handle: HeapRef) u64 {
        return (@as(u64, handle.index) << 32) | @as(u64, handle.generation);
    }
};

pub const EventSink = struct {
    ctx: ?*anyopaque,
    on_event: *const fn (?*anyopaque, Event) void,

    pub fn noop() EventSink {
        return .{
            .ctx = null,
            .on_event = noopEvent,
        };
    }

    pub fn emit(self: EventSink, event: Event) void {
        self.on_event(self.ctx, event);
    }

    fn noopEvent(_: ?*anyopaque, _: Event) void {}
};

test "event_sink: recorder tracks counters by event kind" {
    var recorder = Recorder{};
    const sink = recorder.sink();

    sink.emit(.{ .alloc = .{
        .handle = .{ .index = 1, .generation = 1 },
        .kind = .tuple,
        .payload_bytes = 16,
        .storage_bytes = 16,
        .scan_words = 2,
        .allocation_cost_units = 2,
    } });
    sink.emit(.{ .field_write = .{
        .target = .{ .index = 1, .generation = 1 },
        .index = 0,
        .phase = .mutate,
    } });
    sink.emit(.{ .bytes_write = .{
        .target = .{ .index = 2, .generation = 1 },
        .len = 4,
        .phase = .initialize,
    } });
    sink.emit(.{ .barrier = .{
        .target = .{ .index = 1, .generation = 1 },
        .value_is_block = true,
    } });
    sink.emit(.{ .collect = .{
        .phase = .start,
        .strategy = .mark_sweep,
        .root_count = 1,
        .reclaimed = 0,
    } });
    sink.emit(.{ .gc_phase = .{
        .strategy = .mark_sweep,
        .phase = .enumerate_roots,
        .elapsed_ns = 3,
    } });
    sink.emit(.{ .root = .{ .action = .register, .is_block = true } });
    sink.emit(.{ .root_provider = .{ .name = "root_registry", .count = 1 } });
    sink.emit(.{ .reclaim = .{
        .handle = .{ .index = 1, .generation = 1 },
        .kind = .tuple,
    } });
    sink.emit(.{ .memprof = .{
        .action = .sampled_alloc,
        .handle = .{ .index = 1, .generation = 1 },
        .sample_ordinal = 1,
        .kind = .tuple,
        .payload_bytes = 16,
        .storage_bytes = 16,
        .scan_words = 2,
        .allocation_cost_units = 2,
        .space = .nursery,
        .backtrace_depth = 2,
    } });
    sink.emit(.{ .memprof = .{
        .action = .promoted,
        .handle = .{ .index = 1, .generation = 1 },
        .sample_ordinal = 1,
        .kind = .tuple,
        .payload_bytes = 16,
        .storage_bytes = 16,
        .scan_words = 2,
        .allocation_cost_units = 2,
        .space = .major,
        .promotion_count = 1,
        .backtrace_depth = 2,
    } });
    sink.emit(.{ .memprof = .{
        .action = .reclaimed,
        .handle = .{ .index = 1, .generation = 1 },
        .sample_ordinal = 1,
        .kind = .tuple,
        .payload_bytes = 16,
        .storage_bytes = 16,
        .scan_words = 2,
        .allocation_cost_units = 2,
        .space = .major,
        .promotion_count = 1,
        .backtrace_depth = 2,
    } });
    sink.emit(.{ .control = .{
        .action = .continuation_capture,
        .effect = 7,
        .site_id = 1,
    } });
    sink.emit(.{ .gc_snapshot = .{
        .strategy = .mark_sweep,
        .root_count = 1,
        .reclaimed = .{ .tuple = 1 },
        .promoted = .{ .boxed_i64 = 1 },
        .promoted_allocation_units = 3,
        .nursery_objects = 2,
        .nursery_allocation_units = 5,
        .major_objects = 4,
        .major_allocation_units = 9,
        .timings = .{ .total_ns = 12 },
    } });
    sink.emit(.{ .collect = .{
        .phase = .end,
        .strategy = .mark_sweep,
        .root_count = 1,
        .reclaimed = 1,
    } });

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.allocations);
    try std.testing.expectEqual(@as(usize, 1), counters.field_writes);
    try std.testing.expectEqual(@as(usize, 1), counters.bytes_writes);
    try std.testing.expectEqual(@as(usize, 1), counters.barrier_records);
    try std.testing.expectEqual(@as(usize, 1), counters.root_registrations);
    try std.testing.expectEqual(@as(usize, 1), counters.reclaims);
    try std.testing.expectEqual(@as(usize, 1), counters.collections);
    try std.testing.expectEqual(@as(usize, 1), counters.continuation_captures);
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_samples);
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_promotions);
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_reclaims);
    try std.testing.expectEqualStrings("root_registry", recorder.last_root_providers[0].name);
    try std.testing.expectEqual(@as(usize, 1), recorder.last_collect_root_count);
    try std.testing.expectEqual(@as(usize, 1), recorder.last_collect_reclaimed);
    try std.testing.expectEqual(@as(usize, 1), recorder.last_gc_snapshot.?.reclaimed.tuple);
    try std.testing.expectEqual(@as(usize, 1), recorder.last_gc_snapshot.?.promoted.boxed_i64);
    try std.testing.expectEqual(@as(usize, 3), recorder.last_gc_snapshot.?.promoted_allocation_units);
    try std.testing.expectEqual(@as(usize, 2), recorder.last_gc_snapshot.?.nursery_objects);
    try std.testing.expectEqual(@as(usize, 5), recorder.last_gc_snapshot.?.nursery_allocation_units);
    try std.testing.expectEqual(@as(usize, 4), recorder.last_gc_snapshot.?.major_objects);
    try std.testing.expectEqual(@as(usize, 9), recorder.last_gc_snapshot.?.major_allocation_units);
}

test "event_sink: trace recorder stores object history and gc snapshot" {
    var trace = TraceRecorder.init(std.testing.allocator, .{
        .capture_events = true,
        .track_object_events = true,
    });
    defer trace.deinit();

    const sink = trace.sink();
    const handle = HeapRef{ .index = 2, .generation = 9 };
    sink.emit(.{ .alloc = .{
        .handle = handle,
        .kind = .boxed_i64,
        .payload_bytes = 8,
        .storage_bytes = 0,
        .scan_words = 0,
        .allocation_cost_units = 1,
    } });
    sink.emit(.{ .field_write = .{
        .target = handle,
        .index = 0,
        .phase = .mutate,
    } });
    sink.emit(.{ .barrier = .{
        .target = handle,
        .value_is_block = true,
    } });
    sink.emit(.{ .collect = .{
        .phase = .start,
        .strategy = .mark_sweep,
        .root_count = 2,
        .reclaimed = 0,
    } });
    sink.emit(.{ .gc_phase = .{
        .strategy = .mark_sweep,
        .phase = .mark,
        .elapsed_ns = 7,
    } });
    sink.emit(.{ .root_provider = .{ .name = "control_kernel", .count = 2 } });
    sink.emit(.{ .gc_snapshot = .{
        .strategy = .mark_sweep,
        .root_count = 2,
        .marked = .{ .boxed_i64 = 1 },
        .promoted = .{ .tuple = 1 },
        .promoted_allocation_units = 2,
        .nursery_objects = 1,
        .nursery_allocation_units = 2,
        .major_objects = 3,
        .major_allocation_units = 4,
        .weak_processed = 1,
        .timings = .{ .mark_ns = 7, .total_ns = 44 },
    } });
    sink.emit(.{ .memprof = .{
        .action = .sampled_alloc,
        .handle = handle,
        .sample_ordinal = 1,
        .kind = .boxed_i64,
        .payload_bytes = 8,
        .storage_bytes = 0,
        .scan_words = 0,
        .allocation_cost_units = 1,
        .space = .major,
        .backtrace_depth = 3,
    } });
    sink.emit(.{ .collect = .{
        .phase = .end,
        .strategy = .mark_sweep,
        .root_count = 2,
        .reclaimed = 0,
    } });

    try std.testing.expectEqual(@as(usize, 9), trace.traceEntries().len);
    try std.testing.expectEqual(@as(usize, 1), trace.rootProviderEntries().len);
    try std.testing.expectEqual(@as(usize, 1), trace.last_gc_snapshot.?.marked.boxed_i64);
    try std.testing.expectEqual(@as(usize, 1), trace.last_gc_snapshot.?.promoted.tuple);
    try std.testing.expectEqual(@as(usize, 2), trace.last_gc_snapshot.?.promoted_allocation_units);
    try std.testing.expectEqual(@as(usize, 1), trace.last_gc_snapshot.?.nursery_objects);
    try std.testing.expectEqual(@as(usize, 2), trace.last_gc_snapshot.?.nursery_allocation_units);
    try std.testing.expectEqual(@as(usize, 3), trace.last_gc_snapshot.?.major_objects);
    try std.testing.expectEqual(@as(usize, 4), trace.last_gc_snapshot.?.major_allocation_units);
    try std.testing.expectEqual(@as(usize, 1), trace.last_gc_snapshot.?.weak_processed);
    try std.testing.expectEqual(@as(usize, 1), trace.snapshot().memprof_samples);
    const last = trace.lastObjectEvent(handle).?;
    switch (last) {
        .field_write => |event| try std.testing.expectEqual(@as(usize, 0), event.index),
        else => return error.TestUnexpectedResult,
    }
}
