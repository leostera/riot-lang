const std = @import("std");
const heap_store = @import("heap_store.zig");
const value = @import("value.zig");

pub const HeapRef = value.HeapRef;
pub const ObjectKind = heap_store.ObjectKind;

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
    bump,
};

pub const ControlAction = enum {
    fiber_activate,
    continuation_capture,
    continuation_resume,
    effect_unhandled,
};

pub const AllocEvent = struct {
    handle: HeapRef,
    kind: ObjectKind,
    size: usize,
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

pub const ControlEvent = struct {
    action: ControlAction,
};

pub const Event = union(enum) {
    alloc: AllocEvent,
    field_write: FieldWriteEvent,
    bytes_write: BytesWriteEvent,
    root: RootEvent,
    collect: CollectEvent,
    reclaim: ReclaimEvent,
    control: ControlEvent,
};

pub const Counters = struct {
    allocations: usize = 0,
    field_writes: usize = 0,
    bytes_writes: usize = 0,
    root_registrations: usize = 0,
    root_unregistrations: usize = 0,
    collections: usize = 0,
    reclaims: usize = 0,
    fiber_activations: usize = 0,
    continuation_captures: usize = 0,
    continuation_resumes: usize = 0,
    unhandled_effects: usize = 0,

    pub fn diff(after: Counters, before: Counters) Counters {
        return .{
            .allocations = after.allocations - before.allocations,
            .field_writes = after.field_writes - before.field_writes,
            .bytes_writes = after.bytes_writes - before.bytes_writes,
            .root_registrations = after.root_registrations - before.root_registrations,
            .root_unregistrations = after.root_unregistrations - before.root_unregistrations,
            .collections = after.collections - before.collections,
            .reclaims = after.reclaims - before.reclaims,
            .fiber_activations = after.fiber_activations - before.fiber_activations,
            .continuation_captures = after.continuation_captures - before.continuation_captures,
            .continuation_resumes = after.continuation_resumes - before.continuation_resumes,
            .unhandled_effects = after.unhandled_effects - before.unhandled_effects,
        };
    }
};

pub const Recorder = struct {
    counters: Counters = .{},
    last_collect_root_count: usize = 0,
    last_collect_reclaimed: usize = 0,

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
            .root => |root_event| switch (root_event.action) {
                .register => self.counters.root_registrations +%= 1,
                .unregister => self.counters.root_unregistrations +%= 1,
            },
            .collect => |collect_event| {
                if (collect_event.phase == .end) {
                    self.counters.collections +%= 1;
                    self.last_collect_root_count = collect_event.root_count;
                    self.last_collect_reclaimed = collect_event.reclaimed;
                }
            },
            .reclaim => self.counters.reclaims +%= 1,
            .control => |control_event| switch (control_event.action) {
                .fiber_activate => self.counters.fiber_activations +%= 1,
                .continuation_capture => self.counters.continuation_captures +%= 1,
                .continuation_resume => self.counters.continuation_resumes +%= 1,
                .effect_unhandled => self.counters.unhandled_effects +%= 1,
            },
        }
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
        .size = 2,
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
    sink.emit(.{ .root = .{ .action = .register, .is_block = true } });
    sink.emit(.{ .reclaim = .{
        .handle = .{ .index = 1, .generation = 1 },
        .kind = .tuple,
    } });
    sink.emit(.{ .control = .{ .action = .continuation_capture } });
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
    try std.testing.expectEqual(@as(usize, 1), counters.root_registrations);
    try std.testing.expectEqual(@as(usize, 1), counters.reclaims);
    try std.testing.expectEqual(@as(usize, 1), counters.collections);
    try std.testing.expectEqual(@as(usize, 1), counters.continuation_captures);
    try std.testing.expectEqual(@as(usize, 1), recorder.last_collect_root_count);
    try std.testing.expectEqual(@as(usize, 1), recorder.last_collect_reclaimed);
}
