const std = @import("std");
const event_sink_mod = @import("event_sink.zig");
const root_provider = @import("root_provider.zig");
const value = @import("value.zig");

pub const EventSink = event_sink_mod.EventSink;
pub const RootProvider = root_provider.RootProvider;
pub const RootVisitor = root_provider.RootVisitor;
pub const Value = value.Value;

pub const EffectId = u32;

pub const FiberHandle = struct {
    index: u32,
    generation: u32,
};

pub const ContinuationHandle = struct {
    index: u32,
    generation: u32,
};

pub const FiberStatus = enum {
    active,
    suspended,
    completed,
};

pub const HandlerFrame = struct {
    effect: EffectId,
    handle_effect: Value,
    handle_value: ?Value = null,
    handle_exn: ?Value = null,
};

pub const FiberState = struct {
    status: FiberStatus,
    parent: ?FiberHandle,
    handlers: std.ArrayListUnmanaged(HandlerFrame) = .{},

    fn empty() FiberState {
        return .{
            .status = .completed,
            .parent = null,
            .handlers = .{},
        };
    }

    fn init(parent: ?FiberHandle, status: FiberStatus) FiberState {
        return .{
            .status = status,
            .parent = parent,
            .handlers = .{},
        };
    }

    fn deinit(self: *FiberState, allocator: std.mem.Allocator) void {
        self.handlers.deinit(allocator);
        self.* = FiberState.empty();
    }
};

pub const ContinuationStatus = enum {
    suspended,
    resumed,
    dropped,
};

pub const ContinuationState = struct {
    fiber: FiberHandle,
    parent: ?FiberHandle,
    handler_fiber: FiberHandle,
    handler_index: usize,
    effect: EffectId,
    payload: Value,
    status: ContinuationStatus = .suspended,
    captured_roots: std.ArrayListUnmanaged(Value) = .{},

    fn empty() ContinuationState {
        return .{
            .fiber = .{ .index = 0, .generation = 0 },
            .parent = null,
            .handler_fiber = .{ .index = 0, .generation = 0 },
            .handler_index = 0,
            .effect = 0,
            .payload = value.Unit,
            .status = .dropped,
            .captured_roots = .{},
        };
    }

    fn deinit(self: *ContinuationState, allocator: std.mem.Allocator) void {
        self.captured_roots.deinit(allocator);
        self.* = ContinuationState.empty();
    }
};

const FiberSlot = struct {
    generation: u32,
    alive: bool,
    fiber: FiberState,
};

const ContinuationSlot = struct {
    generation: u32,
    alive: bool,
    continuation: ContinuationState,
};

pub const ControlKernel = struct {
    allocator: std.mem.Allocator,
    event_sink: EventSink,
    fibers: std.ArrayListUnmanaged(FiberSlot) = .{},
    free_fibers: std.ArrayListUnmanaged(u32) = .{},
    continuations: std.ArrayListUnmanaged(ContinuationSlot) = .{},
    free_continuations: std.ArrayListUnmanaged(u32) = .{},
    current_fiber: FiberHandle,

    pub fn init(allocator: std.mem.Allocator) ControlKernel {
        return initWithSink(allocator, EventSink.noop());
    }

    pub fn initWithSink(allocator: std.mem.Allocator, sink: EventSink) ControlKernel {
        var kernel = ControlKernel{
            .allocator = allocator,
            .event_sink = sink,
            .current_fiber = .{ .index = 0, .generation = 0 },
        };
        kernel.current_fiber = kernel.addFiber(FiberState.init(null, .active)) catch {
            @panic("zort: out of memory while creating main fiber");
        };
        return kernel;
    }

    pub fn deinit(self: *ControlKernel) void {
        for (self.fibers.items) |*slot| {
            if (slot.alive) slot.fiber.deinit(self.allocator);
        }
        for (self.continuations.items) |*slot| {
            if (slot.alive) slot.continuation.deinit(self.allocator);
        }
        self.fibers.deinit(self.allocator);
        self.free_fibers.deinit(self.allocator);
        self.continuations.deinit(self.allocator);
        self.free_continuations.deinit(self.allocator);
    }

    pub fn provider(self: *ControlKernel) RootProvider {
        return .{
            .ctx = self,
            .count_fn = countRoots,
            .visit_fn = visitRoots,
        };
    }

    pub fn currentFiber(self: *const ControlKernel) FiberHandle {
        return self.current_fiber;
    }

    pub fn fiber(self: *const ControlKernel, handle: FiberHandle) ?*const FiberState {
        if (handle.index >= self.fibers.items.len) return null;
        const slot = &self.fibers.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return &slot.fiber;
    }

    fn fiberMut(self: *ControlKernel, handle: FiberHandle) ?*FiberState {
        if (handle.index >= self.fibers.items.len) return null;
        const slot = &self.fibers.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return &slot.fiber;
    }

    pub fn continuation(self: *const ControlKernel, handle: ContinuationHandle) ?*const ContinuationState {
        if (handle.index >= self.continuations.items.len) return null;
        const slot = &self.continuations.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return &slot.continuation;
    }

    pub fn createFiber(self: *ControlKernel, parent: ?FiberHandle) !FiberHandle {
        return self.addFiber(FiberState.init(parent, .suspended));
    }

    pub fn activateFiber(self: *ControlKernel, handle: FiberHandle) !void {
        if (self.fiber(handle) == null) return error.InvalidFiber;
        if (self.current_fiber.index != handle.index or self.current_fiber.generation != handle.generation) {
            if (self.fiberMut(self.current_fiber)) |current| {
                if (current.status == .active) current.status = .suspended;
            }
        }
        const next = self.fiberMut(handle).?;
        next.status = .active;
        self.current_fiber = handle;
        self.event_sink.emit(.{ .control = .{ .action = .fiber_activate } });
    }

    pub fn pushHandler(self: *ControlKernel, fiber_handle: FiberHandle, handler: HandlerFrame) !void {
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        try fiber_state.handlers.append(self.allocator, handler);
    }

    pub fn popHandler(self: *ControlKernel, fiber_handle: FiberHandle) !HandlerFrame {
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        return fiber_state.handlers.pop() orelse error.EmptyHandlerStack;
    }

    pub fn handlerCount(self: *const ControlKernel, fiber_handle: FiberHandle) !usize {
        const fiber_state = self.fiber(fiber_handle) orelse return error.InvalidFiber;
        return fiber_state.handlers.items.len;
    }

    pub fn captureContinuation(
        self: *ControlKernel,
        effect: EffectId,
        payload: Value,
        captured_roots: []const Value,
    ) !ContinuationHandle {
        const fiber_handle = self.current_fiber;
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        fiber_state.status = .suspended;

        var captured = ContinuationState{
            .fiber = fiber_handle,
            .parent = fiber_state.parent,
            .handler_fiber = fiber_handle,
            .handler_index = fiber_state.handlers.items.len,
            .effect = effect,
            .payload = payload,
        };
        try captured.captured_roots.appendSlice(self.allocator, captured_roots);
        const handle = try self.addContinuation(captured);
        self.event_sink.emit(.{ .control = .{ .action = .continuation_capture } });
        return handle;
    }

    pub const PerformResult = struct {
        continuation: ContinuationHandle,
        handler_fiber: FiberHandle,
        handler_index: usize,
        handler: HandlerFrame,
    };

    pub const ResumeResult = struct {
        fiber: FiberHandle,
        value: Value,
    };

    pub fn perform(
        self: *ControlKernel,
        effect: EffectId,
        payload: Value,
        captured_roots: []const Value,
    ) !PerformResult {
        const match = self.findHandler(self.current_fiber, effect) orelse {
            self.event_sink.emit(.{ .control = .{ .action = .effect_unhandled } });
            return error.UnhandledEffect;
        };

        const captured_handle = try self.captureMatchedContinuation(match, effect, payload, captured_roots);
        return .{
            .continuation = captured_handle,
            .handler_fiber = match.fiber,
            .handler_index = match.index,
            .handler = match.handler,
        };
    }

    pub fn resumeContinuation(self: *ControlKernel, handle: ContinuationHandle, value_to_resume: Value) !ResumeResult {
        const slot = self.continuationSlotMut(handle) orelse return error.InvalidContinuation;
        switch (slot.continuation.status) {
            .suspended => {},
            .resumed, .dropped => return error.AlreadyResumed,
        }
        slot.continuation.status = .resumed;
        try self.activateFiber(slot.continuation.fiber);
        self.event_sink.emit(.{ .control = .{ .action = .continuation_resume } });
        return .{
            .fiber = slot.continuation.fiber,
            .value = value_to_resume,
        };
    }

    pub fn dropContinuation(self: *ControlKernel, handle: ContinuationHandle) bool {
        if (handle.index >= self.continuations.items.len) return false;
        const slot = &self.continuations.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return false;
        slot.continuation.deinit(self.allocator);
        slot.alive = false;
        slot.generation +%= 1;
        self.free_continuations.append(self.allocator, handle.index) catch {
            @panic("zort: out of memory while dropping continuation slot");
        };
        return true;
    }

    fn addFiber(self: *ControlKernel, fiber_state: FiberState) !FiberHandle {
        const slot_index: usize = if (self.free_fibers.items.len > 0) blk: {
            const reused = self.free_fibers.pop() orelse unreachable;
            break :blk @intCast(reused);
        } else self.fibers.items.len;

        if (slot_index < self.fibers.items.len) {
            const slot = &self.fibers.items[slot_index];
            slot.alive = true;
            slot.fiber = fiber_state;
            return .{ .index = @intCast(slot_index), .generation = slot.generation };
        }

        try self.fibers.append(self.allocator, .{
            .generation = 1,
            .alive = true,
            .fiber = fiber_state,
        });
        return .{ .index = @intCast(slot_index), .generation = 1 };
    }

    fn addContinuation(self: *ControlKernel, captured: ContinuationState) !ContinuationHandle {
        const slot_index: usize = if (self.free_continuations.items.len > 0) blk: {
            const reused = self.free_continuations.pop() orelse unreachable;
            break :blk @intCast(reused);
        } else self.continuations.items.len;

        if (slot_index < self.continuations.items.len) {
            const slot = &self.continuations.items[slot_index];
            slot.alive = true;
            slot.continuation = captured;
            return .{ .index = @intCast(slot_index), .generation = slot.generation };
        }

        try self.continuations.append(self.allocator, .{
            .generation = 1,
            .alive = true,
            .continuation = captured,
        });
        return .{ .index = @intCast(slot_index), .generation = 1 };
    }

    const HandlerMatch = struct {
        fiber: FiberHandle,
        index: usize,
        handler: HandlerFrame,
    };

    fn continuationSlotMut(self: *ControlKernel, handle: ContinuationHandle) ?*ContinuationSlot {
        if (handle.index >= self.continuations.items.len) return null;
        const slot = &self.continuations.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return slot;
    }

    fn findHandler(self: *const ControlKernel, fiber_handle: FiberHandle, effect: EffectId) ?HandlerMatch {
        var cursor: ?FiberHandle = fiber_handle;
        while (cursor) |current| {
            const fiber_state = self.fiber(current) orelse return null;
            var i = fiber_state.handlers.items.len;
            while (i > 0) {
                i -= 1;
                const handler = fiber_state.handlers.items[i];
                if (handler.effect == effect) {
                    return .{
                        .fiber = current,
                        .index = i,
                        .handler = handler,
                    };
                }
            }
            cursor = fiber_state.parent;
        }
        return null;
    }

    fn captureMatchedContinuation(
        self: *ControlKernel,
        match: HandlerMatch,
        effect: EffectId,
        payload: Value,
        captured_roots: []const Value,
    ) !ContinuationHandle {
        const fiber_handle = self.current_fiber;
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        fiber_state.status = .suspended;

        var captured = ContinuationState{
            .fiber = fiber_handle,
            .parent = fiber_state.parent,
            .handler_fiber = match.fiber,
            .handler_index = match.index,
            .effect = effect,
            .payload = payload,
        };
        try captured.captured_roots.appendSlice(self.allocator, captured_roots);
        const handle = try self.addContinuation(captured);
        self.event_sink.emit(.{ .control = .{ .action = .continuation_capture } });
        return handle;
    }

    fn countRoots(ctx: ?*anyopaque) usize {
        const self: *ControlKernel = @ptrCast(@alignCast(ctx.?));
        var count: usize = 0;

        for (self.fibers.items) |slot| {
            if (!slot.alive) continue;
            count += countFiberRoots(&slot.fiber);
        }
        for (self.continuations.items) |slot| {
            if (!slot.alive) continue;
            if (slot.continuation.status != .suspended) continue;
            count += 1 + slot.continuation.captured_roots.items.len;
        }
        return count;
    }

    fn visitRoots(ctx: ?*anyopaque, visitor: RootVisitor) void {
        const self: *ControlKernel = @ptrCast(@alignCast(ctx.?));

        for (self.fibers.items) |slot| {
            if (!slot.alive) continue;
            visitFiberRoots(&slot.fiber, visitor);
        }
        for (self.continuations.items) |slot| {
            if (!slot.alive) continue;
            if (slot.continuation.status != .suspended) continue;
            visitor.visit(slot.continuation.payload);
            for (slot.continuation.captured_roots.items) |rooted| {
                visitor.visit(rooted);
            }
        }
    }

    fn countFiberRoots(fiber_state: *const FiberState) usize {
        var count: usize = 0;
        for (fiber_state.handlers.items) |handler| {
            count += 1;
            if (handler.handle_value != null) count += 1;
            if (handler.handle_exn != null) count += 1;
        }
        return count;
    }

    fn visitFiberRoots(fiber_state: *const FiberState, visitor: RootVisitor) void {
        for (fiber_state.handlers.items) |handler| {
            visitor.visit(handler.handle_effect);
            if (handler.handle_value) |rooted| visitor.visit(rooted);
            if (handler.handle_exn) |rooted| visitor.visit(rooted);
        }
    }
};

test "control_kernel: child fibers preserve parent links and handler stacks" {
    var kernel = ControlKernel.init(std.testing.allocator);
    defer kernel.deinit();

    const main = kernel.currentFiber();
    const child = try kernel.createFiber(main);
    try kernel.pushHandler(main, .{
        .effect = 7,
        .handle_effect = Value.fromInt(1),
        .handle_value = Value.fromInt(2),
    });
    try kernel.pushHandler(main, .{
        .effect = 9,
        .handle_effect = Value.fromInt(3),
        .handle_exn = Value.fromInt(4),
    });

    const child_state = kernel.fiber(child).?;
    try std.testing.expectEqual(main, child_state.parent.?);
    try std.testing.expectEqual(@as(FiberStatus, .suspended), child_state.status);
    try std.testing.expectEqual(@as(usize, 2), try kernel.handlerCount(main));

    const popped = try kernel.popHandler(main);
    try std.testing.expectEqual(@as(EffectId, 9), popped.effect);
    try std.testing.expectEqual(@as(usize, 1), try kernel.handlerCount(main));
}

test "control_kernel: provider exposes handler and suspended continuation roots" {
    var kernel = ControlKernel.init(std.testing.allocator);
    defer kernel.deinit();

    const main = kernel.currentFiber();
    try kernel.pushHandler(main, .{
        .effect = 1,
        .handle_effect = Value.fromHeapRef(.{ .index = 1, .generation = 1 }),
        .handle_value = Value.fromHeapRef(.{ .index = 2, .generation = 1 }),
    });

    const child = try kernel.createFiber(main);
    try kernel.activateFiber(child);
    _ = try kernel.captureContinuation(1, Value.fromHeapRef(.{ .index = 3, .generation = 1 }), &.{
        Value.fromInt(5),
        Value.fromHeapRef(.{ .index = 4, .generation = 1 }),
    });

    var seen = std.ArrayListUnmanaged(Value){};
    defer seen.deinit(std.testing.allocator);

    const Collect = struct {
        fn visit(ctx: ?*anyopaque, rooted: Value) void {
            const items: *std.ArrayListUnmanaged(Value) = @ptrCast(@alignCast(ctx.?));
            items.append(std.testing.allocator, rooted) catch unreachable;
        }
    };

    const provider = kernel.provider();
    try std.testing.expectEqual(@as(usize, 5), provider.count());
    provider.visit(.{
        .ctx = &seen,
        .visit_fn = Collect.visit,
    });

    try std.testing.expectEqual(@as(usize, 5), seen.items.len);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 1, .generation = 1 }), seen.items[0]);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 2, .generation = 1 }), seen.items[1]);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 3, .generation = 1 }), seen.items[2]);
    try std.testing.expectEqual(Value.fromInt(5), seen.items[3]);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 4, .generation = 1 }), seen.items[4]);
}

test "control_kernel: perform walks parent handlers and returns continuation" {
    var kernel = ControlKernel.init(std.testing.allocator);
    defer kernel.deinit();

    const main = kernel.currentFiber();
    try kernel.pushHandler(main, .{
        .effect = 11,
        .handle_effect = Value.fromInt(99),
    });

    const child = try kernel.createFiber(main);
    try kernel.activateFiber(child);

    const performed = try kernel.perform(11, Value.fromInt(7), &.{Value.fromInt(8)});
    try std.testing.expectEqual(main, performed.handler_fiber);
    try std.testing.expectEqual(@as(usize, 0), performed.handler_index);
    try std.testing.expectEqual(@as(i64, 99), performed.handler.handle_effect.asInt());

    const captured = kernel.continuation(performed.continuation).?;
    try std.testing.expectEqual(child, captured.fiber);
    try std.testing.expectEqual(main, captured.handler_fiber);
    try std.testing.expectEqual(@as(usize, 0), captured.handler_index);
    try std.testing.expectEqual(@as(usize, 1), captured.captured_roots.items.len);
}

test "control_kernel: unhandled effects are explicit" {
    var recorder = event_sink_mod.Recorder{};
    var kernel = ControlKernel.initWithSink(std.testing.allocator, recorder.sink());
    defer kernel.deinit();

    try std.testing.expectError(error.UnhandledEffect, kernel.perform(44, Value.fromInt(1), &.{}));

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.unhandled_effects);
}

test "control_kernel: resume is one-shot and suspended roots disappear after resume" {
    var recorder = event_sink_mod.Recorder{};
    var kernel = ControlKernel.initWithSink(std.testing.allocator, recorder.sink());
    defer kernel.deinit();

    const main = kernel.currentFiber();
    try kernel.pushHandler(main, .{
        .effect = 1,
        .handle_effect = Value.fromInt(17),
    });
    const child = try kernel.createFiber(main);
    try kernel.activateFiber(child);

    const performed = try kernel.perform(1, Value.fromHeapRef(.{ .index = 1, .generation = 1 }), &.{
        Value.fromHeapRef(.{ .index = 2, .generation = 1 }),
    });
    try std.testing.expectEqual(@as(usize, 3), kernel.provider().count());

    const resumed = try kernel.resumeContinuation(performed.continuation, Value.fromInt(42));
    try std.testing.expectEqual(child, resumed.fiber);
    try std.testing.expectEqual(@as(i64, 42), resumed.value.asInt());
    try std.testing.expectEqual(@as(usize, 1), kernel.provider().count());
    try std.testing.expectEqual(child, kernel.currentFiber());
    try std.testing.expectEqual(@as(FiberStatus, .active), kernel.fiber(child).?.status);
    try std.testing.expectError(error.AlreadyResumed, kernel.resumeContinuation(performed.continuation, Value.fromInt(0)));

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.continuation_captures);
    try std.testing.expectEqual(@as(usize, 1), counters.continuation_resumes);
    try std.testing.expect(counters.fiber_activations >= 2);
}
