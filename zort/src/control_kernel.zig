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

pub const StackLimits = struct {
    max_frames: usize = 256,
    max_frame_roots: usize = 256,
};

pub const StackFrame = struct {
    site_id: u32,
    roots: std.ArrayListUnmanaged(Value) = .{},

    fn empty() StackFrame {
        return .{
            .site_id = 0,
            .roots = .{},
        };
    }

    fn deinit(self: *StackFrame, allocator: std.mem.Allocator) void {
        self.roots.deinit(allocator);
        self.* = StackFrame.empty();
    }
};

pub const ManagedStack = struct {
    frames: std.ArrayListUnmanaged(StackFrame) = .{},

    pub const FrameInfo = struct {
        site_id: u32,
        root_count: usize,
    };

    pub fn deinit(self: *ManagedStack, allocator: std.mem.Allocator) void {
        for (self.frames.items) |*frame| frame.deinit(allocator);
        self.frames.deinit(allocator);
        self.* = .{};
    }

    pub fn take(self: *ManagedStack) ManagedStack {
        const taken = self.*;
        self.* = .{};
        return taken;
    }

    pub fn pushFrame(self: *ManagedStack, allocator: std.mem.Allocator, limits: StackLimits, site_id: u32) !void {
        if (self.frames.items.len >= limits.max_frames) return error.StackOverflow;
        try self.frames.append(allocator, .{ .site_id = site_id });
    }

    pub fn popFrame(self: *ManagedStack, allocator: std.mem.Allocator) !FrameInfo {
        var frame = self.frames.pop() orelse return error.EmptyFrameStack;
        defer frame.deinit(allocator);
        return .{
            .site_id = frame.site_id,
            .root_count = frame.roots.items.len,
        };
    }

    pub fn pushRoot(self: *ManagedStack, allocator: std.mem.Allocator, limits: StackLimits, rooted: Value) !void {
        if (self.frames.items.len == 0) return error.EmptyFrameStack;
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (frame.roots.items.len >= limits.max_frame_roots) return error.StackOverflow;
        try frame.roots.append(allocator, rooted);
    }

    pub fn frameCount(self: *const ManagedStack) usize {
        return self.frames.items.len;
    }

    fn countRoots(self: *const ManagedStack) usize {
        var count: usize = 0;
        for (self.frames.items) |frame| count += frame.roots.items.len;
        return count;
    }

    fn countValueRoots(self: *const ManagedStack, needle: Value) usize {
        var count: usize = 0;
        for (self.frames.items) |frame| {
            for (frame.roots.items) |rooted| {
                if (std.meta.eql(rooted, needle)) count += 1;
            }
        }
        return count;
    }

    fn visitRoots(self: *const ManagedStack, visitor: RootVisitor) void {
        for (self.frames.items) |frame| {
            for (frame.roots.items) |rooted| visitor.visit(rooted);
        }
    }
};

pub const BacktraceFrame = struct {
    fiber: FiberHandle,
    site_id: u32,
    root_count: usize,
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
    stack: ManagedStack = .{},
    callback_parents: std.ArrayListUnmanaged(?FiberHandle) = .{},

    fn empty() FiberState {
        return .{
            .status = .completed,
            .parent = null,
            .handlers = .{},
            .stack = .{},
            .callback_parents = .{},
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
        self.stack.deinit(allocator);
        self.callback_parents.deinit(allocator);
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
    stack: ManagedStack = .{},

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
            .stack = .{},
        };
    }

    fn deinit(self: *ContinuationState, allocator: std.mem.Allocator) void {
        self.captured_roots.deinit(allocator);
        self.stack.deinit(allocator);
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
    pub const Config = struct {
        event_sink: EventSink = EventSink.noop(),
        stack_limits: StackLimits = .{},
    };

    allocator: std.mem.Allocator,
    event_sink: EventSink,
    stack_limits: StackLimits,
    fibers: std.ArrayListUnmanaged(FiberSlot) = .{},
    free_fibers: std.ArrayListUnmanaged(u32) = .{},
    continuations: std.ArrayListUnmanaged(ContinuationSlot) = .{},
    free_continuations: std.ArrayListUnmanaged(u32) = .{},
    current_fiber: FiberHandle,

    pub fn init(allocator: std.mem.Allocator) ControlKernel {
        return initWithConfig(allocator, .{});
    }

    pub fn initWithSink(allocator: std.mem.Allocator, sink: EventSink) ControlKernel {
        return initWithConfig(allocator, .{
            .event_sink = sink,
        });
    }

    pub fn initWithConfig(allocator: std.mem.Allocator, config: Config) ControlKernel {
        var kernel = ControlKernel{
            .allocator = allocator,
            .event_sink = config.event_sink,
            .stack_limits = config.stack_limits,
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
            .name = "control_kernel",
            .ctx = self,
            .count_fn = countRoots,
            .visit_fn = visitRoots,
        };
    }

    pub fn ownedRootCount(self: *const ControlKernel, needle: Value) usize {
        var count: usize = 0;
        for (self.fibers.items) |slot| {
            if (!slot.alive) continue;
            count += countFiberValueRoots(&slot.fiber, needle);
        }
        for (self.continuations.items) |slot| {
            if (!slot.alive or slot.continuation.status != .suspended) continue;
            if (std.meta.eql(slot.continuation.payload, needle)) count += 1;
            for (slot.continuation.captured_roots.items) |rooted| {
                if (std.meta.eql(rooted, needle)) count += 1;
            }
        }
        return count;
    }

    pub const VerifyError = error{
        InvalidCurrentFiber,
        ActiveFiberCountMismatch,
        InvalidParentFiber,
        InvalidCallbackParent,
        InvalidHandlerValue,
        InvalidFrameValue,
        InvalidContinuationFiber,
        InvalidContinuationParent,
        InvalidHandlerFiber,
        InvalidHandlerIndex,
        InvalidContinuationValue,
    };

    pub fn verify(
        self: *const ControlKernel,
        context: anytype,
        comptime is_valid_value: fn (@TypeOf(context), Value) bool,
    ) VerifyError!void {
        if (self.fiber(self.current_fiber) == null) return error.InvalidCurrentFiber;

        var active_fibers: usize = 0;
        for (self.fibers.items, 0..) |slot, slot_index| {
            if (!slot.alive) continue;
            if (slot.fiber.status == .active) active_fibers += 1;
            if (slot.fiber.parent) |parent| {
                if (self.fiber(parent) == null) return error.InvalidParentFiber;
            }
            for (slot.fiber.callback_parents.items) |saved_parent| {
                if (saved_parent) |parent| {
                    if (self.fiber(parent) == null) return error.InvalidCallbackParent;
                }
            }
            for (slot.fiber.handlers.items) |handler| {
                if (handler.handle_effect.isBlock() and !is_valid_value(context, handler.handle_effect)) {
                    return error.InvalidHandlerValue;
                }
                if (handler.handle_value) |rooted| {
                    if (rooted.isBlock() and !is_valid_value(context, rooted)) return error.InvalidHandlerValue;
                }
                if (handler.handle_exn) |rooted| {
                    if (rooted.isBlock() and !is_valid_value(context, rooted)) return error.InvalidHandlerValue;
                }
            }
            for (slot.fiber.stack.frames.items) |frame| {
                for (frame.roots.items) |rooted| {
                    if (rooted.isBlock() and !is_valid_value(context, rooted)) return error.InvalidFrameValue;
                }
            }
            _ = slot_index;
        }
        if (active_fibers != 1) return error.ActiveFiberCountMismatch;

        for (self.continuations.items) |slot| {
            if (!slot.alive) continue;
            if (self.fiber(slot.continuation.fiber) == null) return error.InvalidContinuationFiber;
            if (slot.continuation.parent) |parent| {
                if (self.fiber(parent) == null) return error.InvalidContinuationParent;
            }
            const handler_fiber = self.fiber(slot.continuation.handler_fiber) orelse return error.InvalidHandlerFiber;
            if (slot.continuation.handler_index > handler_fiber.handlers.items.len) {
                return error.InvalidHandlerIndex;
            }
            if (slot.continuation.payload.isBlock() and !is_valid_value(context, slot.continuation.payload)) {
                return error.InvalidContinuationValue;
            }
            for (slot.continuation.captured_roots.items) |rooted| {
                if (rooted.isBlock() and !is_valid_value(context, rooted)) return error.InvalidContinuationValue;
            }
            for (slot.continuation.stack.frames.items) |frame| {
                for (frame.roots.items) |rooted| {
                    if (rooted.isBlock() and !is_valid_value(context, rooted)) return error.InvalidContinuationValue;
                }
            }
        }
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
        self.event_sink.emit(.{ .control = .{
            .action = .fiber_activate,
            .fiber = toTraceHandle(handle),
            .parent_depth = self.parentDepth(handle),
        } });
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

    pub fn pushFrame(self: *ControlKernel, fiber_handle: FiberHandle, site_id: u32) !void {
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        try fiber_state.stack.pushFrame(self.allocator, self.stack_limits, site_id);
    }

    pub fn popFrame(self: *ControlKernel, fiber_handle: FiberHandle) !ManagedStack.FrameInfo {
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        return fiber_state.stack.popFrame(self.allocator);
    }

    pub fn pushFrameRoot(self: *ControlKernel, fiber_handle: FiberHandle, rooted: Value) !void {
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        try fiber_state.stack.pushRoot(self.allocator, self.stack_limits, rooted);
    }

    pub fn frameCount(self: *const ControlKernel, fiber_handle: FiberHandle) !usize {
        const fiber_state = self.fiber(fiber_handle) orelse return error.InvalidFiber;
        return fiber_state.stack.frameCount();
    }

    pub fn enterCallbackBoundary(self: *ControlKernel, fiber_handle: FiberHandle) !void {
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        try fiber_state.callback_parents.append(self.allocator, fiber_state.parent);
        fiber_state.parent = null;
        self.event_sink.emit(.{ .control = .{
            .action = .callback_enter,
            .fiber = toTraceHandle(fiber_handle),
            .parent_depth = self.parentDepth(fiber_handle),
        } });
    }

    pub fn exitCallbackBoundary(self: *ControlKernel, fiber_handle: FiberHandle) !void {
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        fiber_state.parent = fiber_state.callback_parents.pop() orelse return error.CallbackBoundaryUnderflow;
        self.event_sink.emit(.{ .control = .{
            .action = .callback_exit,
            .fiber = toTraceHandle(fiber_handle),
            .parent_depth = self.parentDepth(fiber_handle),
        } });
    }

    pub fn captureBacktrace(self: *const ControlKernel, allocator: std.mem.Allocator, start: ?FiberHandle) ![]BacktraceFrame {
        var frames = std.ArrayListUnmanaged(BacktraceFrame){};
        errdefer frames.deinit(allocator);

        var cursor = start orelse self.current_fiber;
        while (true) {
            const fiber_state = self.fiber(cursor) orelse return error.InvalidFiber;
            var i = fiber_state.stack.frames.items.len;
            while (i > 0) {
                i -= 1;
                const frame = fiber_state.stack.frames.items[i];
                try frames.append(allocator, .{
                    .fiber = cursor,
                    .site_id = frame.site_id,
                    .root_count = frame.roots.items.len,
                });
            }
            cursor = fiber_state.parent orelse break;
        }

        return frames.toOwnedSlice(allocator);
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
            .stack = fiber_state.stack.take(),
        };
        try captured.captured_roots.appendSlice(self.allocator, captured_roots);
        const handle = try self.addContinuation(captured);
        self.event_sink.emit(.{ .control = .{
            .action = .continuation_capture,
            .effect = effect,
            .fiber = toTraceHandle(fiber_handle),
            .continuation = toTraceHandle(handle),
            .handler_fiber = toTraceHandle(fiber_handle),
            .handler_index = fiber_state.handlers.items.len,
            .parent_depth = self.parentDepth(fiber_handle),
        } });
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
        return self.performAt(0, effect, payload, captured_roots);
    }

    pub fn performAt(
        self: *ControlKernel,
        site_id: u32,
        effect: EffectId,
        payload: Value,
        captured_roots: []const Value,
    ) !PerformResult {
        const match = self.findHandler(self.current_fiber, effect) orelse {
            self.event_sink.emit(.{ .control = .{
                .action = .effect_unhandled,
                .site_id = site_id,
                .effect = effect,
                .fiber = toTraceHandle(self.current_fiber),
                .parent_depth = self.parentDepth(self.current_fiber),
            } });
            return error.UnhandledEffect;
        };

        const captured_handle = try self.captureMatchedContinuation(site_id, match, effect, payload, captured_roots);
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
        const captured_stack = slot.continuation.stack.take();
        const fiber_handle = slot.continuation.fiber;
        const effect = slot.continuation.effect;
        const handler_fiber = slot.continuation.handler_fiber;
        const handler_index = slot.continuation.handler_index;
        slot.continuation.status = .resumed;
        const fiber_state = self.fiberMut(fiber_handle) orelse return error.InvalidFiber;
        fiber_state.stack.deinit(self.allocator);
        fiber_state.stack = captured_stack;
        try self.activateFiber(fiber_handle);
        self.event_sink.emit(.{ .control = .{
            .action = .continuation_resume,
            .effect = effect,
            .fiber = toTraceHandle(fiber_handle),
            .continuation = toTraceHandle(handle),
            .handler_fiber = toTraceHandle(handler_fiber),
            .handler_index = handler_index,
            .parent_depth = self.parentDepth(fiber_handle),
        } });
        return .{
            .fiber = fiber_handle,
            .value = value_to_resume,
        };
    }

    pub fn dropContinuation(self: *ControlKernel, handle: ContinuationHandle) bool {
        if (handle.index >= self.continuations.items.len) return false;
        const slot = &self.continuations.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return false;
        self.event_sink.emit(.{ .control = .{
            .action = .continuation_drop,
            .effect = slot.continuation.effect,
            .fiber = toTraceHandle(slot.continuation.fiber),
            .continuation = toTraceHandle(handle),
            .handler_fiber = toTraceHandle(slot.continuation.handler_fiber),
            .handler_index = slot.continuation.handler_index,
            .parent_depth = self.parentDepth(slot.continuation.fiber),
        } });
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
        site_id: u32,
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
            .stack = fiber_state.stack.take(),
        };
        try captured.captured_roots.appendSlice(self.allocator, captured_roots);
        const handle = try self.addContinuation(captured);
        self.event_sink.emit(.{ .control = .{
            .action = .continuation_capture,
            .site_id = site_id,
            .effect = effect,
            .fiber = toTraceHandle(fiber_handle),
            .continuation = toTraceHandle(handle),
            .handler_fiber = toTraceHandle(match.fiber),
            .handler_index = match.index,
            .parent_depth = self.parentDepth(fiber_handle),
        } });
        return handle;
    }

    fn parentDepth(self: *const ControlKernel, handle: FiberHandle) usize {
        var depth: usize = 0;
        var cursor = self.fiber(handle).?.parent;
        while (cursor) |parent| {
            depth += 1;
            cursor = self.fiber(parent).?.parent;
        }
        return depth;
    }

    fn toTraceHandle(handle: anytype) event_sink_mod.HandleRef {
        return .{
            .index = handle.index,
            .generation = handle.generation,
        };
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
            count += 1 + slot.continuation.captured_roots.items.len + slot.continuation.stack.countRoots();
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
            slot.continuation.stack.visitRoots(visitor);
        }
    }

    fn countFiberRoots(fiber_state: *const FiberState) usize {
        var count: usize = 0;
        for (fiber_state.handlers.items) |handler| {
            count += 1;
            if (handler.handle_value != null) count += 1;
            if (handler.handle_exn != null) count += 1;
        }
        count += fiber_state.stack.countRoots();
        return count;
    }

    fn countFiberValueRoots(fiber_state: *const FiberState, needle: Value) usize {
        var count: usize = 0;
        for (fiber_state.handlers.items) |handler| {
            if (std.meta.eql(handler.handle_effect, needle)) count += 1;
            if (handler.handle_value) |rooted| {
                if (std.meta.eql(rooted, needle)) count += 1;
            }
            if (handler.handle_exn) |rooted| {
                if (std.meta.eql(rooted, needle)) count += 1;
            }
        }
        count += fiber_state.stack.countValueRoots(needle);
        return count;
    }

    fn visitFiberRoots(fiber_state: *const FiberState, visitor: RootVisitor) void {
        for (fiber_state.handlers.items) |handler| {
            visitor.visit(handler.handle_effect);
            if (handler.handle_value) |rooted| visitor.visit(rooted);
            if (handler.handle_exn) |rooted| visitor.visit(rooted);
        }
        fiber_state.stack.visitRoots(visitor);
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
    try kernel.pushFrame(child, 12);
    try kernel.pushFrameRoot(child, Value.fromHeapRef(.{ .index = 5, .generation = 1 }));
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
    try std.testing.expectEqual(@as(usize, 6), provider.count());
    provider.visit(.{
        .ctx = &seen,
        .visit_fn = Collect.visit,
    });

    try std.testing.expectEqual(@as(usize, 6), seen.items.len);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 1, .generation = 1 }), seen.items[0]);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 2, .generation = 1 }), seen.items[1]);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 3, .generation = 1 }), seen.items[2]);
    try std.testing.expectEqual(Value.fromInt(5), seen.items[3]);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 4, .generation = 1 }), seen.items[4]);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 5, .generation = 1 }), seen.items[5]);
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
    try kernel.pushFrame(child, 77);
    try kernel.pushFrameRoot(child, Value.fromHeapRef(.{ .index = 6, .generation = 1 }));

    const performed = try kernel.perform(11, Value.fromInt(7), &.{Value.fromInt(8)});
    try std.testing.expectEqual(main, performed.handler_fiber);
    try std.testing.expectEqual(@as(usize, 0), performed.handler_index);
    try std.testing.expectEqual(@as(i64, 99), performed.handler.handle_effect.asInt());

    const captured = kernel.continuation(performed.continuation).?;
    try std.testing.expectEqual(child, captured.fiber);
    try std.testing.expectEqual(main, captured.handler_fiber);
    try std.testing.expectEqual(@as(usize, 0), captured.handler_index);
    try std.testing.expectEqual(@as(usize, 1), captured.captured_roots.items.len);
    try std.testing.expectEqual(@as(usize, 1), captured.stack.frameCount());
    try std.testing.expectEqual(@as(usize, 0), try kernel.frameCount(child));
}

test "control_kernel: unhandled effects are explicit" {
    var recorder = event_sink_mod.Recorder{};
    var kernel = ControlKernel.initWithSink(std.testing.allocator, recorder.sink());
    defer kernel.deinit();

    try std.testing.expectError(error.UnhandledEffect, kernel.perform(44, Value.fromInt(1), &.{}));

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.unhandled_effects);
}

test "control_kernel: resume is one-shot and continuation-owned suspended roots disappear after resume" {
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
    try kernel.pushFrame(child, 18);
    try kernel.pushFrameRoot(child, Value.fromHeapRef(.{ .index = 9, .generation = 1 }));

    const performed = try kernel.perform(1, Value.fromHeapRef(.{ .index = 1, .generation = 1 }), &.{
        Value.fromHeapRef(.{ .index = 2, .generation = 1 }),
    });
    try std.testing.expectEqual(@as(usize, 4), kernel.provider().count());

    const resumed = try kernel.resumeContinuation(performed.continuation, Value.fromInt(42));
    try std.testing.expectEqual(child, resumed.fiber);
    try std.testing.expectEqual(@as(i64, 42), resumed.value.asInt());
    try std.testing.expectEqual(@as(usize, 2), kernel.provider().count());
    try std.testing.expectEqual(child, kernel.currentFiber());
    try std.testing.expectEqual(@as(FiberStatus, .active), kernel.fiber(child).?.status);
    try std.testing.expectEqual(@as(usize, 1), try kernel.frameCount(child));
    try std.testing.expectError(error.AlreadyResumed, kernel.resumeContinuation(performed.continuation, Value.fromInt(0)));

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.continuation_captures);
    try std.testing.expectEqual(@as(usize, 1), counters.continuation_resumes);
    try std.testing.expect(counters.fiber_activations >= 2);
}

test "control_kernel: managed stack limits and callback boundaries are explicit" {
    var kernel = ControlKernel.initWithConfig(std.testing.allocator, .{
        .stack_limits = .{ .max_frames = 1, .max_frame_roots = 1 },
    });
    defer kernel.deinit();

    const main = kernel.currentFiber();
    try kernel.pushFrame(main, 10);
    try kernel.pushFrameRoot(main, Value.fromInt(1));
    try std.testing.expectError(error.StackOverflow, kernel.pushFrame(main, 11));
    try std.testing.expectError(error.StackOverflow, kernel.pushFrameRoot(main, Value.fromInt(2)));

    const child = try kernel.createFiber(main);
    try kernel.activateFiber(child);
    try kernel.pushFrame(child, 20);
    try kernel.enterCallbackBoundary(child);
    defer kernel.exitCallbackBoundary(child) catch unreachable;
    try std.testing.expectEqual(@as(?FiberHandle, null), kernel.fiber(child).?.parent);
}

test "control_kernel: callback boundaries truncate parent backtraces and effect search" {
    var recorder = event_sink_mod.Recorder{};
    var kernel = ControlKernel.initWithSink(std.testing.allocator, recorder.sink());
    defer kernel.deinit();

    const main = kernel.currentFiber();
    try kernel.pushHandler(main, .{
        .effect = 77,
        .handle_effect = Value.fromInt(9),
    });
    try kernel.pushFrame(main, 100);

    const child = try kernel.createFiber(main);
    try kernel.activateFiber(child);
    try kernel.pushFrame(child, 200);

    const full_trace = try kernel.captureBacktrace(std.testing.allocator, child);
    defer std.testing.allocator.free(full_trace);
    try std.testing.expectEqual(@as(usize, 2), full_trace.len);
    try std.testing.expectEqual(@as(u32, 200), full_trace[0].site_id);
    try std.testing.expectEqual(@as(u32, 100), full_trace[1].site_id);

    try kernel.enterCallbackBoundary(child);
    defer kernel.exitCallbackBoundary(child) catch unreachable;
    try std.testing.expectError(error.UnhandledEffect, kernel.performAt(55, 77, Value.fromInt(1), &.{}));

    const callback_trace = try kernel.captureBacktrace(std.testing.allocator, child);
    defer std.testing.allocator.free(callback_trace);
    try std.testing.expectEqual(@as(usize, 1), callback_trace.len);
    try std.testing.expectEqual(@as(u32, 200), callback_trace[0].site_id);

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.unhandled_effects);
}
