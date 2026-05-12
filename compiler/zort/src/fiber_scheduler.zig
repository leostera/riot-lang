const std = @import("std");
const atomic_mod = @import("atomic_primitives.zig");
const control_kernel_mod = @import("control_kernel.zig");
const domain_registry_mod = @import("domain_registry.zig");
const event_sink_mod = @import("event_sink.zig");

pub const FiberHandle = control_kernel_mod.FiberHandle;
pub const DomainHandle = domain_registry_mod.DomainHandle;
pub const EventSink = event_sink_mod.EventSink;

const AtomicCounter = atomic_mod.AtomicCounter;
const AtomicFlag = atomic_mod.AtomicFlag;
const OptionalTokenCell = atomic_mod.OptionalTokenCell;
const OptionalFiberCell = atomic_mod.OptionalHandleCell(FiberHandle);

pub const LaneCoordinationSnapshot = struct {
    state_epoch: usize,
    ownership_epoch: usize,
    runnable_count: usize,
    parked_count: usize,
    suspended_count: usize,
    wake_requested: bool,
    owner_token: ?u64,
    current: ?FiberHandle,
};

const LaneCoordination = struct {
    state_epoch: AtomicCounter = AtomicCounter.init(0),
    ownership_epoch: AtomicCounter = AtomicCounter.init(0),
    runnable_count: AtomicCounter = AtomicCounter.init(0),
    parked_count: AtomicCounter = AtomicCounter.init(0),
    suspended_count: AtomicCounter = AtomicCounter.init(0),
    wake_requested: AtomicFlag = AtomicFlag.init(false),
    owner_token: OptionalTokenCell = OptionalTokenCell.init(null),
    current: OptionalFiberCell = OptionalFiberCell.init(null),

    fn sync(self: *LaneCoordination, current: ?FiberHandle, runnable_count: usize, parked_count: usize, suspended_count: usize) void {
        self.current.store(current);
        self.runnable_count.store(runnable_count);
        self.parked_count.store(parked_count);
        self.suspended_count.store(suspended_count);
        _ = self.state_epoch.increment();
    }

    fn requestWake(self: *LaneCoordination) void {
        if (!self.wake_requested.set()) _ = self.state_epoch.increment();
    }

    fn clearWake(self: *LaneCoordination) void {
        if (self.wake_requested.clear()) _ = self.state_epoch.increment();
    }

    fn takeWake(self: *LaneCoordination) bool {
        const had_wake = self.wake_requested.take();
        if (had_wake) _ = self.state_epoch.increment();
        return had_wake;
    }

    fn snapshot(self: *const LaneCoordination) LaneCoordinationSnapshot {
        return .{
            .state_epoch = self.state_epoch.load(),
            .ownership_epoch = self.ownership_epoch.load(),
            .runnable_count = self.runnable_count.load(),
            .parked_count = self.parked_count.load(),
            .suspended_count = self.suspended_count.load(),
            .wake_requested = self.wake_requested.isSet(),
            .owner_token = self.owner_token.load(),
            .current = self.current.load(),
        };
    }

    fn claim(self: *LaneCoordination, token: u64) bool {
        if (self.owner_token.claim(token)) {
            _ = self.ownership_epoch.increment();
            return true;
        }
        return self.owner_token.load() == token;
    }

    fn release(self: *LaneCoordination, token: u64) bool {
        if (self.owner_token.release(token)) {
            _ = self.ownership_epoch.increment();
            return true;
        }
        return false;
    }
};

const Lane = struct {
    current: ?FiberHandle = null,
    runnable: std.ArrayListUnmanaged(FiberHandle) = .{},
    parked: std.ArrayListUnmanaged(FiberHandle) = .{},
    suspended: std.ArrayListUnmanaged(FiberHandle) = .{},
    coordination: LaneCoordination = .{},

    fn deinit(self: *Lane, allocator: std.mem.Allocator) void {
        self.runnable.deinit(allocator);
        self.parked.deinit(allocator);
        self.suspended.deinit(allocator);
        self.* = .{};
    }
};

const LaneSlot = struct {
    generation: u32,
    alive: bool,
    lane: Lane,
};

pub const FiberScheduler = struct {
    allocator: std.mem.Allocator,
    event_sink: EventSink,
    lanes: std.ArrayListUnmanaged(LaneSlot) = .{},

    pub const Error = error{
        InvalidDomain,
        LaneNotOwned,
        FiberNotRunnable,
        FiberAlreadyOwned,
    };

    pub const VerifyError = error{
        DuplicateRunnableFiber,
        DuplicateParkedFiber,
        DuplicateSuspendedFiber,
        CurrentFiberAlsoRunnable,
        CurrentFiberAlsoParked,
        CurrentFiberAlsoSuspended,
        FiberOwnedMultipleTimes,
    };

    pub fn init(allocator: std.mem.Allocator, sink: EventSink, main_domain: DomainHandle, main_fiber: FiberHandle) FiberScheduler {
        var scheduler = FiberScheduler{
            .allocator = allocator,
            .event_sink = sink,
        };
        scheduler.registerDomain(main_domain) catch @panic("zort: out of memory while creating main scheduler lane");
        scheduler.setCurrentUnchecked(main_domain, main_fiber) catch unreachable;
        return scheduler;
    }

    pub fn deinit(self: *FiberScheduler) void {
        for (self.lanes.items) |*slot| {
            if (slot.alive) slot.lane.deinit(self.allocator);
        }
        self.lanes.deinit(self.allocator);
    }

    pub fn registerDomain(self: *FiberScheduler, domain: DomainHandle) !void {
        _ = try self.ensureLane(domain);
    }

    pub fn current(self: *const FiberScheduler, domain: DomainHandle) ?FiberHandle {
        const lane_state = self.lane(domain) orelse return null;
        return lane_state.current;
    }

    pub fn runnableCount(self: *const FiberScheduler, domain: DomainHandle) usize {
        const lane_state = self.lane(domain) orelse return 0;
        return lane_state.runnable.items.len;
    }

    pub fn coordinationSnapshot(self: *const FiberScheduler, domain: DomainHandle) Error!LaneCoordinationSnapshot {
        const lane_state = self.lane(domain) orelse return error.InvalidDomain;
        return lane_state.coordination.snapshot();
    }

    pub fn claimLaneOwnership(self: *FiberScheduler, domain: DomainHandle, token: u64) Error!bool {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        return lane_state.coordination.claim(token);
    }

    pub fn releaseLaneOwnership(self: *FiberScheduler, domain: DomainHandle, token: u64) Error!bool {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        return lane_state.coordination.release(token);
    }

    pub fn parkedCount(self: *const FiberScheduler, domain: DomainHandle) usize {
        const lane_state = self.lane(domain) orelse return 0;
        return lane_state.parked.items.len;
    }

    pub fn suspendedCount(self: *const FiberScheduler, domain: DomainHandle) usize {
        const lane_state = self.lane(domain) orelse return 0;
        return lane_state.suspended.items.len;
    }

    pub fn setCurrent(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle, owner_token: u64) Error!void {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        setCurrentInLane(lane_state, fiber);
    }

    fn setCurrentUnchecked(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle) Error!void {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        setCurrentInLane(lane_state, fiber);
    }

    fn setCurrentInLane(lane_state: *Lane, fiber: FiberHandle) void {
        _ = removeHandle(&lane_state.runnable, fiber);
        _ = removeHandle(&lane_state.parked, fiber);
        _ = removeHandle(&lane_state.suspended, fiber);
        lane_state.current = fiber;
        syncLaneCoordination(lane_state);
        lane_state.coordination.clearWake();
    }

    pub fn activate(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle, owner_token: u64) !void {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        if (lane_state.current) |active_fiber| {
            if (!sameFiber(active_fiber, fiber) and
                !containsHandle(lane_state.runnable.items, active_fiber) and
                !containsHandle(lane_state.parked.items, active_fiber) and
                !containsHandle(lane_state.suspended.items, active_fiber))
            {
                try lane_state.runnable.append(self.allocator, active_fiber);
                self.emit(.fiber_enqueue, active_fiber, domain);
            }
        }
        _ = removeHandle(&lane_state.runnable, fiber);
        _ = removeHandle(&lane_state.parked, fiber);
        _ = removeHandle(&lane_state.suspended, fiber);
        lane_state.current = fiber;
        syncLaneCoordination(lane_state);
        lane_state.coordination.clearWake();
    }

    pub fn suspendCurrent(self: *FiberScheduler, domain: DomainHandle, owner_token: u64) !?FiberHandle {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        const active_fiber = lane_state.current orelse return null;
        lane_state.current = null;
        if (!containsHandle(lane_state.suspended.items, active_fiber)) {
            try lane_state.suspended.append(self.allocator, active_fiber);
        }
        syncLaneCoordination(lane_state);
        lane_state.coordination.clearWake();
        return active_fiber;
    }

    pub fn discardSuspended(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle, owner_token: u64) Error!bool {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        const removed = removeHandle(&lane_state.suspended, fiber);
        if (removed) syncLaneCoordination(lane_state);
        return removed;
    }

    pub fn ownsFiber(self: *const FiberScheduler, needle: FiberHandle) bool {
        for (self.lanes.items) |slot| {
            if (!slot.alive) continue;
            if (slot.lane.current) |active_fiber| {
                if (sameFiber(active_fiber, needle)) return true;
            }
            if (containsHandle(slot.lane.runnable.items, needle)) return true;
            if (containsHandle(slot.lane.parked.items, needle)) return true;
            if (containsHandle(slot.lane.suspended.items, needle)) return true;
        }
        return false;
    }

    pub fn visitOwnedFibers(
        self: *const FiberScheduler,
        context: anytype,
        comptime visit: fn (@TypeOf(context), DomainHandle, FiberHandle) void,
    ) void {
        for (self.lanes.items, 0..) |slot, slot_index| {
            if (!slot.alive) continue;
            const domain = DomainHandle{
                .index = @intCast(slot_index),
                .generation = slot.generation,
            };
            if (slot.lane.current) |fiber| visit(context, domain, fiber);
            for (slot.lane.runnable.items) |fiber| visit(context, domain, fiber);
            for (slot.lane.parked.items) |fiber| visit(context, domain, fiber);
            for (slot.lane.suspended.items) |fiber| visit(context, domain, fiber);
        }
    }

    pub fn enqueue(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle, owner_token: u64) !void {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        if (lane_state.current) |active_fiber| {
            if (sameFiber(active_fiber, fiber)) return;
        }
        if (containsHandle(lane_state.runnable.items, fiber)) return;
        _ = removeHandle(&lane_state.parked, fiber);
        _ = removeHandle(&lane_state.suspended, fiber);
        try lane_state.runnable.append(self.allocator, fiber);
        syncLaneCoordination(lane_state);
        lane_state.coordination.requestWake();
        self.emit(.fiber_enqueue, fiber, domain);
    }

    pub fn requestWake(self: *FiberScheduler, domain: DomainHandle) Error!void {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        lane_state.coordination.requestWake();
    }

    pub fn takeWakeRequest(self: *FiberScheduler, domain: DomainHandle) Error!bool {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        return lane_state.coordination.takeWake();
    }

    pub fn switchToNext(self: *FiberScheduler, domain: DomainHandle, owner_token: u64) Error!?FiberHandle {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        if (lane_state.current) |active_fiber| {
            if (!containsHandle(lane_state.runnable.items, active_fiber) and
                !containsHandle(lane_state.parked.items, active_fiber) and
                !containsHandle(lane_state.suspended.items, active_fiber))
            {
                lane_state.runnable.append(self.allocator, active_fiber) catch @panic("zort: out of memory while rotating scheduler lane");
                self.emit(.fiber_enqueue, active_fiber, domain);
            }
        }
        lane_state.current = popFront(&lane_state.runnable);
        syncLaneCoordination(lane_state);
        lane_state.coordination.clearWake();
        return lane_state.current;
    }

    pub fn yieldCurrent(self: *FiberScheduler, domain: DomainHandle, owner_token: u64) !?FiberHandle {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        if (lane_state.current) |active_fiber| {
            try lane_state.runnable.append(self.allocator, active_fiber);
            self.emit(.fiber_yield, active_fiber, domain);
        }
        lane_state.current = null;
        const next = popFront(&lane_state.runnable);
        lane_state.current = next;
        syncLaneCoordination(lane_state);
        lane_state.coordination.clearWake();
        return next;
    }

    pub fn parkCurrent(self: *FiberScheduler, domain: DomainHandle, owner_token: u64) !?FiberHandle {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        if (lane_state.current) |active_fiber| {
            try lane_state.parked.append(self.allocator, active_fiber);
            self.emit(.fiber_park, active_fiber, domain);
        }
        lane_state.current = null;
        const next = popFront(&lane_state.runnable);
        lane_state.current = next;
        syncLaneCoordination(lane_state);
        lane_state.coordination.clearWake();
        return next;
    }

    pub fn unpark(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle, owner_token: u64) Error!void {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(lane_state, owner_token);
        if (!removeHandle(&lane_state.parked, fiber)) return;
        if (lane_state.current) |active_fiber| {
            if (sameFiber(active_fiber, fiber)) return;
        }
        if (!containsHandle(lane_state.runnable.items, fiber)) {
            lane_state.runnable.append(self.allocator, fiber) catch @panic("zort: out of memory while unparking fiber");
        }
        syncLaneCoordination(lane_state);
        lane_state.coordination.requestWake();
        self.emit(.fiber_unpark, fiber, domain);
    }

    pub fn transferRunnable(
        self: *FiberScheduler,
        source_domain: DomainHandle,
        target_domain: DomainHandle,
        fiber: FiberHandle,
        source_owner_token: u64,
        target_owner_token: u64,
    ) Error!bool {
        if (sameDomain(source_domain, target_domain)) return false;

        const source_lane = self.laneMut(source_domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(source_lane, source_owner_token);
        const target_lane = self.laneMut(target_domain) orelse return error.InvalidDomain;
        try requireLaneOwnership(target_lane, target_owner_token);

        if (target_lane.current) |active_fiber| {
            if (sameFiber(active_fiber, fiber)) return error.FiberAlreadyOwned;
        }
        if (containsHandle(target_lane.runnable.items, fiber) or
            containsHandle(target_lane.parked.items, fiber) or
            containsHandle(target_lane.suspended.items, fiber))
        {
            return error.FiberAlreadyOwned;
        }

        if (!removeHandle(&source_lane.runnable, fiber)) return error.FiberNotRunnable;
        target_lane.runnable.append(self.allocator, fiber) catch @panic("zort: out of memory while transferring runnable fiber");
        syncLaneCoordination(source_lane);
        syncLaneCoordination(target_lane);
        target_lane.coordination.requestWake();
        self.emit(.fiber_enqueue, fiber, target_domain);
        return true;
    }

    pub fn verify(self: *const FiberScheduler) VerifyError!void {
        for (self.lanes.items) |slot| {
            if (!slot.alive) continue;
            if (slot.lane.current) |active_fiber| {
                if (containsHandle(slot.lane.runnable.items, active_fiber)) return error.CurrentFiberAlsoRunnable;
                if (containsHandle(slot.lane.parked.items, active_fiber)) return error.CurrentFiberAlsoParked;
                if (containsHandle(slot.lane.suspended.items, active_fiber)) return error.CurrentFiberAlsoSuspended;
            }
            if (hasDuplicates(slot.lane.runnable.items)) return error.DuplicateRunnableFiber;
            if (hasDuplicates(slot.lane.parked.items)) return error.DuplicateParkedFiber;
            if (hasDuplicates(slot.lane.suspended.items)) return error.DuplicateSuspendedFiber;
        }
        if (hasDuplicateOwnedFiber(self)) return error.FiberOwnedMultipleTimes;
    }

    fn lane(self: *const FiberScheduler, domain: DomainHandle) ?*const Lane {
        if (domain.index >= self.lanes.items.len) return null;
        const slot = &self.lanes.items[domain.index];
        if (!slot.alive or slot.generation != domain.generation) return null;
        return &slot.lane;
    }

    fn laneMut(self: *FiberScheduler, domain: DomainHandle) ?*Lane {
        if (domain.index >= self.lanes.items.len) return null;
        const slot = &self.lanes.items[domain.index];
        if (!slot.alive or slot.generation != domain.generation) return null;
        return &slot.lane;
    }

    fn ensureLane(self: *FiberScheduler, domain: DomainHandle) !*Lane {
        const needed_len: usize = @intCast(domain.index + 1);
        if (self.lanes.items.len < needed_len) {
            const old_len = self.lanes.items.len;
            try self.lanes.resize(self.allocator, needed_len);
            for (self.lanes.items[old_len..]) |*slot| {
                slot.* = .{
                    .generation = 0,
                    .alive = false,
                    .lane = .{},
                };
            }
        }

        const slot = &self.lanes.items[domain.index];
        if (!slot.alive) {
            slot.generation = domain.generation;
            slot.alive = true;
            slot.lane = .{};
        } else if (slot.generation != domain.generation) {
            slot.lane.deinit(self.allocator);
            slot.generation = domain.generation;
            slot.alive = true;
            slot.lane = .{};
        }
        return &slot.lane;
    }

    fn emit(self: *FiberScheduler, action: event_sink_mod.ControlAction, fiber: FiberHandle, domain: DomainHandle) void {
        self.event_sink.emit(.{ .control = .{
            .action = action,
            .fiber = .{ .index = fiber.index, .generation = fiber.generation },
            .handler_fiber = .{ .index = domain.index, .generation = domain.generation },
        } });
    }
};

fn requireLaneOwnership(lane_state: *Lane, owner_token: u64) FiberScheduler.Error!void {
    if (lane_state.coordination.owner_token.load() != owner_token) return error.LaneNotOwned;
}

fn syncLaneCoordination(lane_state: *Lane) void {
    lane_state.coordination.sync(
        lane_state.current,
        lane_state.runnable.items.len,
        lane_state.parked.items.len,
        lane_state.suspended.items.len,
    );
}

fn sameFiber(lhs: FiberHandle, rhs: FiberHandle) bool {
    return lhs.index == rhs.index and lhs.generation == rhs.generation;
}

fn sameDomain(lhs: DomainHandle, rhs: DomainHandle) bool {
    return lhs.index == rhs.index and lhs.generation == rhs.generation;
}

fn containsHandle(items: []const FiberHandle, needle: FiberHandle) bool {
    for (items) |item| {
        if (sameFiber(item, needle)) return true;
    }
    return false;
}

fn removeHandle(items: *std.ArrayListUnmanaged(FiberHandle), needle: FiberHandle) bool {
    for (items.items, 0..) |item, index| {
        if (!sameFiber(item, needle)) continue;
        _ = items.swapRemove(index);
        return true;
    }
    return false;
}

fn popFront(items: *std.ArrayListUnmanaged(FiberHandle)) ?FiberHandle {
    if (items.items.len == 0) return null;
    const head = items.items[0];
    if (items.items.len > 1) {
        std.mem.copyForwards(FiberHandle, items.items[0 .. items.items.len - 1], items.items[1..]);
    }
    items.items.len -= 1;
    return head;
}

fn hasDuplicates(items: []const FiberHandle) bool {
    for (items, 0..) |item, index| {
        for (items[index + 1 ..]) |other| {
            if (sameFiber(item, other)) return true;
        }
    }
    return false;
}

fn hasDuplicateOwnedFiber(self: *const FiberScheduler) bool {
    const Outer = struct {
        scheduler: *const FiberScheduler,
        duplicate: bool = false,

        fn visitOuter(ctx: *@This(), _: DomainHandle, fiber: FiberHandle) void {
            if (ctx.duplicate) return;
            const Inner = struct {
                fn visitInner(inner_ctx: *@This(), _: DomainHandle, candidate: FiberHandle) void {
                    if (!sameFiber(inner_ctx.fiber, candidate)) return;
                    inner_ctx.seen += 1;
                    if (inner_ctx.seen > 1) inner_ctx.duplicate.* = true;
                }

                fiber: FiberHandle,
                seen: usize,
                duplicate: *bool,
            };
            var inner = Inner{
                .fiber = fiber,
                .seen = 0,
                .duplicate = &ctx.duplicate,
            };
            ctx.scheduler.visitOwnedFibers(&inner, Inner.visitInner);
        }
    };

    var outer = Outer{ .scheduler = self };
    self.visitOwnedFibers(&outer, Outer.visitOuter);
    return outer.duplicate;
}

test "fiber_scheduler: per-domain runnable queues are explicit" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 0, .generation = 1 },
    );
    defer scheduler.deinit();

    const domain = DomainHandle{ .index = 1, .generation = 1 };
    const worker_owner: u64 = 99;
    try scheduler.registerDomain(domain);
    try std.testing.expect(try scheduler.claimLaneOwnership(domain, worker_owner));
    try scheduler.enqueue(domain, .{ .index = 1, .generation = 1 }, worker_owner);
    try scheduler.enqueue(domain, .{ .index = 2, .generation = 1 }, worker_owner);

    try std.testing.expectEqual(@as(usize, 2), scheduler.runnableCount(domain));
    try std.testing.expectEqual(@as(?FiberHandle, null), scheduler.current(domain));
    try std.testing.expectEqual(FiberHandle{ .index = 1, .generation = 1 }, (try scheduler.switchToNext(domain, worker_owner)).?);
    try std.testing.expectEqual(@as(usize, 1), scheduler.runnableCount(domain));
}

test "fiber_scheduler: yielding and parking rotate domain-local work" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 0, .generation = 1 },
    );
    defer scheduler.deinit();

    const main_domain = DomainHandle{ .index = 0, .generation = 1 };
    const main_owner: u64 = 1;
    try std.testing.expect(try scheduler.claimLaneOwnership(main_domain, main_owner));
    try scheduler.enqueue(main_domain, .{ .index = 1, .generation = 1 }, main_owner);
    try std.testing.expectEqual(FiberHandle{ .index = 1, .generation = 1 }, (try scheduler.yieldCurrent(main_domain, main_owner)).?);
    try std.testing.expectEqual(FiberHandle{ .index = 1, .generation = 1 }, scheduler.current(main_domain).?);

    try scheduler.enqueue(main_domain, .{ .index = 2, .generation = 1 }, main_owner);
    try std.testing.expectEqual(FiberHandle{ .index = 0, .generation = 1 }, (try scheduler.parkCurrent(main_domain, main_owner)).?);
    try std.testing.expectEqual(@as(usize, 1), scheduler.parkedCount(main_domain));
    try std.testing.expectEqual(FiberHandle{ .index = 0, .generation = 1 }, scheduler.current(main_domain).?);

    try scheduler.unpark(main_domain, .{ .index = 1, .generation = 1 }, main_owner);
    try std.testing.expectEqual(@as(usize, 2), scheduler.runnableCount(main_domain));
    try std.testing.expectEqual(@as(usize, 0), scheduler.parkedCount(main_domain));
    try scheduler.verify();
}

test "fiber_scheduler: ownership traversal sees current runnable and parked fibers" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 1, .generation = 1 },
    );
    defer scheduler.deinit();

    const main_domain = DomainHandle{ .index = 0, .generation = 1 };
    const worker_domain = DomainHandle{ .index = 1, .generation = 1 };
    try scheduler.registerDomain(worker_domain);

    const runnable = FiberHandle{ .index = 2, .generation = 1 };
    const parked = FiberHandle{ .index = 3, .generation = 1 };
    const worker_current = FiberHandle{ .index = 4, .generation = 1 };
    const suspended = FiberHandle{ .index = 5, .generation = 1 };
    const main_owner: u64 = 1;
    const worker_owner: u64 = 2;

    try std.testing.expect(try scheduler.claimLaneOwnership(main_domain, main_owner));
    try std.testing.expect(try scheduler.claimLaneOwnership(worker_domain, worker_owner));
    try scheduler.enqueue(main_domain, runnable, main_owner);
    try scheduler.enqueue(main_domain, parked, main_owner);
    _ = try scheduler.parkCurrent(main_domain, main_owner);
    try scheduler.setCurrent(worker_domain, worker_current, worker_owner);
    try scheduler.enqueue(worker_domain, suspended, worker_owner);
    _ = try scheduler.suspendCurrent(worker_domain, worker_owner);

    var seen = std.ArrayListUnmanaged(FiberHandle){};
    defer seen.deinit(std.testing.allocator);

    const Collect = struct {
        fn visit(ctx: *std.ArrayListUnmanaged(FiberHandle), _: DomainHandle, fiber: FiberHandle) void {
            ctx.append(std.testing.allocator, fiber) catch unreachable;
        }
    };

    scheduler.visitOwnedFibers(&seen, Collect.visit);
    try std.testing.expectEqual(@as(usize, 5), seen.items.len);
    try std.testing.expect(scheduler.ownsFiber(.{ .index = 1, .generation = 1 }));
    try std.testing.expect(scheduler.ownsFiber(runnable));
    try std.testing.expect(scheduler.ownsFiber(parked));
    try std.testing.expect(scheduler.ownsFiber(worker_current));
    try std.testing.expect(scheduler.ownsFiber(suspended));
    try std.testing.expectEqual(@as(usize, 1), scheduler.suspendedCount(worker_domain));
}

test "fiber_scheduler: coordination snapshots mirror queue state and wake requests" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 0, .generation = 1 },
    );
    defer scheduler.deinit();

    const worker_domain = DomainHandle{ .index = 1, .generation = 1 };
    const worker = FiberHandle{ .index = 9, .generation = 1 };
    try scheduler.registerDomain(worker_domain);

    var snapshot = try scheduler.coordinationSnapshot(worker_domain);
    try std.testing.expectEqual(@as(usize, 0), snapshot.runnable_count);
    try std.testing.expectEqual(@as(?FiberHandle, null), snapshot.current);
    try std.testing.expect(!snapshot.wake_requested);
    try std.testing.expectEqual(@as(?u64, null), snapshot.owner_token);

    try std.testing.expect(try scheduler.claimLaneOwnership(worker_domain, 99));
    try scheduler.enqueue(worker_domain, worker, 99);
    snapshot = try scheduler.coordinationSnapshot(worker_domain);
    try std.testing.expectEqual(@as(usize, 1), snapshot.runnable_count);
    try std.testing.expectEqual(@as(?FiberHandle, null), snapshot.current);
    try std.testing.expect(snapshot.wake_requested);
    try std.testing.expectEqual(@as(?u64, 99), snapshot.owner_token);

    try std.testing.expect(try scheduler.takeWakeRequest(worker_domain));
    snapshot = try scheduler.coordinationSnapshot(worker_domain);
    try std.testing.expect(!snapshot.wake_requested);

    snapshot = try scheduler.coordinationSnapshot(worker_domain);
    try std.testing.expectEqual(@as(?u64, 99), snapshot.owner_token);
    try std.testing.expect(snapshot.ownership_epoch >= 1);
    try std.testing.expect(!(try scheduler.releaseLaneOwnership(worker_domain, 7)));

    try std.testing.expectEqual(worker, (try scheduler.switchToNext(worker_domain, 99)).?);
    snapshot = try scheduler.coordinationSnapshot(worker_domain);
    try std.testing.expectEqual(worker, snapshot.current.?);
    try std.testing.expectEqual(@as(usize, 0), snapshot.runnable_count);
    try std.testing.expect(!snapshot.wake_requested);
    try std.testing.expect(try scheduler.releaseLaneOwnership(worker_domain, 99));

    try scheduler.requestWake(worker_domain);
    snapshot = try scheduler.coordinationSnapshot(worker_domain);
    try std.testing.expect(snapshot.wake_requested);
    try std.testing.expect(try scheduler.takeWakeRequest(worker_domain));
    try std.testing.expect(!(try scheduler.takeWakeRequest(worker_domain)));
}

test "fiber_scheduler: lane ownership claims are atomic" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 0, .generation = 1 },
    );
    defer scheduler.deinit();

    const worker_domain = DomainHandle{ .index = 1, .generation = 1 };
    try scheduler.registerDomain(worker_domain);

    const ClaimContext = struct {
        scheduler: *FiberScheduler,
        domain: DomainHandle,
        token: u64,
        result: *bool,
    };
    const Claim = struct {
        fn run(ctx: *ClaimContext) void {
            ctx.result.* = ctx.scheduler.claimLaneOwnership(ctx.domain, ctx.token) catch unreachable;
        }
    };

    var results = [_]bool{ false, false };
    var contexts = [_]ClaimContext{
        .{ .scheduler = &scheduler, .domain = worker_domain, .token = 1, .result = &results[0] },
        .{ .scheduler = &scheduler, .domain = worker_domain, .token = 2, .result = &results[1] },
    };
    var threads: [2]std.Thread = undefined;
    for (&threads, &contexts) |*thread, *ctx| {
        thread.* = try std.Thread.spawn(.{}, Claim.run, .{ctx});
    }
    for (threads) |thread| thread.join();

    try std.testing.expect(results[0] != results[1]);
    const snapshot = try scheduler.coordinationSnapshot(worker_domain);
    try std.testing.expect(snapshot.owner_token == 1 or snapshot.owner_token == 2);
}

test "fiber_scheduler: mutable lane paths require claimed ownership" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 0, .generation = 1 },
    );
    defer scheduler.deinit();

    const main_domain = DomainHandle{ .index = 0, .generation = 1 };
    try std.testing.expectError(error.LaneNotOwned, scheduler.enqueue(main_domain, .{ .index = 1, .generation = 1 }, 7));
    try std.testing.expect(try scheduler.claimLaneOwnership(main_domain, 7));
    try scheduler.enqueue(main_domain, .{ .index = 1, .generation = 1 }, 7);
    try std.testing.expectError(error.LaneNotOwned, scheduler.switchToNext(main_domain, 8));
    try std.testing.expectEqual(FiberHandle{ .index = 1, .generation = 1 }, (try scheduler.switchToNext(main_domain, 7)).?);
}

test "fiber_scheduler: runnable transfer moves ownership between claimed lanes" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 0, .generation = 1 },
    );
    defer scheduler.deinit();

    const source_domain = DomainHandle{ .index = 0, .generation = 1 };
    const target_domain = DomainHandle{ .index = 1, .generation = 1 };
    try scheduler.registerDomain(target_domain);
    try std.testing.expect(try scheduler.claimLaneOwnership(source_domain, 1));
    try std.testing.expect(try scheduler.claimLaneOwnership(target_domain, 2));

    const worker = FiberHandle{ .index = 7, .generation = 1 };
    try scheduler.enqueue(source_domain, worker, 1);
    try std.testing.expect(try scheduler.transferRunnable(source_domain, target_domain, worker, 1, 2));
    try std.testing.expectEqual(@as(usize, 0), scheduler.runnableCount(source_domain));
    try std.testing.expectEqual(@as(usize, 1), scheduler.runnableCount(target_domain));
    try std.testing.expect((try scheduler.coordinationSnapshot(target_domain)).wake_requested);
}

test "fiber_scheduler: runnable transfer rejects non-runnable fibers" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 0, .generation = 1 },
    );
    defer scheduler.deinit();

    const source_domain = DomainHandle{ .index = 0, .generation = 1 };
    const target_domain = DomainHandle{ .index = 1, .generation = 1 };
    try scheduler.registerDomain(target_domain);
    try std.testing.expect(try scheduler.claimLaneOwnership(source_domain, 1));
    try std.testing.expect(try scheduler.claimLaneOwnership(target_domain, 2));

    try std.testing.expectError(
        error.FiberNotRunnable,
        scheduler.transferRunnable(source_domain, target_domain, .{ .index = 7, .generation = 1 }, 1, 2),
    );
}
