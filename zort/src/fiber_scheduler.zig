const std = @import("std");
const control_kernel_mod = @import("control_kernel.zig");
const domain_registry_mod = @import("domain_registry.zig");
const event_sink_mod = @import("event_sink.zig");

pub const FiberHandle = control_kernel_mod.FiberHandle;
pub const DomainHandle = domain_registry_mod.DomainHandle;
pub const EventSink = event_sink_mod.EventSink;

const Lane = struct {
    current: ?FiberHandle = null,
    runnable: std.ArrayListUnmanaged(FiberHandle) = .{},
    parked: std.ArrayListUnmanaged(FiberHandle) = .{},

    fn deinit(self: *Lane, allocator: std.mem.Allocator) void {
        self.runnable.deinit(allocator);
        self.parked.deinit(allocator);
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
    };

    pub const VerifyError = error{
        DuplicateRunnableFiber,
        DuplicateParkedFiber,
        CurrentFiberAlsoRunnable,
        CurrentFiberAlsoParked,
    };

    pub fn init(allocator: std.mem.Allocator, sink: EventSink, main_domain: DomainHandle, main_fiber: FiberHandle) FiberScheduler {
        var scheduler = FiberScheduler{
            .allocator = allocator,
            .event_sink = sink,
        };
        scheduler.registerDomain(main_domain) catch @panic("zort: out of memory while creating main scheduler lane");
        scheduler.setCurrent(main_domain, main_fiber) catch unreachable;
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

    pub fn parkedCount(self: *const FiberScheduler, domain: DomainHandle) usize {
        const lane_state = self.lane(domain) orelse return 0;
        return lane_state.parked.items.len;
    }

    pub fn setCurrent(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle) Error!void {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        _ = removeHandle(&lane_state.runnable, fiber);
        _ = removeHandle(&lane_state.parked, fiber);
        lane_state.current = fiber;
    }

    pub fn enqueue(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle) !void {
        const lane_state = try self.ensureLane(domain);
        if (lane_state.current) |active_fiber| {
            if (sameFiber(active_fiber, fiber)) return;
        }
        if (containsHandle(lane_state.runnable.items, fiber)) return;
        _ = removeHandle(&lane_state.parked, fiber);
        try lane_state.runnable.append(self.allocator, fiber);
        self.emit(.fiber_enqueue, fiber, domain);
    }

    pub fn switchToNext(self: *FiberScheduler, domain: DomainHandle) Error!?FiberHandle {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        lane_state.current = popFront(&lane_state.runnable);
        return lane_state.current;
    }

    pub fn yieldCurrent(self: *FiberScheduler, domain: DomainHandle) !?FiberHandle {
        const lane_state = try self.ensureLane(domain);
        if (lane_state.current) |active_fiber| {
            try lane_state.runnable.append(self.allocator, active_fiber);
            self.emit(.fiber_yield, active_fiber, domain);
        }
        lane_state.current = null;
        const next = popFront(&lane_state.runnable);
        lane_state.current = next;
        return next;
    }

    pub fn parkCurrent(self: *FiberScheduler, domain: DomainHandle) !?FiberHandle {
        const lane_state = try self.ensureLane(domain);
        if (lane_state.current) |active_fiber| {
            try lane_state.parked.append(self.allocator, active_fiber);
            self.emit(.fiber_park, active_fiber, domain);
        }
        lane_state.current = null;
        const next = popFront(&lane_state.runnable);
        lane_state.current = next;
        return next;
    }

    pub fn unpark(self: *FiberScheduler, domain: DomainHandle, fiber: FiberHandle) Error!void {
        const lane_state = self.laneMut(domain) orelse return error.InvalidDomain;
        if (!removeHandle(&lane_state.parked, fiber)) return;
        if (lane_state.current) |active_fiber| {
            if (sameFiber(active_fiber, fiber)) return;
        }
        if (!containsHandle(lane_state.runnable.items, fiber)) {
            lane_state.runnable.append(self.allocator, fiber) catch @panic("zort: out of memory while unparking fiber");
        }
        self.emit(.fiber_unpark, fiber, domain);
    }

    pub fn verify(self: *const FiberScheduler) VerifyError!void {
        for (self.lanes.items) |slot| {
            if (!slot.alive) continue;
            if (slot.lane.current) |active_fiber| {
                if (containsHandle(slot.lane.runnable.items, active_fiber)) return error.CurrentFiberAlsoRunnable;
                if (containsHandle(slot.lane.parked.items, active_fiber)) return error.CurrentFiberAlsoParked;
            }
            if (hasDuplicates(slot.lane.runnable.items)) return error.DuplicateRunnableFiber;
            if (hasDuplicates(slot.lane.parked.items)) return error.DuplicateParkedFiber;
        }
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

fn sameFiber(lhs: FiberHandle, rhs: FiberHandle) bool {
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

test "fiber_scheduler: per-domain runnable queues are explicit" {
    var scheduler = FiberScheduler.init(
        std.testing.allocator,
        EventSink.noop(),
        .{ .index = 0, .generation = 1 },
        .{ .index = 0, .generation = 1 },
    );
    defer scheduler.deinit();

    const domain = DomainHandle{ .index = 1, .generation = 1 };
    try scheduler.registerDomain(domain);
    try scheduler.enqueue(domain, .{ .index = 1, .generation = 1 });
    try scheduler.enqueue(domain, .{ .index = 2, .generation = 1 });

    try std.testing.expectEqual(@as(usize, 2), scheduler.runnableCount(domain));
    try std.testing.expectEqual(@as(?FiberHandle, null), scheduler.current(domain));
    try std.testing.expectEqual(FiberHandle{ .index = 1, .generation = 1 }, (try scheduler.switchToNext(domain)).?);
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
    try scheduler.enqueue(main_domain, .{ .index = 1, .generation = 1 });
    try std.testing.expectEqual(FiberHandle{ .index = 1, .generation = 1 }, (try scheduler.yieldCurrent(main_domain)).?);
    try std.testing.expectEqual(FiberHandle{ .index = 1, .generation = 1 }, scheduler.current(main_domain).?);

    try scheduler.enqueue(main_domain, .{ .index = 2, .generation = 1 });
    try std.testing.expectEqual(FiberHandle{ .index = 0, .generation = 1 }, (try scheduler.parkCurrent(main_domain)).?);
    try std.testing.expectEqual(@as(usize, 1), scheduler.parkedCount(main_domain));
    try std.testing.expectEqual(FiberHandle{ .index = 0, .generation = 1 }, scheduler.current(main_domain).?);

    try scheduler.unpark(main_domain, .{ .index = 1, .generation = 1 });
    try std.testing.expectEqual(@as(usize, 2), scheduler.runnableCount(main_domain));
    try std.testing.expectEqual(@as(usize, 0), scheduler.parkedCount(main_domain));
    try scheduler.verify();
}
