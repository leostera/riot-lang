const std = @import("std");
const atomic_mod = @import("atomic_primitives.zig");
const domain_registry_mod = @import("domain_registry.zig");
const event_sink_mod = @import("event_sink.zig");

pub const DomainHandle = domain_registry_mod.DomainHandle;
pub const EventSink = event_sink_mod.EventSink;

const AtomicCounter = atomic_mod.AtomicCounter;
const AtomicFlag = atomic_mod.AtomicFlag;
const OptionalDomainCell = atomic_mod.OptionalHandleCell(DomainHandle);

pub const CoordinationSnapshot = struct {
    active: bool,
    generation: usize,
    target_pause_count: usize,
    paused_count: usize,
    pause_epoch: usize,
    resume_epoch: usize,
    initiator: ?DomainHandle,
};

const CoordinationState = struct {
    active: AtomicFlag = AtomicFlag.init(false),
    generation: AtomicCounter = AtomicCounter.init(0),
    target_pause_count: AtomicCounter = AtomicCounter.init(0),
    paused_count: AtomicCounter = AtomicCounter.init(0),
    pause_epoch: AtomicCounter = AtomicCounter.init(0),
    resume_epoch: AtomicCounter = AtomicCounter.init(0),
    initiator: OptionalDomainCell = OptionalDomainCell.init(null),

    fn beginRequest(self: *CoordinationState, generation: usize, initiator: ?DomainHandle, target_pause_count: usize) void {
        self.generation.store(generation);
        self.target_pause_count.store(target_pause_count);
        self.paused_count.store(0);
        self.initiator.store(initiator);
        self.active.store(true);
        _ = self.pause_epoch.increment();
    }

    fn notePause(self: *CoordinationState) usize {
        const paused_count = self.paused_count.increment();
        _ = self.pause_epoch.increment();
        return paused_count;
    }

    fn noteResume(self: *CoordinationState) void {
        self.target_pause_count.store(0);
        self.paused_count.store(0);
        self.initiator.store(null);
        self.active.store(false);
        _ = self.resume_epoch.increment();
    }

    fn snapshot(self: *const CoordinationState) CoordinationSnapshot {
        return .{
            .active = self.active.isSet(),
            .generation = self.generation.load(),
            .target_pause_count = self.target_pause_count.load(),
            .paused_count = self.paused_count.load(),
            .pause_epoch = self.pause_epoch.load(),
            .resume_epoch = self.resume_epoch.load(),
            .initiator = self.initiator.load(),
        };
    }
};

const PauseSlot = struct {
    generation: u32 = 0,
    alive: bool = false,
    acknowledged_generation: AtomicCounter = AtomicCounter.init(0),
};

pub const StopTheWorldCoordinator = struct {
    allocator: std.mem.Allocator,
    event_sink: EventSink,
    active: bool = false,
    generation: usize = 0,
    initiator: ?DomainHandle = null,
    pause_slots: std.ArrayListUnmanaged(PauseSlot) = .{},
    coordination: CoordinationState = .{},

    pub const Error = error{
        InvalidDomain,
    };

    pub const VerifyError = error{
        PausedWithoutActiveRequest,
        PausedCountExceedsTarget,
    };

    pub fn init(allocator: std.mem.Allocator, sink: EventSink) StopTheWorldCoordinator {
        return .{
            .allocator = allocator,
            .event_sink = sink,
        };
    }

    pub fn deinit(self: *StopTheWorldCoordinator) void {
        self.pause_slots.deinit(self.allocator);
    }

    pub fn registerDomain(self: *StopTheWorldCoordinator, domain: DomainHandle) !void {
        _ = try self.ensurePauseSlot(domain);
    }

    pub fn coordinationSnapshot(self: *const StopTheWorldCoordinator) CoordinationSnapshot {
        return self.coordination.snapshot();
    }

    pub fn request(self: *StopTheWorldCoordinator, initiator: ?DomainHandle, target_pause_count: usize) !usize {
        self.generation +%= 1;
        self.active = true;
        self.initiator = initiator;
        self.coordination.beginRequest(self.generation, initiator, target_pause_count);
        self.emit(.stw_request, initiator);
        return self.generation;
    }

    pub fn currentGeneration(self: *const StopTheWorldCoordinator) usize {
        return self.generation;
    }

    pub fn targetPauseCount(self: *const StopTheWorldCoordinator) usize {
        return self.coordination.target_pause_count.load();
    }

    pub fn shouldPause(self: *const StopTheWorldCoordinator, observed_generation: usize) bool {
        if (!self.active) return false;
        return self.generation != observed_generation;
    }

    pub fn acknowledgePause(self: *StopTheWorldCoordinator, domain: DomainHandle, observed_generation: usize) Error!bool {
        if (!self.active) return false;
        if (observed_generation != self.generation) return false;

        const slot = self.pauseSlotMut(domain) orelse return error.InvalidDomain;
        while (true) {
            const acknowledged = slot.acknowledged_generation.load();
            if (acknowledged == observed_generation) return false;
            if (slot.acknowledged_generation.compareExchange(acknowledged, observed_generation) == null) break;
        }

        _ = self.coordination.notePause();
        self.emit(.stw_pause, domain);
        return true;
    }

    pub fn resumeWorld(self: *StopTheWorldCoordinator) void {
        if (!self.active) return;
        self.active = false;
        self.initiator = null;
        self.coordination.noteResume();
        self.emit(.stw_resume, null);
    }

    pub fn isPaused(self: *const StopTheWorldCoordinator, domain: DomainHandle) bool {
        if (!self.active) return false;
        const slot = self.pauseSlot(domain) orelse return false;
        return slot.acknowledged_generation.load() == self.generation;
    }

    pub fn pausedCount(self: *const StopTheWorldCoordinator) usize {
        return self.coordination.paused_count.load();
    }

    pub fn allPaused(self: *const StopTheWorldCoordinator) bool {
        if (!self.active) return true;
        return self.pausedCount() >= self.targetPauseCount();
    }

    pub fn isActive(self: *const StopTheWorldCoordinator) bool {
        return self.active;
    }

    pub fn verify(self: *const StopTheWorldCoordinator) VerifyError!void {
        if (!self.active and self.pausedCount() != 0) return error.PausedWithoutActiveRequest;
        if (self.active and self.pausedCount() > self.targetPauseCount()) return error.PausedCountExceedsTarget;
    }

    fn ensurePauseSlot(self: *StopTheWorldCoordinator, domain: DomainHandle) !*PauseSlot {
        const needed_len: usize = @intCast(domain.index + 1);
        if (self.pause_slots.items.len < needed_len) {
            const old_len = self.pause_slots.items.len;
            try self.pause_slots.resize(self.allocator, needed_len);
            for (self.pause_slots.items[old_len..]) |*slot| {
                slot.* = .{};
            }
        }

        const slot = &self.pause_slots.items[domain.index];
        if (!slot.alive) {
            slot.generation = domain.generation;
            slot.alive = true;
            slot.acknowledged_generation.store(0);
        } else if (slot.generation != domain.generation) {
            slot.generation = domain.generation;
            slot.alive = true;
            slot.acknowledged_generation.store(0);
        }
        return slot;
    }

    fn pauseSlot(self: *const StopTheWorldCoordinator, domain: DomainHandle) ?*const PauseSlot {
        if (domain.index >= self.pause_slots.items.len) return null;
        const slot = &self.pause_slots.items[domain.index];
        if (!slot.alive or slot.generation != domain.generation) return null;
        return slot;
    }

    fn pauseSlotMut(self: *StopTheWorldCoordinator, domain: DomainHandle) ?*PauseSlot {
        if (domain.index >= self.pause_slots.items.len) return null;
        const slot = &self.pause_slots.items[domain.index];
        if (!slot.alive or slot.generation != domain.generation) return null;
        return slot;
    }

    fn emit(self: *StopTheWorldCoordinator, action: event_sink_mod.ControlAction, domain: ?DomainHandle) void {
        self.event_sink.emit(.{ .control = .{
            .action = action,
            .handler_fiber = if (domain) |handle| .{
                .index = handle.index,
                .generation = handle.generation,
            } else null,
        } });
    }
};

test "stw_coordinator: request, pause, and resume are explicit" {
    var coordinator = StopTheWorldCoordinator.init(std.testing.allocator, EventSink.noop());
    defer coordinator.deinit();

    const initiator = DomainHandle{ .index = 0, .generation = 1 };
    const other = DomainHandle{ .index = 1, .generation = 1 };
    try coordinator.registerDomain(initiator);
    try coordinator.registerDomain(other);

    const generation = try coordinator.request(initiator, 2);
    try std.testing.expectEqual(@as(usize, 1), generation);
    try std.testing.expect(coordinator.isActive());
    try std.testing.expect(coordinator.shouldPause(0));
    try std.testing.expect(!coordinator.shouldPause(generation));

    try std.testing.expect(try coordinator.acknowledgePause(initiator, generation));
    try std.testing.expect(try coordinator.acknowledgePause(other, generation));
    try std.testing.expectEqual(@as(usize, 2), coordinator.pausedCount());
    try std.testing.expect(coordinator.allPaused());
    try coordinator.verify();

    coordinator.resumeWorld();
    try std.testing.expect(!coordinator.isActive());
    try std.testing.expectEqual(@as(usize, 0), coordinator.pausedCount());
}

test "stw_coordinator: coordination snapshots mirror request lifecycle" {
    var coordinator = StopTheWorldCoordinator.init(std.testing.allocator, EventSink.noop());
    defer coordinator.deinit();

    const initiator = DomainHandle{ .index = 3, .generation = 9 };
    try coordinator.registerDomain(initiator);

    var snapshot = coordinator.coordinationSnapshot();
    try std.testing.expect(!snapshot.active);
    try std.testing.expectEqual(@as(usize, 0), snapshot.generation);
    try std.testing.expectEqual(@as(?DomainHandle, null), snapshot.initiator);

    _ = try coordinator.request(initiator, 3);
    snapshot = coordinator.coordinationSnapshot();
    try std.testing.expect(snapshot.active);
    try std.testing.expectEqual(@as(usize, 1), snapshot.generation);
    try std.testing.expectEqual(@as(usize, 3), snapshot.target_pause_count);
    try std.testing.expectEqual(@as(usize, 0), snapshot.paused_count);
    try std.testing.expectEqual(initiator, snapshot.initiator.?);
    try std.testing.expect(snapshot.pause_epoch >= 1);

    try std.testing.expect(try coordinator.acknowledgePause(initiator, snapshot.generation));
    snapshot = coordinator.coordinationSnapshot();
    try std.testing.expectEqual(@as(usize, 1), snapshot.paused_count);

    coordinator.resumeWorld();
    snapshot = coordinator.coordinationSnapshot();
    try std.testing.expect(!snapshot.active);
    try std.testing.expectEqual(@as(usize, 0), snapshot.paused_count);
    try std.testing.expectEqual(@as(?DomainHandle, null), snapshot.initiator);
    try std.testing.expect(snapshot.resume_epoch >= 1);
}

test "stw_coordinator: parallel acknowledgements count each domain once" {
    var coordinator = StopTheWorldCoordinator.init(std.testing.allocator, EventSink.noop());
    defer coordinator.deinit();

    const domains = [_]DomainHandle{
        .{ .index = 0, .generation = 1 },
        .{ .index = 1, .generation = 1 },
    };
    for (domains) |domain| try coordinator.registerDomain(domain);

    const generation = try coordinator.request(domains[0], domains.len);

    const AckContext = struct {
        coordinator: *StopTheWorldCoordinator,
        domain: DomainHandle,
        generation: usize,
    };
    const Ack = struct {
        fn run(ctx: *AckContext) void {
            _ = ctx.coordinator.acknowledgePause(ctx.domain, ctx.generation) catch unreachable;
        }
    };

    var contexts = [_]AckContext{
        .{ .coordinator = &coordinator, .domain = domains[0], .generation = generation },
        .{ .coordinator = &coordinator, .domain = domains[1], .generation = generation },
    };
    var threads: [2]std.Thread = undefined;
    for (&threads, &contexts) |*thread, *ctx| {
        thread.* = try std.Thread.spawn(.{}, Ack.run, .{ctx});
    }
    for (threads) |thread| thread.join();

    try std.testing.expectEqual(@as(usize, 2), coordinator.pausedCount());
    try std.testing.expect(coordinator.allPaused());
}
