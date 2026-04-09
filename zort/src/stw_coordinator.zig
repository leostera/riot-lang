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
    paused_count: usize,
    pause_epoch: usize,
    resume_epoch: usize,
    initiator: ?DomainHandle,
};

const CoordinationState = struct {
    active: AtomicFlag = AtomicFlag.init(false),
    generation: AtomicCounter = AtomicCounter.init(0),
    paused_count: AtomicCounter = AtomicCounter.init(0),
    pause_epoch: AtomicCounter = AtomicCounter.init(0),
    resume_epoch: AtomicCounter = AtomicCounter.init(0),
    initiator: OptionalDomainCell = OptionalDomainCell.init(null),

    fn beginRequest(self: *CoordinationState, generation: usize, initiator: ?DomainHandle) void {
        self.generation.store(generation);
        self.paused_count.store(0);
        self.initiator.store(initiator);
        self.active.store(true);
        _ = self.pause_epoch.increment();
    }

    fn notePause(self: *CoordinationState, paused_count: usize) void {
        self.paused_count.store(paused_count);
        _ = self.pause_epoch.increment();
    }

    fn noteResume(self: *CoordinationState) void {
        self.paused_count.store(0);
        self.initiator.store(null);
        self.active.store(false);
        _ = self.resume_epoch.increment();
    }

    fn snapshot(self: *const CoordinationState) CoordinationSnapshot {
        return .{
            .active = self.active.isSet(),
            .generation = self.generation.load(),
            .paused_count = self.paused_count.load(),
            .pause_epoch = self.pause_epoch.load(),
            .resume_epoch = self.resume_epoch.load(),
            .initiator = self.initiator.load(),
        };
    }
};

pub const StopTheWorldCoordinator = struct {
    allocator: std.mem.Allocator,
    event_sink: EventSink,
    active: bool = false,
    generation: usize = 0,
    initiator: ?DomainHandle = null,
    paused_domains: std.ArrayListUnmanaged(DomainHandle) = .{},
    coordination: CoordinationState = .{},

    pub const VerifyError = error{
        DuplicatePausedDomain,
        PausedWithoutActiveRequest,
    };

    pub fn init(allocator: std.mem.Allocator, sink: EventSink) StopTheWorldCoordinator {
        return .{
            .allocator = allocator,
            .event_sink = sink,
        };
    }

    pub fn deinit(self: *StopTheWorldCoordinator) void {
        self.paused_domains.deinit(self.allocator);
    }

    pub fn coordinationSnapshot(self: *const StopTheWorldCoordinator) CoordinationSnapshot {
        return self.coordination.snapshot();
    }

    pub fn request(self: *StopTheWorldCoordinator, initiator: ?DomainHandle) !usize {
        self.generation +%= 1;
        self.active = true;
        self.initiator = initiator;
        self.paused_domains.clearRetainingCapacity();
        self.coordination.beginRequest(self.generation, initiator);
        self.emit(.stw_request, initiator);
        return self.generation;
    }

    pub fn markPaused(self: *StopTheWorldCoordinator, domain: DomainHandle) !void {
        if (!self.active) return;
        if (self.isPaused(domain)) return;
        try self.paused_domains.append(self.allocator, domain);
        self.coordination.notePause(self.paused_domains.items.len);
        self.emit(.stw_pause, domain);
    }

    pub fn resumeWorld(self: *StopTheWorldCoordinator) void {
        if (!self.active) return;
        self.active = false;
        self.initiator = null;
        self.paused_domains.clearRetainingCapacity();
        self.coordination.noteResume();
        self.emit(.stw_resume, null);
    }

    pub fn isPaused(self: *const StopTheWorldCoordinator, domain: DomainHandle) bool {
        for (self.paused_domains.items) |paused| {
            if (sameDomain(paused, domain)) return true;
        }
        return false;
    }

    pub fn pausedCount(self: *const StopTheWorldCoordinator) usize {
        return self.paused_domains.items.len;
    }

    pub fn isActive(self: *const StopTheWorldCoordinator) bool {
        return self.active;
    }

    pub fn verify(self: *const StopTheWorldCoordinator) VerifyError!void {
        if (!self.active and self.paused_domains.items.len != 0) return error.PausedWithoutActiveRequest;
        for (self.paused_domains.items, 0..) |domain, index| {
            for (self.paused_domains.items[index + 1 ..]) |other| {
                if (sameDomain(domain, other)) return error.DuplicatePausedDomain;
            }
        }
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

fn sameDomain(lhs: DomainHandle, rhs: DomainHandle) bool {
    return lhs.index == rhs.index and lhs.generation == rhs.generation;
}

test "stw_coordinator: request, pause, and resume are explicit" {
    var coordinator = StopTheWorldCoordinator.init(std.testing.allocator, EventSink.noop());
    defer coordinator.deinit();

    const initiator = DomainHandle{ .index = 0, .generation = 1 };
    const generation = try coordinator.request(initiator);
    try std.testing.expectEqual(@as(usize, 1), generation);
    try std.testing.expect(coordinator.isActive());

    try coordinator.markPaused(initiator);
    try coordinator.markPaused(.{ .index = 1, .generation = 1 });
    try std.testing.expectEqual(@as(usize, 2), coordinator.pausedCount());
    try coordinator.verify();

    coordinator.resumeWorld();
    try std.testing.expect(!coordinator.isActive());
    try std.testing.expectEqual(@as(usize, 0), coordinator.pausedCount());
}

test "stw_coordinator: coordination snapshots mirror request lifecycle" {
    var coordinator = StopTheWorldCoordinator.init(std.testing.allocator, EventSink.noop());
    defer coordinator.deinit();

    var snapshot = coordinator.coordinationSnapshot();
    try std.testing.expect(!snapshot.active);
    try std.testing.expectEqual(@as(usize, 0), snapshot.generation);
    try std.testing.expectEqual(@as(?DomainHandle, null), snapshot.initiator);

    const initiator = DomainHandle{ .index = 3, .generation = 9 };
    _ = try coordinator.request(initiator);
    snapshot = coordinator.coordinationSnapshot();
    try std.testing.expect(snapshot.active);
    try std.testing.expectEqual(@as(usize, 1), snapshot.generation);
    try std.testing.expectEqual(@as(usize, 0), snapshot.paused_count);
    try std.testing.expectEqual(initiator, snapshot.initiator.?);
    try std.testing.expect(snapshot.pause_epoch >= 1);

    try coordinator.markPaused(initiator);
    snapshot = coordinator.coordinationSnapshot();
    try std.testing.expectEqual(@as(usize, 1), snapshot.paused_count);

    coordinator.resumeWorld();
    snapshot = coordinator.coordinationSnapshot();
    try std.testing.expect(!snapshot.active);
    try std.testing.expectEqual(@as(usize, 0), snapshot.paused_count);
    try std.testing.expectEqual(@as(?DomainHandle, null), snapshot.initiator);
    try std.testing.expect(snapshot.resume_epoch >= 1);
}
