const std = @import("std");
const domain_registry_mod = @import("domain_registry.zig");
const event_sink_mod = @import("event_sink.zig");

pub const DomainHandle = domain_registry_mod.DomainHandle;
pub const EventSink = event_sink_mod.EventSink;

pub const StopTheWorldCoordinator = struct {
    allocator: std.mem.Allocator,
    event_sink: EventSink,
    active: bool = false,
    generation: usize = 0,
    initiator: ?DomainHandle = null,
    paused_domains: std.ArrayListUnmanaged(DomainHandle) = .{},

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

    pub fn request(self: *StopTheWorldCoordinator, initiator: ?DomainHandle) !usize {
        self.generation +%= 1;
        self.active = true;
        self.initiator = initiator;
        self.paused_domains.clearRetainingCapacity();
        self.emit(.stw_request, initiator);
        return self.generation;
    }

    pub fn markPaused(self: *StopTheWorldCoordinator, domain: DomainHandle) !void {
        if (!self.active) return;
        if (self.isPaused(domain)) return;
        try self.paused_domains.append(self.allocator, domain);
        self.emit(.stw_pause, domain);
    }

    pub fn resumeWorld(self: *StopTheWorldCoordinator) void {
        if (!self.active) return;
        self.active = false;
        self.initiator = null;
        self.paused_domains.clearRetainingCapacity();
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
