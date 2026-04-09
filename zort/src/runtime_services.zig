const std = @import("std");
const root_provider = @import("root_provider.zig");
const value = @import("value.zig");

pub const RootProvider = root_provider.RootProvider;
pub const RootVisitor = root_provider.RootVisitor;
pub const Value = value.Value;

pub const RuntimeServices = struct {
    allocator: std.mem.Allocator,
    startup_depth: usize = 0,
    was_shutdown: bool = false,
    pending_signals: u64 = 0,
    blocking_sections: usize = 0,
    named_values: std.ArrayListUnmanaged(NamedValue) = .{},
    signal_handlers: [64]?Value = [_]?Value{null} ** 64,

    pub const Error = error{
        RuntimeAlreadyShutdown,
        ShutdownWithoutStartup,
        BlockingSectionUnderflow,
        UnsupportedSignal,
        OutOfMemory,
    };

    pub const NamedValue = struct {
        name: []u8,
        value: Value,
    };

    pub fn init(allocator: std.mem.Allocator) RuntimeServices {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RuntimeServices) void {
        for (self.named_values.items) |entry| self.allocator.free(entry.name);
        self.named_values.deinit(self.allocator);
    }

    pub fn provider(self: *RuntimeServices) RootProvider {
        return .{
            .name = "runtime_services",
            .ctx = self,
            .count_fn = countRoots,
            .visit_fn = visitRoots,
        };
    }

    pub fn ownerCount(self: *const RuntimeServices, needle: Value) usize {
        var count: usize = 0;
        for (self.named_values.items) |entry| {
            if (entry.value.isBlock() and std.meta.eql(entry.value, needle)) count += 1;
        }
        for (self.signal_handlers) |handler| {
            if (handler) |value_ref| {
                if (value_ref.isBlock() and std.meta.eql(value_ref, needle)) count += 1;
            }
        }
        return count;
    }

    pub fn startup(self: *RuntimeServices) Error!void {
        if (self.was_shutdown and self.startup_depth == 0) return error.RuntimeAlreadyShutdown;
        self.startup_depth +%= 1;
    }

    pub fn shutdown(self: *RuntimeServices) Error!void {
        if (self.startup_depth == 0) return error.ShutdownWithoutStartup;
        self.startup_depth -= 1;
        if (self.startup_depth == 0) self.was_shutdown = true;
    }

    pub fn isStarted(self: *const RuntimeServices) bool {
        return self.startup_depth > 0;
    }

    pub fn enterBlockingSection(self: *RuntimeServices) void {
        self.blocking_sections +%= 1;
    }

    pub fn exitBlockingSection(self: *RuntimeServices) Error!void {
        if (self.blocking_sections == 0) return error.BlockingSectionUnderflow;
        self.blocking_sections -= 1;
    }

    pub fn blockingDepth(self: *const RuntimeServices) usize {
        return self.blocking_sections;
    }

    pub fn recordSignal(self: *RuntimeServices, signo: u8) Error!void {
        if (signo >= 64) return error.UnsupportedSignal;
        self.pending_signals |= (@as(u64, 1) << @intCast(signo));
    }

    pub fn takePendingSignals(self: *RuntimeServices) u64 {
        const pending = self.pending_signals;
        self.pending_signals = 0;
        return pending;
    }

    pub fn registerSignalHandler(self: *RuntimeServices, signo: u8, handler: Value) Error!void {
        if (signo >= self.signal_handlers.len) return error.UnsupportedSignal;
        self.signal_handlers[signo] = handler;
    }

    pub fn lookupSignalHandler(self: *const RuntimeServices, signo: u8) ?Value {
        if (signo >= self.signal_handlers.len) return null;
        return self.signal_handlers[signo];
    }

    pub fn registerNamedValue(self: *RuntimeServices, name: []const u8, val: Value) Error!void {
        for (self.named_values.items) |*entry| {
            if (std.mem.eql(u8, entry.name, name)) {
                entry.value = val;
                return;
            }
        }

        try self.named_values.append(self.allocator, .{
            .name = try self.allocator.dupe(u8, name),
            .value = val,
        });
    }

    pub fn lookupNamedValue(self: *const RuntimeServices, name: []const u8) ?Value {
        for (self.named_values.items) |entry| {
            if (std.mem.eql(u8, entry.name, name)) return entry.value;
        }
        return null;
    }

    fn countRoots(ctx: ?*anyopaque) usize {
        const self: *RuntimeServices = @ptrCast(@alignCast(ctx.?));
        var count: usize = 0;
        for (self.named_values.items) |entry| {
            if (entry.value.isBlock()) count += 1;
        }
        for (self.signal_handlers) |handler| {
            if (handler) |rooted| {
                if (rooted.isBlock()) count += 1;
            }
        }
        return count;
    }

    fn visitRoots(ctx: ?*anyopaque, visitor: RootVisitor) void {
        const self: *RuntimeServices = @ptrCast(@alignCast(ctx.?));
        for (self.named_values.items) |entry| {
            if (entry.value.isBlock()) visitor.visit(entry.value);
        }
        for (self.signal_handlers) |handler| {
            if (handler) |rooted| {
                if (rooted.isBlock()) visitor.visit(rooted);
            }
        }
    }
};

test "runtime_services: startup and shutdown are reference-counted" {
    var services = RuntimeServices.init(std.testing.allocator);
    defer services.deinit();

    try services.startup();
    try services.startup();
    try std.testing.expect(services.isStarted());
    try services.shutdown();
    try std.testing.expect(services.isStarted());
    try services.shutdown();
    try std.testing.expect(!services.isStarted());
    try std.testing.expectError(RuntimeServices.Error.RuntimeAlreadyShutdown, services.startup());
}

test "runtime_services: named values are rooted through provider" {
    var services = RuntimeServices.init(std.testing.allocator);
    defer services.deinit();

    try services.registerNamedValue("unit", Value.fromInt(0));
    try services.registerNamedValue("tuple", Value.fromHeapRef(.{ .index = 3, .generation = 1 }));

    var seen = std.ArrayListUnmanaged(Value){};
    defer seen.deinit(std.testing.allocator);

    const Collect = struct {
        fn visit(ctx: ?*anyopaque, rooted: Value) void {
            const items: *std.ArrayListUnmanaged(Value) = @ptrCast(@alignCast(ctx.?));
            items.append(std.testing.allocator, rooted) catch unreachable;
        }
    };

    const provider = services.provider();
    try std.testing.expectEqual(@as(usize, 1), provider.count());
    provider.visit(.{
        .ctx = &seen,
        .visit_fn = Collect.visit,
    });
    try std.testing.expectEqual(@as(usize, 1), seen.items.len);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 3, .generation = 1 }), seen.items[0]);
}

test "runtime_services: pending signals and blocking sections are explicit" {
    var services = RuntimeServices.init(std.testing.allocator);
    defer services.deinit();

    services.enterBlockingSection();
    try std.testing.expectEqual(@as(usize, 1), services.blockingDepth());
    try services.recordSignal(2);
    try services.recordSignal(5);
    try std.testing.expectEqual((@as(u64, 1) << 2) | (@as(u64, 1) << 5), services.takePendingSignals());
    try std.testing.expectEqual(@as(u64, 0), services.takePendingSignals());
    try services.exitBlockingSection();
    try std.testing.expectEqual(@as(usize, 0), services.blockingDepth());
}

test "runtime_services: signal handlers are rooted runtime state" {
    var services = RuntimeServices.init(std.testing.allocator);
    defer services.deinit();

    try services.registerSignalHandler(2, Value.fromHeapRef(.{ .index = 8, .generation = 1 }));
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 8, .generation = 1 }), services.lookupSignalHandler(2).?);
    try std.testing.expectEqual(@as(usize, 1), services.provider().count());
}
