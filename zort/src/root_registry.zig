const std = @import("std");
const event_sink = @import("event_sink.zig");
const value = @import("value.zig");

pub const Value = value.Value;
pub const EventSink = event_sink.EventSink;

pub const Stats = struct {
    root_generation: usize,
    root_registrations: usize,
    root_unregistrations: usize,
};

pub const RootHandle = struct {
    registry: *RootRegistry,
    slot: *const Value,
    active: bool = true,

    pub fn deinit(self: *RootHandle) void {
        if (!self.active) return;
        self.registry.unregister(self.slot);
        self.active = false;
    }
};

pub const RootRegistry = struct {
    allocator: std.mem.Allocator,
    event_sink: EventSink,
    roots: std.ArrayListUnmanaged(*const Value) = .{},
    root_generation: usize = 0,
    root_registrations: usize = 0,
    root_unregistrations: usize = 0,

    pub fn init(allocator: std.mem.Allocator, sink: EventSink) RootRegistry {
        return .{
            .allocator = allocator,
            .event_sink = sink,
        };
    }

    pub fn deinit(self: *RootRegistry) void {
        self.roots.deinit(self.allocator);
    }

    pub fn stats(self: *const RootRegistry) Stats {
        return .{
            .root_generation = self.root_generation,
            .root_registrations = self.root_registrations,
            .root_unregistrations = self.root_unregistrations,
        };
    }

    pub fn items(self: *const RootRegistry) []const *const Value {
        return self.roots.items;
    }

    pub fn register(self: *RootRegistry, slot: *const Value) !void {
        try self.roots.append(self.allocator, slot);
        self.root_generation +%= 1;
        self.root_registrations +%= 1;
        self.event_sink.emit(.{ .root = .{
            .action = .register,
            .is_block = slot.*.isBlock(),
        } });
    }

    pub fn scoped(self: *RootRegistry, slot: *const Value) !RootHandle {
        try self.register(slot);
        return .{
            .registry = self,
            .slot = slot,
        };
    }

    pub fn unregister(self: *RootRegistry, slot: *const Value) void {
        var i: usize = 0;
        while (i < self.roots.items.len) {
            if (self.roots.items[i] == slot) {
                _ = self.roots.swapRemove(i);
                self.root_generation +%= 1;
                self.root_unregistrations +%= 1;
                self.event_sink.emit(.{ .root = .{
                    .action = .unregister,
                    .is_block = slot.*.isBlock(),
                } });
                return;
            }
            i += 1;
        }
    }

    pub fn verify(self: *const RootRegistry, context: anytype, comptime is_valid: fn (@TypeOf(context), Value) bool) void {
        for (self.roots.items) |slot| {
            const rooted = slot.*;
            if (rooted.isBlock() and !is_valid(context, rooted)) {
                @panic("zort: root points to non-runtime object");
            }
        }
    }
};

test "root_registry: counters track registration and unregister" {
    var registry = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer registry.deinit();

    var slot = Value.fromInt(1);
    try registry.register(&slot);
    try registry.register(&slot);
    registry.unregister(&slot);

    const stats = registry.stats();
    try std.testing.expectEqual(@as(usize, 3), stats.root_generation);
    try std.testing.expectEqual(@as(usize, 2), stats.root_registrations);
    try std.testing.expectEqual(@as(usize, 1), stats.root_unregistrations);
    try std.testing.expectEqual(@as(usize, 1), registry.items().len);
}

test "root_registry: scoped handle unregisters on deinit" {
    var registry = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer registry.deinit();

    var slot = Value.fromInt(7);
    var handle = try registry.scoped(&slot);
    try std.testing.expectEqual(@as(usize, 1), registry.items().len);

    handle.deinit();
    try std.testing.expectEqual(@as(usize, 0), registry.items().len);

    // idempotent
    handle.deinit();
    try std.testing.expectEqual(@as(usize, 0), registry.items().len);
}

fn rootIsValid(valid: []const Value, rooted: Value) bool {
    for (valid) |item| {
        if (std.meta.eql(item, rooted)) return true;
    }
    return false;
}

test "root_registry: verify consults external validity hook" {
    var registry = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer registry.deinit();

    var rooted = Value.fromHeapRef(.{ .index = 1, .generation = 2 });
    try registry.register(&rooted);

    const valid: []const Value = &[_]Value{rooted};
    registry.verify(valid, rootIsValid);
}

test "root_registry: emits registration events" {
    var recorder = event_sink.Recorder{};
    var registry = RootRegistry.init(std.testing.allocator, recorder.sink());
    defer registry.deinit();

    var rooted = Value.fromHeapRef(.{ .index = 1, .generation = 1 });
    try registry.register(&rooted);
    registry.unregister(&rooted);

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.root_registrations);
    try std.testing.expectEqual(@as(usize, 1), counters.root_unregistrations);
}
