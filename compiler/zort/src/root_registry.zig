const std = @import("std");
const event_sink = @import("event_sink.zig");
const root_provider = @import("root_provider.zig");
const value = @import("value.zig");

pub const Value = value.Value;
pub const EventSink = event_sink.EventSink;
pub const RootProvider = root_provider.RootProvider;
pub const RootVisitor = root_provider.RootVisitor;

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

const RootFrameSlot = struct {
    value: Value,
};

pub const RootBinding = struct {
    slot: *RootFrameSlot,

    pub fn get(self: *const RootBinding) Value {
        return self.slot.value;
    }

    pub fn set(self: *RootBinding, next: Value) void {
        self.slot.value = next;
    }

    pub fn ptr(self: *RootBinding) *Value {
        return &self.slot.value;
    }
};

pub const RootFrame = struct {
    allocator: std.mem.Allocator,
    registry: *RootRegistry,
    slots: std.ArrayListUnmanaged(*RootFrameSlot) = .{},
    active: bool = true,

    pub const Error = error{
        FrameInactive,
    } || std.mem.Allocator.Error;

    pub fn bind(self: *RootFrame, rooted: Value) Error!RootBinding {
        if (!self.active) return error.FrameInactive;

        const slot = try self.allocator.create(RootFrameSlot);
        errdefer self.allocator.destroy(slot);
        slot.* = .{ .value = rooted };

        try self.registry.register(&slot.value);
        errdefer self.registry.unregister(&slot.value);

        try self.slots.append(self.allocator, slot);
        return .{ .slot = slot };
    }

    pub fn end(self: *RootFrame) void {
        if (!self.active) return;

        for (self.slots.items) |slot| {
            self.registry.unregister(&slot.value);
            self.allocator.destroy(slot);
        }
        self.slots.deinit(self.allocator);
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

    pub fn provider(self: *RootRegistry) RootProvider {
        return .{
            .name = "root_registry",
            .ctx = self,
            .count_fn = countRoots,
            .visit_fn = visitRoots,
        };
    }

    pub fn ownerCount(self: *const RootRegistry, needle: Value) usize {
        var count: usize = 0;
        for (self.roots.items) |slot| {
            if (std.meta.eql(slot.*, needle)) count += 1;
        }
        return count;
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

    pub fn beginFrame(self: *RootRegistry) RootFrame {
        return .{
            .allocator = self.allocator,
            .registry = self,
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

    fn countRoots(ctx: ?*anyopaque) usize {
        const self: *RootRegistry = @ptrCast(@alignCast(ctx.?));
        return self.roots.items.len;
    }

    fn visitRoots(ctx: ?*anyopaque, visitor: RootVisitor) void {
        const self: *RootRegistry = @ptrCast(@alignCast(ctx.?));
        for (self.roots.items) |slot| {
            visitor.visit(slot.*);
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

test "root_registry: lexical root frame owns stable rooted slots" {
    var registry = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer registry.deinit();

    var frame = registry.beginFrame();
    var left = try frame.bind(Value.fromInt(7));
    var right = try frame.bind(Value.fromHeapRef(.{ .index = 3, .generation = 1 }));

    try std.testing.expectEqual(@as(usize, 2), registry.items().len);
    try std.testing.expectEqual(Value.fromInt(7), left.get());
    right.set(Value.fromInt(9));
    try std.testing.expectEqual(Value.fromInt(9), right.get());

    frame.end();
    try std.testing.expectEqual(@as(usize, 0), registry.items().len);

    try std.testing.expectError(RootFrame.Error.FrameInactive, frame.bind(Value.fromInt(1)));
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

test "root_registry: provider enumerates rooted values" {
    var registry = RootRegistry.init(std.testing.allocator, EventSink.noop());
    defer registry.deinit();

    var left = Value.fromInt(7);
    var right = Value.fromHeapRef(.{ .index = 3, .generation = 9 });
    try registry.register(&left);
    try registry.register(&right);

    var seen = std.ArrayListUnmanaged(Value){};
    defer seen.deinit(std.testing.allocator);

    const Collect = struct {
        fn visit(ctx: ?*anyopaque, rooted: Value) void {
            const items: *std.ArrayListUnmanaged(Value) = @ptrCast(@alignCast(ctx.?));
            items.append(std.testing.allocator, rooted) catch unreachable;
        }
    };

    const provider = registry.provider();
    try std.testing.expectEqual(@as(usize, 2), provider.count());
    provider.visit(.{
        .ctx = &seen,
        .visit_fn = Collect.visit,
    });

    try std.testing.expectEqual(@as(usize, 2), seen.items.len);
    try std.testing.expectEqual(left, seen.items[0]);
    try std.testing.expectEqual(right, seen.items[1]);
}
