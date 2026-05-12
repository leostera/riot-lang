const std = @import("std");
const event_sink = @import("event_sink.zig");
const runtime_mod = @import("runtime.zig");

pub const Runtime = runtime_mod.Runtime;
pub const Value = runtime_mod.Value;

pub const Error = runtime_mod.Error || error{
    PrimitiveNotFound,
    ArityMismatch,
    DuplicatePrimitive,
    UnhandledEffect,
};

pub const PrimitiveFn = *const fn (*Runtime, []const Value) Error!Value;

pub const Primitive = struct {
    name: []const u8,
    arity: usize,
    function: PrimitiveFn,
};

pub const PrimitiveRegistry = struct {
    allocator: std.mem.Allocator,
    entries: std.StringHashMapUnmanaged(Primitive) = .{},

    pub fn init(allocator: std.mem.Allocator) PrimitiveRegistry {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *PrimitiveRegistry) void {
        var it = self.entries.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.entries.deinit(self.allocator);
    }

    pub fn register(self: *PrimitiveRegistry, name: []const u8, arity: usize, function: PrimitiveFn) Error!void {
        if (self.entries.contains(name)) return Error.DuplicatePrimitive;
        const owned_name = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned_name);
        try self.entries.put(self.allocator, owned_name, .{
            .name = owned_name,
            .arity = arity,
            .function = function,
        });
    }

    pub fn call(self: *const PrimitiveRegistry, runtime: *Runtime, name: []const u8, args: []const Value) Error!Value {
        const primitive = self.entries.get(name) orelse return Error.PrimitiveNotFound;
        if (primitive.arity != args.len) return Error.ArityMismatch;
        return primitive.function(runtime, args);
    }

    /// Use this for external or compatibility-boundary dispatch.
    /// It enters the runtime's callback boundary so effects cannot
    /// implicitly search past the foreign caller.
    pub fn callWithBoundary(self: *const PrimitiveRegistry, runtime: *Runtime, name: []const u8, args: []const Value) Error!Value {
        const current = runtime.currentFiber();
        runtime.enterCallbackBoundary(current) catch unreachable;
        defer runtime.exitCallbackBoundary(current) catch unreachable;
        return self.call(runtime, name, args);
    }
};

fn primitiveAddI64(runtime: *Runtime, args: []const Value) Error!Value {
    return runtime.allocI64((try runtime.unboxI64(args[0])) + (try runtime.unboxI64(args[1])));
}

fn primitivePerformEffect(runtime: *Runtime, args: []const Value) Error!Value {
    _ = args;
    const performed = runtime.performEffectAt(7001, 77, Value.fromInt(1), &.{}) catch |err| switch (err) {
        error.UnhandledEffect => return Error.UnhandledEffect,
        else => unreachable,
    };
    return performed.handler.handle_effect;
}

test "primitive_registry: register and call typed primitive" {
    var registry = PrimitiveRegistry.init(std.testing.allocator);
    defer registry.deinit();
    try registry.register("zort.add_i64", 2, primitiveAddI64);

    var runtime = Runtime.init(std.testing.allocator);
    defer runtime.deinit();

    const left = try runtime.allocI64(20);
    const right = try runtime.allocI64(22);
    const result = try registry.call(&runtime, "zort.add_i64", &.{ left, right });
    try std.testing.expectEqual(@as(i64, 42), try runtime.unboxI64(result));
}

test "primitive_registry: arity and lookup errors are explicit" {
    var registry = PrimitiveRegistry.init(std.testing.allocator);
    defer registry.deinit();
    try registry.register("zort.add_i64", 2, primitiveAddI64);

    var runtime = Runtime.init(std.testing.allocator);
    defer runtime.deinit();

    const left = try runtime.allocI64(1);
    try std.testing.expectError(Error.ArityMismatch, registry.call(&runtime, "zort.add_i64", &.{left}));
    try std.testing.expectError(Error.PrimitiveNotFound, registry.call(&runtime, "zort.unknown", &.{ left, left }));
}

test "primitive_registry: callback-boundary dispatch isolates effects from ambient handlers" {
    var trace = event_sink.TraceRecorder.init(std.testing.allocator, .{
        .capture_events = true,
    });
    defer trace.deinit();

    var registry = PrimitiveRegistry.init(std.testing.allocator);
    defer registry.deinit();
    try registry.register("zort.perform_effect", 0, primitivePerformEffect);

    var runtime = Runtime.initWithConfig(std.testing.allocator, .{
        .eventSink = trace.sink(),
    });
    defer runtime.deinit();

    const main = runtime.currentFiber();
    try runtime.pushEffectHandler(main, .{
        .effect = 77,
        .handle_effect = Value.fromInt(9),
    });

    const child = try runtime.spawnFiberInDomain(main, runtime.currentDomain());
    try runtime.activateFiberInDomain(runtime.currentDomain(), child);

    const internal_result = try registry.call(&runtime, "zort.perform_effect", &.{});
    try std.testing.expectEqual(@as(i64, 9), internal_result.asInt());

    try runtime.activateFiberInDomain(runtime.currentDomain(), child);
    try std.testing.expectError(error.UnhandledEffect, registry.callWithBoundary(&runtime, "zort.perform_effect", &.{}));

    var callback_entries: usize = 0;
    for (trace.traceEntries()) |entry| {
        if (entry.event != .control) continue;
        if (entry.event.control.action == .callback_enter or entry.event.control.action == .callback_exit) {
            callback_entries += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), callback_entries);
}
