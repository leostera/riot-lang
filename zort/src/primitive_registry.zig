const std = @import("std");
const runtime_mod = @import("runtime.zig");

pub const Runtime = runtime_mod.Runtime;
pub const Value = runtime_mod.Value;

pub const Error = runtime_mod.Error || error{
    PrimitiveNotFound,
    ArityMismatch,
    DuplicatePrimitive,
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
};

fn primitiveAddI64(runtime: *Runtime, args: []const Value) Error!Value {
    return runtime.allocI64((try runtime.unboxI64(args[0])) + (try runtime.unboxI64(args[1])));
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
