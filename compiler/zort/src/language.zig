const std = @import("std");
const event_sink = @import("event_sink.zig");
const heap_store = @import("heap_store.zig");
const mutator_mod = @import("mutator.zig");
const value = @import("value.zig");

pub const Error = mutator_mod.Error || error{
    InvalidFloatLiteral,
    BufferTooSmall,
};

pub const Value = value.Value;
pub const Object = heap_store.Object;
pub const ObjectKind = heap_store.ObjectKind;
pub const HeapStore = heap_store.HeapStore;
pub const Mutator = mutator_mod.Mutator;
pub const EventSink = event_sink.EventSink;
pub const RememberedSet = mutator_mod.RememberedSet;

pub const Language = struct {
    host_allocator: std.mem.Allocator,
    heap_store: *HeapStore,
    writer: Mutator,

    pub fn init(
        host_allocator: std.mem.Allocator,
        heap_allocator: std.mem.Allocator,
        heap: *HeapStore,
        sink: EventSink,
        remembered_set: ?*RememberedSet,
    ) Language {
        return .{
            .host_allocator = host_allocator,
            .heap_store = heap,
            .writer = Mutator.init(heap_allocator, heap, sink, remembered_set),
        };
    }

    pub fn allocTuple(self: *Language, len: usize) Error!Value {
        return self.writer.allocTuple(len);
    }

    pub fn tuple(self: *Language, fields: []const Value) Error!Value {
        const result = try self.writer.allocTuple(fields.len);
        if (fields.len == 0) return result;
        try self.writer.initTupleFromSlice(result, fields);
        return result;
    }

    pub fn tupleLength(self: *Language, block_value: Value) Error!usize {
        const obj = try self.objectFrom(block_value);
        const fields = obj.tupleFields() orelse return Error.InvalidValue;
        return fields.len;
    }

    pub fn field(self: *Language, block_value: Value, idx: usize) Error!Value {
        const obj = try self.objectFrom(block_value);
        const fields = obj.tupleFields() orelse return Error.InvalidValue;
        if (idx >= fields.len) return Error.InvalidValue;
        return fields[idx];
    }

    pub fn setField(self: *Language, block_value: Value, idx: usize, next: Value) Error!void {
        try self.writer.writeField(block_value, idx, next);
    }

    pub fn allocString(self: *Language, bytes: []const u8) Error!Value {
        return self.allocStringWithInit(bytes.len, bytes);
    }

    pub fn allocStringWithFill(self: *Language, len: usize, fill: u8) Error!Value {
        const string = try self.writer.allocStringLen(len);
        try self.writer.fillString(string, fill);
        return string;
    }

    pub fn allocStringWithInit(self: *Language, len: usize, initial_bytes: []const u8) Error!Value {
        if (initial_bytes.len > len) return Error.OutOfMemory;
        const string = try self.writer.allocStringLen(len);
        try self.writer.initStringBytes(string, initial_bytes);
        return string;
    }

    pub fn allocBytes(self: *Language, bytes: []const u8) Error!Value {
        return self.allocString(bytes);
    }

    pub fn allocBytesWithFill(self: *Language, len: usize, fill: u8) Error!Value {
        return self.allocStringWithFill(len, fill);
    }

    pub fn allocBytesWithInit(self: *Language, len: usize, initial_bytes: []const u8) Error!Value {
        return self.allocStringWithInit(len, initial_bytes);
    }

    pub fn stringLength(self: *Language, block_value: Value) Error!usize {
        const obj = try self.objectFrom(block_value);
        const bytes = obj.stringSlice() orelse return Error.InvalidValue;
        return bytes.len;
    }

    pub fn bytesLength(self: *Language, block_value: Value) Error!usize {
        return self.stringLength(block_value);
    }

    pub fn stringSlice(self: *Language, block_value: Value) Error![]const u8 {
        const obj = try self.objectFrom(block_value);
        return obj.stringSlice() orelse Error.InvalidValue;
    }

    pub fn bytesSlice(self: *Language, block_value: Value) Error![]const u8 {
        return self.stringSlice(block_value);
    }

    pub fn setStringBytes(self: *Language, block_value: Value, bytes: []const u8) Error!void {
        try self.writer.writeStringBytes(block_value, bytes);
    }

    pub fn setBytes(self: *Language, block_value: Value, bytes: []const u8) Error!void {
        try self.setStringBytes(block_value, bytes);
    }

    pub fn isString(self: *Language, block_value: Value) bool {
        const obj = self.objectFrom(block_value) catch return false;
        return obj.kind() == .string;
    }

    pub fn isBytes(self: *Language, block_value: Value) bool {
        return self.isString(block_value);
    }

    pub fn allocI64(self: *Language, number: i64) Error!Value {
        return self.writer.allocBoxedI64(number);
    }

    pub fn allocInt64(self: *Language, number: i64) Error!Value {
        return self.allocI64(number);
    }

    pub fn allocInt32(self: *Language, number: i32) Error!Value {
        return self.allocI32(number);
    }

    pub fn allocI32(self: *Language, number: i32) Error!Value {
        return self.allocI64(@as(i64, number));
    }

    pub fn allocF64(self: *Language, number: f64) Error!Value {
        return self.writer.allocBoxedF64(number);
    }

    pub fn allocDouble(self: *Language, number: f64) Error!Value {
        return self.allocF64(number);
    }

    pub fn unboxI64(self: *Language, boxed_value: Value) Error!i64 {
        const obj = try self.objectFrom(boxed_value);
        return obj.boxedI64() orelse Error.InvalidValue;
    }

    pub fn unboxF64(self: *Language, boxed_value: Value) Error!f64 {
        const obj = try self.objectFrom(boxed_value);
        return obj.boxedF64() orelse Error.InvalidValue;
    }

    pub fn parseF64(self: *Language, literal: []const u8) Error!Value {
        const number = try self.parseFloatLiteral(literal);
        return self.allocF64(number);
    }

    pub fn formatF64(self: *Language, boxed_value: Value, buffer: []u8) Error![]const u8 {
        const number = try self.unboxF64(boxed_value);
        return self.formatFloatLiteral(number, buffer);
    }

    fn objectFrom(self: *Language, block_value: Value) Error!*Object {
        const handle = block_value.asHeapRef() orelse return Error.InvalidValue;
        return self.heap_store.get(handle) orelse Error.InvalidValue;
    }

    fn parseFloatLiteral(self: *Language, literal: []const u8) Error!f64 {
        if (literal.len == 0) return Error.InvalidFloatLiteral;

        if (std.mem.indexOfScalar(u8, literal, '_') == null) {
            return std.fmt.parseFloat(f64, literal) catch Error.InvalidFloatLiteral;
        }

        const stripped = try self.host_allocator.alloc(u8, literal.len);
        defer self.host_allocator.free(stripped);

        var out_len: usize = 0;
        for (literal) |byte| {
            if (byte == '_') continue;
            stripped[out_len] = byte;
            out_len += 1;
        }
        if (out_len == 0) return Error.InvalidFloatLiteral;

        return std.fmt.parseFloat(f64, stripped[0..out_len]) catch Error.InvalidFloatLiteral;
    }

    fn formatFloatLiteral(_: *Language, number: f64, buffer: []u8) Error![]const u8 {
        if (std.math.isNan(number)) {
            return std.fmt.bufPrint(buffer, "nan", .{}) catch Error.BufferTooSmall;
        }
        if (std.math.isInf(number)) {
            if (number < 0) {
                return std.fmt.bufPrint(buffer, "-inf", .{}) catch Error.BufferTooSmall;
            }
            return std.fmt.bufPrint(buffer, "inf", .{}) catch Error.BufferTooSmall;
        }
        return std.fmt.bufPrint(buffer, "{d}", .{number}) catch Error.BufferTooSmall;
    }
};

test "language: bytes alias string representation and length semantics" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var language = Language.init(std.testing.allocator, std.testing.allocator, &store, EventSink.noop(), null);

    const bytes = try language.allocBytes("abc");
    try std.testing.expect(language.isBytes(bytes));
    try std.testing.expect(language.isString(bytes));
    try std.testing.expectEqual(@as(usize, 3), try language.bytesLength(bytes));
    try std.testing.expectEqualSlices(u8, "abc", try language.bytesSlice(bytes));

    try language.setBytes(bytes, "xy");
    try std.testing.expectEqualSlices(u8, "xy\x00", try language.bytesSlice(bytes));
}

test "language: tuple and boxed accessors are semantic and typed" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var language = Language.init(std.testing.allocator, std.testing.allocator, &store, EventSink.noop(), null);

    const tuple = try language.tuple(&.{ Value.fromInt(1), Value.fromInt(2) });
    try std.testing.expectEqual(@as(usize, 2), try language.tupleLength(tuple));
    try std.testing.expectEqual(Value.fromInt(2), try language.field(tuple, 1));

    const int_box = try language.allocI64(1234);
    const float_box = try language.allocF64(12.375);
    try std.testing.expectEqual(@as(i64, 1234), try language.unboxI64(int_box));
    try std.testing.expectApproxEqRel(@as(f64, 12.375), try language.unboxF64(float_box), 1e-12);
    try std.testing.expectError(Error.InvalidValue, language.unboxI64(tuple));
    try std.testing.expectError(Error.InvalidValue, language.unboxF64(tuple));
}

test "language: float parse strips underscores and format is locale-stable" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    var language = Language.init(std.testing.allocator, std.testing.allocator, &store, EventSink.noop(), null);

    const parsed = try language.parseF64("1_23.5");
    try std.testing.expectApproxEqRel(@as(f64, 123.5), try language.unboxF64(parsed), 1e-12);
    try std.testing.expectError(Error.InvalidFloatLiteral, language.parseF64(""));
    try std.testing.expectError(Error.InvalidFloatLiteral, language.parseF64("12.5ms"));

    var buffer: [64]u8 = undefined;
    const ordinary = try language.allocF64(12.375);
    try std.testing.expectEqualSlices(u8, "12.375", try language.formatF64(ordinary, &buffer));

    const pos_inf = try language.allocF64(std.math.inf(f64));
    try std.testing.expectEqualSlices(u8, "inf", try language.formatF64(pos_inf, &buffer));

    const neg_inf = try language.allocF64(-std.math.inf(f64));
    try std.testing.expectEqualSlices(u8, "-inf", try language.formatF64(neg_inf, &buffer));
}
