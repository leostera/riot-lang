const std = @import("std");

pub const Tag = enum(u8) {
    tuple = 0,
    string = 252,
    double = 253,
    int64 = 254,
    custom = 255,
};

pub const HeapRef = struct {
    index: u32,
    generation: u32,
};

pub const Atom = enum(u8) {
    false = 0,
    true = 1,
};

pub const Immediate = union(enum) {
    int: i64,
    atom: Atom,
};

pub const Value = union(enum) {
    immediate: Immediate,
    block: HeapRef,

    pub fn isImmediate(self: Value) bool {
        return switch (self) {
            .immediate => true,
            .block => false,
        };
    }

    pub fn isBlock(self: Value) bool {
        return !self.isImmediate();
    }

    pub fn fromInt(v: i64) Value {
        return .{ .immediate = .{ .int = v } };
    }

    pub fn fromHeapRef(ref: HeapRef) Value {
        return .{ .block = ref };
    }

    pub fn fromAtom(atom: Atom) Value {
        return .{ .immediate = .{ .atom = atom } };
    }

    pub fn asInt(self: Value) i64 {
        return switch (self) {
            .immediate => |imm| switch (imm) {
                .int => |value| value,
                .atom => unreachable,
            },
            .block => unreachable,
        };
    }

    pub fn asHeapRef(self: Value) ?HeapRef {
        return switch (self) {
            .block => |block| block,
            else => null,
        };
    }

    pub fn toFingerprint(self: Value) usize {
        return switch (self) {
            .immediate => |imm| switch (imm) {
                .int => |i| @as(usize, @bitCast(@as(u64, @bitCast(i)))),
                .atom => |atom| 0x8000_0000 | @as(usize, @intFromEnum(atom)),
            },
            .block => |ref| (0x4000_0000 | (@as(usize, ref.index) << 32)) ^ @as(usize, ref.generation),
        };
    }
};

pub const False: Value = Value.fromAtom(.false);
pub const True: Value = Value.fromAtom(.true);
pub const Unit: Value = False;

test "value: immediates remain encoded" {
    const one = Value.fromInt(1);
    const zero = Value.fromInt(0);
    const forty_two = Value.fromInt(42);
    try std.testing.expect(one.isImmediate());
    try std.testing.expect(zero.isImmediate());
    try std.testing.expect(forty_two.isImmediate());
}

test "value: int conversion round-trip" {
    const neg = Value.fromInt(-17);
    const pos = Value.fromInt(17);
    const neg_round = neg.asInt();
    const pos_round = pos.asInt();
    try std.testing.expectEqual(@as(i64, -17), neg_round);
    try std.testing.expectEqual(@as(i64, 17), pos_round);
}

test "value: block refs are explicit handles" {
    const ref = Value.fromHeapRef(.{ .index = 1, .generation = 2 });
    const got = ref.asHeapRef().?;
    try std.testing.expectEqual(@as(u32, 1), got.index);
    try std.testing.expectEqual(@as(u32, 2), got.generation);
    try std.testing.expect(ref.isBlock());
}

test "value: bool constants are immediate immediates" {
    try std.testing.expect(True.isImmediate());
    try std.testing.expect(False.isImmediate());
    try std.testing.expect(Unit.toFingerprint() == False.toFingerprint());
    try std.testing.expect(True.toFingerprint() != False.toFingerprint());
}

test "value: large int payloads round-trip" {
    const big_pos = Value.fromInt(@as(i64, 0x3FFF_ffff_ffff_f00f));
    const big_neg = Value.fromInt(@as(i64, -0x3FFF_ffff_ffff_f00f));
    try std.testing.expectEqual(@as(i64, 0x3FFF_ffff_ffff_f00f), big_pos.asInt());
    try std.testing.expectEqual(@as(i64, -0x3FFF_ffff_ffff_f00f), big_neg.asInt());
}
