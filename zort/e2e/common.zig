const std = @import("std");

pub const E2eError = error{ExpectationFailed};

pub fn expect(ok: bool, comptime message: []const u8) E2eError!void {
    if (ok) return;
    std.debug.print("zort e2e failed: {s}\n", .{message});
    return error.ExpectationFailed;
}

pub fn expectEqual(
    comptime T: type,
    actual: T,
    expected: T,
    comptime label: []const u8,
) E2eError!void {
    if (std.meta.eql(actual, expected)) return;
    std.debug.print(
        "zort e2e failed: {s}: expected={any} actual={any}\n",
        .{ label, expected, actual },
    );
    return error.ExpectationFailed;
}

pub fn expectBytesEqual(actual: []const u8, expected: []const u8, comptime label: []const u8) E2eError!void {
    if (std.mem.eql(u8, actual, expected)) return;
    std.debug.print(
        "zort e2e failed: {s}: expected=\"{s}\" actual=\"{s}\"\n",
        .{ label, expected, actual },
    );
    return error.ExpectationFailed;
}

pub fn sameHandle(lhs: anytype, rhs: anytype) bool {
    return lhs.index == rhs.index and lhs.generation == rhs.generation;
}
