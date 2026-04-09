const std = @import("std");
const builtin = @import("builtin");

const RawValue = usize;
const raw_unit: RawValue = 1;

const FakeDomainState = extern struct {
    pad0: [0x28]u8 = [_]u8{0} ** 0x28,
    stack_anchor: usize = 0,
};

extern fn caml_start_program(state: *FakeDomainState) callconv(.c) RawValue;

comptime {
    if (builtin.is_test) {
        _ = struct {
            pub export fn caml_program() callconv(.c) RawValue {
                return raw_unit;
            }
        };
    }
}

var fake_domain = FakeDomainState{};

pub export var caml_globals_inited: usize = 0;
pub export var @"caml_system$frametable": [1]usize = .{0};
pub export var zort_last_emitted_int: i64 = -1;

pub export fn caml_startup(argv: ?*anyopaque) void {
    _ = argv;
    zort_last_emitted_int = -1;
    _ = caml_start_program(&fake_domain);
}

pub export fn caml_main(argv: ?*anyopaque) void {
    caml_startup(argv);
}

pub export fn caml_startup_exn(argv: ?*anyopaque) RawValue {
    _ = argv;
    zort_last_emitted_int = -1;
    return caml_start_program(&fake_domain);
}

pub export fn caml_startup_pooled(argv: ?*anyopaque) void {
    caml_startup(argv);
}

pub export fn caml_startup_pooled_exn(argv: ?*anyopaque) RawValue {
    return caml_startup_exn(argv);
}

pub export fn caml_shutdown() void {
    caml_globals_inited = 0;
}

fn decodeImmediateInt(raw: RawValue) i64 {
    const signed: isize = @bitCast(raw);
    return @as(i64, @intCast(signed >> 1));
}

pub export fn zort_emit_int(raw: RawValue) callconv(.c) RawValue {
    zort_last_emitted_int = decodeImmediateInt(raw);
    return raw_unit;
}

test "compiler compat: primitive decodes tagged ints" {
    zort_last_emitted_int = -1;
    try std.testing.expectEqual(raw_unit, zort_emit_int(0x55));
    try std.testing.expectEqual(@as(i64, 42), zort_last_emitted_int);
}
