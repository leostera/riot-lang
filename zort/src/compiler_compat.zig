const std = @import("std");
const builtin = @import("builtin");

const RawValue = usize;
const raw_unit: RawValue = 1;
const Intnat = isize;

const Segment = extern struct {
    begin: ?*const anyopaque = null,
    end: ?*const anyopaque = null,
};

const MetadataSummary = struct {
    frametable_count: usize = 0,
    frame_descriptor_count: usize = 0,
    gc_root_table_count: usize = 0,
    gc_root_entry_count: usize = 0,
    code_segment_count: usize = 0,
    data_segment_count: usize = 0,
};

const MetadataTables = struct {
    frametables: ?[*]const ?[*]const Intnat = null,
    globals: ?[*]const ?[*]const RawValue = null,
    code_segments: ?[*]const Segment = null,
    data_segments: ?[*]const Segment = null,

    fn isEmpty(self: MetadataTables) bool {
        return self.frametables == null and
            self.globals == null and
            self.code_segments == null and
            self.data_segments == null;
    }

    fn summarize(self: MetadataTables) MetadataSummary {
        const frametables = self.frametables orelse return .{};
        const globals = self.globals orelse return .{};
        const code_segments = self.code_segments orelse return .{};
        const data_segments = self.data_segments orelse return .{};
        return summarizeMetadata(frametables, globals, code_segments, data_segments);
    }

    fn captureExtern() MetadataTables {
        if (builtin.is_test) return .{};

        return .{
            .frametables = externSymbolPtr([*]const ?[*]const Intnat, "caml_frametable"),
            .globals = externSymbolPtr([*]const ?[*]const RawValue, "caml_globals"),
            .code_segments = externSymbolPtr([*]const Segment, "caml_code_segments"),
            .data_segments = externSymbolPtr([*]const Segment, "caml_data_segments"),
        };
    }
};

const RawRootSlotVisitor = struct {
    ctx: ?*anyopaque,
    visit_fn: *const fn (?*anyopaque, *const RawValue) void,

    fn visit(self: RawRootSlotVisitor, slot: *const RawValue) void {
        self.visit_fn(self.ctx, slot);
    }
};

const CompilerCompatState = struct {
    const StartupMetadata = struct {
        tables: MetadataTables,
        summary: MetadataSummary,
    };

    startup_metadata: ?StartupMetadata = null,

    fn reset(self: *CompilerCompatState) void {
        self.startup_metadata = null;
    }

    fn registerStartupMetadata(self: *CompilerCompatState, tables: MetadataTables) void {
        if (tables.isEmpty()) {
            self.reset();
            return;
        }

        self.startup_metadata = .{
            .tables = tables,
            .summary = tables.summarize(),
        };
    }

    fn hasStartupMetadata(self: *const CompilerCompatState) bool {
        return self.startup_metadata != null;
    }

    fn summary(self: *const CompilerCompatState) MetadataSummary {
        return if (self.startup_metadata) |metadata| metadata.summary else .{};
    }

    fn visitGcRootTableEntries(self: *const CompilerCompatState, visitor: RawRootSlotVisitor) void {
        const metadata = self.startup_metadata orelse return;
        const globals = metadata.tables.globals orelse return;

        var global_index: usize = 0;
        while (globals[global_index]) |table| : (global_index += 1) {
            var root_index: usize = 0;
            while (table[root_index] != 0) : (root_index += 1) {
                visitor.visit(&table[root_index]);
            }
        }
    }
};

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
var compiler_compat_state = CompilerCompatState{};

pub export var caml_globals_inited: usize = 0;
pub export var @"caml_system$frametable": [1]usize = .{0};
pub export var zort_last_emitted_int: i64 = -1;
pub export var zort_startup_calls: usize = 0;
pub export var zort_start_program_calls: usize = 0;
pub export var zort_last_start_program_result: RawValue = raw_unit;
pub export var zort_metadata_registered: usize = 0;
pub export var zort_metadata_frametables: usize = 0;
pub export var zort_metadata_frame_descriptors: usize = 0;
pub export var zort_metadata_gc_root_tables: usize = 0;
pub export var zort_metadata_gc_root_entries: usize = 0;
pub export var zort_metadata_code_segments: usize = 0;
pub export var zort_metadata_data_segments: usize = 0;

fn countSegments(segments: [*]const Segment) usize {
    var count: usize = 0;
    while (segments[count].begin != null) : (count += 1) {}
    return count;
}

fn summarizeMetadata(
    frametables: [*]const ?[*]const Intnat,
    globals: [*]const ?[*]const RawValue,
    code_segments: [*]const Segment,
    data_segments: [*]const Segment,
) MetadataSummary {
    var summary = MetadataSummary{};

    var frametable_index: usize = 0;
    while (frametables[frametable_index]) |table| : (frametable_index += 1) {
        summary.frametable_count += 1;
        summary.frame_descriptor_count += @as(usize, @intCast(table[0]));
    }

    var global_index: usize = 0;
    while (globals[global_index]) |table| : (global_index += 1) {
        summary.gc_root_table_count += 1;

        var root_index: usize = 0;
        while (table[root_index] != 0) : (root_index += 1) {
            summary.gc_root_entry_count += 1;
        }
    }

    summary.code_segment_count = countSegments(code_segments);
    summary.data_segment_count = countSegments(data_segments);
    return summary;
}

fn externSymbolPtr(comptime T: type, comptime name: []const u8) T {
    return @ptrCast(@alignCast(@extern(*const anyopaque, .{ .name = name })));
}

fn resetObservability() void {
    compiler_compat_state.reset();
    zort_last_emitted_int = -1;
    zort_startup_calls = 0;
    zort_start_program_calls = 0;
    zort_last_start_program_result = raw_unit;
    zort_metadata_registered = 0;
    zort_metadata_frametables = 0;
    zort_metadata_frame_descriptors = 0;
    zort_metadata_gc_root_tables = 0;
    zort_metadata_gc_root_entries = 0;
    zort_metadata_code_segments = 0;
    zort_metadata_data_segments = 0;
}

fn startupCommon() RawValue {
    resetObservability();
    zort_startup_calls = 1;

    compiler_compat_state.registerStartupMetadata(MetadataTables.captureExtern());
    zort_metadata_registered = @intFromBool(compiler_compat_state.hasStartupMetadata());

    const metadata = compiler_compat_state.summary();
    zort_metadata_frametables = metadata.frametable_count;
    zort_metadata_frame_descriptors = metadata.frame_descriptor_count;
    zort_metadata_gc_root_tables = metadata.gc_root_table_count;
    zort_metadata_gc_root_entries = metadata.gc_root_entry_count;
    zort_metadata_code_segments = metadata.code_segment_count;
    zort_metadata_data_segments = metadata.data_segment_count;

    const result = caml_start_program(&fake_domain);
    zort_start_program_calls = 1;
    zort_last_start_program_result = result;
    return result;
}

pub export fn caml_startup(argv: ?*anyopaque) void {
    _ = argv;
    _ = startupCommon();
}

pub export fn caml_main(argv: ?*anyopaque) void {
    caml_startup(argv);
}

pub export fn caml_startup_exn(argv: ?*anyopaque) RawValue {
    _ = argv;
    return startupCommon();
}

pub export fn caml_startup_pooled(argv: ?*anyopaque) void {
    caml_startup(argv);
}

pub export fn caml_startup_pooled_exn(argv: ?*anyopaque) RawValue {
    return caml_startup_exn(argv);
}

pub export fn caml_shutdown() void {
    resetObservability();
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

test "compiler compat: summarize startup metadata tables" {
    const frametable_a = [_]Intnat{ 2, 0, 0 };
    const frametable_b = [_]Intnat{ 1, 0 };
    const frametables = [_]?[*]const Intnat{
        frametable_a[0..].ptr,
        frametable_b[0..].ptr,
        null,
    };

    const roots_a = [_]RawValue{ 0x10, 0x20, 0 };
    const roots_b = [_]RawValue{0};
    const globals = [_]?[*]const RawValue{
        roots_a[0..].ptr,
        roots_b[0..].ptr,
        null,
    };

    const code_segments = [_]Segment{
        .{ .begin = @ptrFromInt(0x10), .end = @ptrFromInt(0x20) },
        .{ .begin = @ptrFromInt(0x30), .end = @ptrFromInt(0x40) },
        .{},
    };
    const data_segments = [_]Segment{
        .{ .begin = @ptrFromInt(0x50), .end = @ptrFromInt(0x60) },
        .{},
    };

    const summary = summarizeMetadata(
        frametables[0..].ptr,
        globals[0..].ptr,
        code_segments[0..].ptr,
        data_segments[0..].ptr,
    );

    try std.testing.expectEqual(@as(usize, 2), summary.frametable_count);
    try std.testing.expectEqual(@as(usize, 3), summary.frame_descriptor_count);
    try std.testing.expectEqual(@as(usize, 2), summary.gc_root_table_count);
    try std.testing.expectEqual(@as(usize, 2), summary.gc_root_entry_count);
    try std.testing.expectEqual(@as(usize, 2), summary.code_segment_count);
    try std.testing.expectEqual(@as(usize, 1), summary.data_segment_count);
}

test "compiler compat: state retains startup metadata and visits gc root entries" {
    const frametable = [_]Intnat{1};
    const frametables = [_]?[*]const Intnat{
        frametable[0..].ptr,
        null,
    };
    const roots = [_]RawValue{ 0x10, 0x20, 0 };
    const globals = [_]?[*]const RawValue{
        roots[0..].ptr,
        null,
    };
    const code_segments = [_]Segment{
        .{ .begin = @ptrFromInt(0x10), .end = @ptrFromInt(0x20) },
        .{},
    };
    const data_segments = [_]Segment{
        .{ .begin = @ptrFromInt(0x30), .end = @ptrFromInt(0x40) },
        .{},
    };

    var state = CompilerCompatState{};
    state.registerStartupMetadata(.{
        .frametables = frametables[0..].ptr,
        .globals = globals[0..].ptr,
        .code_segments = code_segments[0..].ptr,
        .data_segments = data_segments[0..].ptr,
    });

    const summary = state.summary();
    try std.testing.expect(state.hasStartupMetadata());
    try std.testing.expectEqual(@as(usize, 1), summary.frametable_count);
    try std.testing.expectEqual(@as(usize, 1), summary.gc_root_table_count);
    try std.testing.expectEqual(@as(usize, 2), summary.gc_root_entry_count);

    var seen = std.ArrayListUnmanaged(RawValue){};
    defer seen.deinit(std.testing.allocator);

    const Collect = struct {
        fn visit(ctx: ?*anyopaque, slot: *const RawValue) void {
            const items: *std.ArrayListUnmanaged(RawValue) = @ptrCast(@alignCast(ctx.?));
            items.append(std.testing.allocator, slot.*) catch unreachable;
        }
    };

    state.visitGcRootTableEntries(.{
        .ctx = &seen,
        .visit_fn = Collect.visit,
    });

    try std.testing.expectEqual(@as(usize, 2), seen.items.len);
    try std.testing.expectEqual(@as(RawValue, 0x10), seen.items[0]);
    try std.testing.expectEqual(@as(RawValue, 0x20), seen.items[1]);
}

test "compiler compat: state reset clears registered startup metadata" {
    const frametable = [_]Intnat{1};
    const frametables = [_]?[*]const Intnat{
        frametable[0..].ptr,
        null,
    };
    const roots = [_]RawValue{ 0x10, 0 };
    const globals = [_]?[*]const RawValue{
        roots[0..].ptr,
        null,
    };
    const segments = [_]Segment{
        .{ .begin = @ptrFromInt(0x10), .end = @ptrFromInt(0x20) },
        .{},
    };

    var state = CompilerCompatState{};
    state.registerStartupMetadata(.{
        .frametables = frametables[0..].ptr,
        .globals = globals[0..].ptr,
        .code_segments = segments[0..].ptr,
        .data_segments = segments[0..].ptr,
    });
    try std.testing.expect(state.hasStartupMetadata());

    state.reset();

    try std.testing.expect(!state.hasStartupMetadata());
    try std.testing.expectEqual(MetadataSummary{}, state.summary());
}
