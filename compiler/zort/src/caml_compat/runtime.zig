const std = @import("std");
const builtin = @import("builtin");

const RawValue = usize;
const raw_unit: RawValue = 1;
const Intnat = isize;
const raw_immediate_mask: RawValue = 1;
const raw_header_tag_bits: usize = 8;
const raw_header_color_bits: usize = 2;
const raw_header_color_shift: usize = raw_header_tag_bits;
const raw_header_wosize_shift: usize = raw_header_tag_bits + raw_header_color_bits;
const raw_header_tag_mask: RawValue = (@as(RawValue, 1) << raw_header_tag_bits) - 1;
const raw_no_scan_tag: u8 = 251;

const Segment = extern struct {
    begin: ?*const anyopaque = null,
    end: ?*const anyopaque = null,
};

const MetadataSummary = struct {
    frametable_count: usize = 0,
    frame_descriptor_count: usize = 0,
    gc_root_table_count: usize = 0,
    gc_root_entry_count: usize = 0,
    gc_root_block_count: usize = 0,
    gc_root_block_field_count: usize = 0,
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

const RegisteredFrameTable = struct {
    table: [*]const Intnat,
    descriptor_count: usize,
};

const RegisteredGcRootTable = struct {
    table: [*]const RawValue,
    entry_count: usize,
    block_field_count: usize,

    fn visitBlockFields(self: RegisteredGcRootTable, visitor: RawRootSlotVisitor) void {
        var root_index: usize = 0;
        while (self.table[root_index] != 0) : (root_index += 1) {
            visitRawBlockFieldRoots(self.table[root_index], visitor);
        }
    }

    fn blockFieldSlotAt(self: RegisteredGcRootTable, slot_index: usize) ?*RawValue {
        var remaining = slot_index;
        var root_index: usize = 0;
        while (self.table[root_index] != 0) : (root_index += 1) {
            const raw = self.table[root_index];
            if (!rawBlockHasScannableFields(raw)) continue;

            const field_count = rawBlockWordSize(raw);
            if (remaining < field_count) {
                return &rawBlockFieldBase(raw)[remaining];
            }
            remaining -= field_count;
        }

        return null;
    }
};

const RegisteredDataSegment = struct {
    begin: *const anyopaque,
    end: *const anyopaque,
};

const RegisteredCodeFragment = struct {
    const DigestPolicy = enum {
        later,
    };

    begin: *const anyopaque,
    end: *const anyopaque,
    digest_policy: DigestPolicy = .later,

    fn contains(self: RegisteredCodeFragment, pc: *const anyopaque) bool {
        const pc_addr = @intFromPtr(pc);
        return pc_addr >= @intFromPtr(self.begin) and pc_addr < @intFromPtr(self.end);
    }
};

const RawRootSlotVisitor = struct {
    ctx: ?*anyopaque,
    visit_fn: *const fn (?*anyopaque, *RawValue) void,

    fn visit(self: RawRootSlotVisitor, slot: *RawValue) void {
        self.visit_fn(self.ctx, slot);
    }
};

const GcRootTableSummary = struct {
    entry_count: usize = 0,
    block_count: usize = 0,
    block_field_count: usize = 0,
};

// Raw OCaml block decoding is intentionally trapped inside the locked compiler
// compatibility layer. The semantic kernel must not learn this ABI layout.
fn rawValueIsBlock(raw: RawValue) bool {
    return raw != 0 and (raw & raw_immediate_mask) == 0;
}

fn rawBlockFieldBase(raw: RawValue) [*]RawValue {
    return @ptrFromInt(raw);
}

fn rawBlockHeader(raw: RawValue) RawValue {
    const fields = rawBlockFieldBase(raw);
    return (fields - 1)[0];
}

fn rawBlockTag(raw: RawValue) u8 {
    return @as(u8, @truncate(rawBlockHeader(raw) & raw_header_tag_mask));
}

fn rawBlockWordSize(raw: RawValue) usize {
    return @as(usize, @intCast(rawBlockHeader(raw) >> raw_header_wosize_shift));
}

fn rawBlockHasScannableFields(raw: RawValue) bool {
    return rawValueIsBlock(raw) and rawBlockTag(raw) < raw_no_scan_tag;
}

fn visitRawBlockFieldRoots(raw: RawValue, visitor: RawRootSlotVisitor) void {
    if (!rawBlockHasScannableFields(raw)) return;

    const fields = rawBlockFieldBase(raw);
    var field_index: usize = 0;
    while (field_index < rawBlockWordSize(raw)) : (field_index += 1) {
        visitor.visit(&fields[field_index]);
    }
}

fn summarizeGcRootTable(table: [*]const RawValue) GcRootTableSummary {
    var summary = GcRootTableSummary{};

    while (table[summary.entry_count] != 0) : (summary.entry_count += 1) {
        const raw = table[summary.entry_count];
        if (!rawBlockHasScannableFields(raw)) continue;

        summary.block_count += 1;
        summary.block_field_count += rawBlockWordSize(raw);
    }

    return summary;
}

const CamlCompatState = struct {
    const LifecycleError = error{
        StartupAfterShutdown,
        ShutdownWithoutStartup,
    };

    const StartupMetadata = struct {
        frame_tables: std.ArrayListUnmanaged(RegisteredFrameTable) = .{},
        gc_root_tables: std.ArrayListUnmanaged(RegisteredGcRootTable) = .{},
        code_fragments: std.ArrayListUnmanaged(RegisteredCodeFragment) = .{},
        data_segments: std.ArrayListUnmanaged(RegisteredDataSegment) = .{},
        summary: MetadataSummary,

        fn deinit(self: *StartupMetadata, allocator: std.mem.Allocator) void {
            self.frame_tables.deinit(allocator);
            self.gc_root_tables.deinit(allocator);
            self.code_fragments.deinit(allocator);
            self.data_segments.deinit(allocator);
            self.* = .{ .summary = .{} };
        }
    };

    allocator: std.mem.Allocator,
    startup_metadata: ?StartupMetadata = null,
    startup_depth: usize = 0,
    shutdown_happened: bool = false,

    fn init(allocator: std.mem.Allocator) CamlCompatState {
        return .{
            .allocator = allocator,
        };
    }

    fn reset(self: *CamlCompatState) void {
        if (self.startup_metadata) |*metadata| metadata.deinit(self.allocator);
        self.startup_metadata = null;
    }

    fn registerStartupMetadata(self: *CamlCompatState, tables: MetadataTables) !void {
        self.reset();
        if (tables.isEmpty()) {
            return;
        }

        var metadata = StartupMetadata{
            .summary = .{},
        };
        errdefer metadata.deinit(self.allocator);

        if (tables.frametables) |frametables| {
            var frametable_index: usize = 0;
            while (frametables[frametable_index]) |table| : (frametable_index += 1) {
                try metadata.frame_tables.append(self.allocator, .{
                    .table = table,
                    .descriptor_count = @as(usize, @intCast(table[0])),
                });
                metadata.summary.frametable_count += 1;
                metadata.summary.frame_descriptor_count += @as(usize, @intCast(table[0]));
            }
        }

        if (tables.globals) |globals| {
            var global_index: usize = 0;
            while (globals[global_index]) |table| : (global_index += 1) {
                const table_summary = summarizeGcRootTable(table);

                try metadata.gc_root_tables.append(self.allocator, .{
                    .table = table,
                    .entry_count = table_summary.entry_count,
                    .block_field_count = table_summary.block_field_count,
                });
                metadata.summary.gc_root_table_count += 1;
                metadata.summary.gc_root_entry_count += table_summary.entry_count;
                metadata.summary.gc_root_block_count += table_summary.block_count;
                metadata.summary.gc_root_block_field_count += table_summary.block_field_count;
            }
        }

        if (tables.code_segments) |code_segments| {
            var code_index: usize = 0;
            while (code_segments[code_index].begin) |begin| : (code_index += 1) {
                try metadata.code_fragments.append(self.allocator, .{
                    .begin = begin,
                    .end = code_segments[code_index].end.?,
                });
                metadata.summary.code_segment_count += 1;
            }
        }

        if (tables.data_segments) |data_segments| {
            var data_index: usize = 0;
            while (data_segments[data_index].begin) |begin| : (data_index += 1) {
                try metadata.data_segments.append(self.allocator, .{
                    .begin = begin,
                    .end = data_segments[data_index].end.?,
                });
                metadata.summary.data_segment_count += 1;
            }
        }

        self.startup_metadata = metadata;
    }

    fn beginStartup(self: *CamlCompatState) LifecycleError!bool {
        if (self.shutdown_happened) return error.StartupAfterShutdown;
        self.startup_depth +%= 1;
        return self.startup_depth == 1;
    }

    fn beginShutdown(self: *CamlCompatState) LifecycleError!bool {
        if (self.startup_depth == 0) return error.ShutdownWithoutStartup;
        self.startup_depth -= 1;
        if (self.startup_depth > 0) return false;
        self.shutdown_happened = true;
        return true;
    }

    fn startupDepth(self: *const CamlCompatState) usize {
        return self.startup_depth;
    }

    fn hasStartupMetadata(self: *const CamlCompatState) bool {
        return self.startup_metadata != null;
    }

    fn summary(self: *const CamlCompatState) MetadataSummary {
        return if (self.startup_metadata) |metadata| metadata.summary else .{};
    }

    fn visitGcRootBlockFields(self: *const CamlCompatState, visitor: RawRootSlotVisitor) void {
        const metadata = self.startup_metadata orelse return;
        for (metadata.gc_root_tables.items) |table| table.visitBlockFields(visitor);
    }

    fn gcRootBlockFieldSlotCount(self: *const CamlCompatState) usize {
        return self.summary().gc_root_block_field_count;
    }

    fn gcRootBlockFieldSlotAt(self: *const CamlCompatState, slot_index: usize) ?*RawValue {
        const metadata = self.startup_metadata orelse return null;
        var remaining = slot_index;
        for (metadata.gc_root_tables.items) |table| {
            if (remaining < table.block_field_count) {
                return table.blockFieldSlotAt(remaining);
            }
            remaining -= table.block_field_count;
        }
        return null;
    }

    fn findCodeFragment(self: *const CamlCompatState, pc: *const anyopaque) ?RegisteredCodeFragment {
        const metadata = self.startup_metadata orelse return null;
        for (metadata.code_fragments.items) |fragment| {
            if (fragment.contains(pc)) return fragment;
        }
        return null;
    }
};

const FakeDomainState = extern struct {
    pad0: [0x28]u8 = [_]u8{0} ** 0x28,
    // The locked arm64 compiler-emitted startup path reads `current_stack`
    // for the non-growing stack guard and `c_stack` for no-allocation C calls
    // such as `caml_initialize`. Other domain-state fields stay outside this
    // narrow compatibility shim until broader compiler/runtime support lands.
    current_stack: usize = 0,
    pad1: [0x10]u8 = [_]u8{0} ** 0x10,
    c_stack: usize = 0,
};

extern fn caml_start_program(state: *FakeDomainState) callconv(.c) RawValue;
extern fn caml_program() callconv(.c) RawValue;
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
var caml_compat_state = CamlCompatState.init(std.heap.page_allocator);

pub export var caml_globals_inited: usize = 0;
pub export var @"caml_system$frametable": [1]usize = .{0};
pub export var zort_last_emitted_int: i64 = -1;
pub export var zort_startup_calls: usize = 0;
pub export var zort_shutdown_calls: usize = 0;
pub export var zort_start_program_calls: usize = 0;
pub export var zort_last_start_program_result: RawValue = raw_unit;
pub export var zort_startup_depth: usize = 0;
pub export var zort_shutdown_happened: usize = 0;
pub export var zort_metadata_registration_calls: usize = 0;
pub export var zort_metadata_registered: usize = 0;
pub export var zort_metadata_frametables: usize = 0;
pub export var zort_metadata_frame_descriptors: usize = 0;
pub export var zort_metadata_gc_root_tables: usize = 0;
pub export var zort_metadata_gc_root_entries: usize = 0;
pub export var zort_metadata_gc_root_blocks: usize = 0;
pub export var zort_metadata_gc_root_block_fields: usize = 0;
pub export var zort_metadata_code_segments: usize = 0;
pub export var zort_metadata_data_segments: usize = 0;
pub export var zort_metadata_program_fragment_registered: usize = 0;

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
        const table_summary = summarizeGcRootTable(table);
        summary.gc_root_entry_count += table_summary.entry_count;
        summary.gc_root_block_count += table_summary.block_count;
        summary.gc_root_block_field_count += table_summary.block_field_count;
    }

    summary.code_segment_count = countSegments(code_segments);
    summary.data_segment_count = countSegments(data_segments);
    return summary;
}

fn externSymbolPtr(comptime T: type, comptime name: []const u8) T {
    return @ptrCast(@alignCast(@extern(*const anyopaque, .{ .name = name })));
}

fn resetObservabilityForFreshStartup() void {
    caml_globals_inited = 0;
    zort_last_emitted_int = -1;
    zort_startup_calls = 0;
    zort_shutdown_calls = 0;
    zort_start_program_calls = 0;
    zort_last_start_program_result = raw_unit;
    zort_startup_depth = 0;
    zort_shutdown_happened = 0;
    zort_metadata_registration_calls = 0;
    zort_metadata_registered = 0;
    zort_metadata_frametables = 0;
    zort_metadata_frame_descriptors = 0;
    zort_metadata_gc_root_tables = 0;
    zort_metadata_gc_root_entries = 0;
    zort_metadata_gc_root_blocks = 0;
    zort_metadata_gc_root_block_fields = 0;
    zort_metadata_code_segments = 0;
    zort_metadata_data_segments = 0;
    zort_metadata_program_fragment_registered = 0;
}

fn syncLifecycleObservability() void {
    zort_startup_depth = caml_compat_state.startupDepth();
    zort_shutdown_happened = @intFromBool(caml_compat_state.shutdown_happened);
}

fn syncMetadataObservability() void {
    const summary = caml_compat_state.summary();
    zort_metadata_registered = @intFromBool(caml_compat_state.hasStartupMetadata());
    zort_metadata_frametables = summary.frametable_count;
    zort_metadata_frame_descriptors = summary.frame_descriptor_count;
    zort_metadata_gc_root_tables = summary.gc_root_table_count;
    zort_metadata_gc_root_entries = summary.gc_root_entry_count;
    zort_metadata_gc_root_blocks = summary.gc_root_block_count;
    zort_metadata_gc_root_block_fields = summary.gc_root_block_field_count;
    zort_metadata_code_segments = summary.code_segment_count;
    zort_metadata_data_segments = summary.data_segment_count;
    zort_metadata_program_fragment_registered =
        @intFromBool(caml_compat_state.findCodeFragment(@ptrCast(&caml_program)) != null);
}

fn lifecycleFatal(err: CamlCompatState.LifecycleError) noreturn {
    const message = switch (err) {
        error.StartupAfterShutdown => "Fatal error: caml_startup was called after the runtime was shut down with caml_shutdown\n",
        error.ShutdownWithoutStartup => "Fatal error: a call to caml_shutdown has no corresponding call to caml_startup\n",
    };

    // Match the OCaml fatal shape without depending on Zig's panic formatting.
    _ = std.posix.write(std.posix.STDERR_FILENO, message) catch {};
    std.process.abort();
}

fn alignDown16(ptr: usize) usize {
    return ptr & ~@as(usize, 0xf);
}

fn startupCommon() RawValue {
    const should_initialize = caml_compat_state.beginStartup() catch |err| lifecycleFatal(err);
    if (should_initialize) resetObservabilityForFreshStartup();
    zort_startup_calls +%= 1;
    syncLifecycleObservability();

    if (!should_initialize) return raw_unit;

    caml_compat_state.registerStartupMetadata(MetadataTables.captureExtern()) catch
        @panic("zort: out of memory while registering compiler metadata");
    zort_metadata_registration_calls +%= 1;
    syncMetadataObservability();

    var c_stack_anchor: usize = 0;
    fake_domain.c_stack = alignDown16(@intFromPtr(&c_stack_anchor));
    const result = caml_start_program(&fake_domain);
    zort_start_program_calls +%= 1;
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
    const should_cleanup = caml_compat_state.beginShutdown() catch |err| lifecycleFatal(err);
    zort_shutdown_calls +%= 1;
    if (should_cleanup) {
        caml_compat_state.reset();
        caml_globals_inited = 0;
    }
    syncLifecycleObservability();
    syncMetadataObservability();
}

pub export fn caml_initialize(slot: *RawValue, value: RawValue) void {
    // This startup-only path covers compiler-emitted stores into preallocated
    // out-of-heap blocks. It intentionally stays in the compatibility layer.
    slot.* = value;
}

pub export fn zort_gc_root_block_field_slot_count() usize {
    return caml_compat_state.gcRootBlockFieldSlotCount();
}

pub export fn zort_gc_root_block_field_slot_at(index: usize) ?*RawValue {
    return caml_compat_state.gcRootBlockFieldSlotAt(index);
}

fn decodeImmediateInt(raw: RawValue) i64 {
    const signed: isize = @bitCast(raw);
    return @as(i64, @intCast(signed >> 1));
}

pub export fn zort_emit_int(raw: RawValue) callconv(.c) RawValue {
    zort_last_emitted_int = decodeImmediateInt(raw);
    return raw_unit;
}

fn makeRawOutOfHeapHeader(wosize: usize, tag: u8) RawValue {
    return (@as(RawValue, wosize) << raw_header_wosize_shift) |
        (@as(RawValue, 3) << raw_header_color_shift) |
        @as(RawValue, tag);
}

test "caml compat: primitive decodes tagged ints" {
    zort_last_emitted_int = -1;
    try std.testing.expectEqual(raw_unit, zort_emit_int(0x55));
    try std.testing.expectEqual(@as(i64, 42), zort_last_emitted_int);
}

test "caml compat: summarize startup metadata tables" {
    const frametable_a = [_]Intnat{ 2, 0, 0 };
    const frametable_b = [_]Intnat{ 1, 0 };
    const frametables = [_]?[*]const Intnat{
        frametable_a[0..].ptr,
        frametable_b[0..].ptr,
        null,
    };

    const TupleBlock = extern struct {
        header: RawValue,
        fields: [2]RawValue,
    };

    const root_block = TupleBlock{
        .header = makeRawOutOfHeapHeader(2, 0),
        .fields = .{ 0x11, 0x13 },
    };

    const roots_a = [_]RawValue{ @intFromPtr(root_block.fields[0..].ptr), 0 };
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
    try std.testing.expectEqual(@as(usize, 1), summary.gc_root_entry_count);
    try std.testing.expectEqual(@as(usize, 1), summary.gc_root_block_count);
    try std.testing.expectEqual(@as(usize, 2), summary.gc_root_block_field_count);
    try std.testing.expectEqual(@as(usize, 2), summary.code_segment_count);
    try std.testing.expectEqual(@as(usize, 1), summary.data_segment_count);
}

test "caml compat: state retains startup metadata and visits gc root block fields" {
    const frametable = [_]Intnat{1};
    const frametables = [_]?[*]const Intnat{
        frametable[0..].ptr,
        null,
    };
    const TupleBlock = extern struct {
        header: RawValue,
        fields: [2]RawValue,
    };
    const StringBlock = extern struct {
        header: RawValue,
        fields: [1]RawValue,
    };

    const tuple_block = TupleBlock{
        .header = makeRawOutOfHeapHeader(2, 0),
        .fields = .{ 0x11, 0x13 },
    };
    const string_block = StringBlock{
        .header = makeRawOutOfHeapHeader(1, 252),
        .fields = .{0x99},
    };

    const roots = [_]RawValue{
        @intFromPtr(tuple_block.fields[0..].ptr),
        raw_unit,
        @intFromPtr(string_block.fields[0..].ptr),
        0,
    };
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

    var state = CamlCompatState.init(std.testing.allocator);
    defer state.reset();
    try state.registerStartupMetadata(.{
        .frametables = frametables[0..].ptr,
        .globals = globals[0..].ptr,
        .code_segments = code_segments[0..].ptr,
        .data_segments = data_segments[0..].ptr,
    });

    const summary = state.summary();
    try std.testing.expect(state.hasStartupMetadata());
    try std.testing.expectEqual(@as(usize, 1), summary.frametable_count);
    try std.testing.expectEqual(@as(usize, 1), summary.gc_root_table_count);
    try std.testing.expectEqual(@as(usize, 3), summary.gc_root_entry_count);
    try std.testing.expectEqual(@as(usize, 1), summary.gc_root_block_count);
    try std.testing.expectEqual(@as(usize, 2), summary.gc_root_block_field_count);
    try std.testing.expectEqual(@as(usize, 1), state.startup_metadata.?.frame_tables.items.len);
    try std.testing.expectEqual(@as(usize, 3), state.startup_metadata.?.gc_root_tables.items[0].entry_count);
    try std.testing.expectEqual(@as(usize, 2), state.startup_metadata.?.gc_root_tables.items[0].block_field_count);
    try std.testing.expectEqual(@as(usize, 1), state.startup_metadata.?.code_fragments.items.len);
    try std.testing.expectEqual(RegisteredCodeFragment.DigestPolicy.later, state.startup_metadata.?.code_fragments.items[0].digest_policy);
    try std.testing.expectEqual(@as(usize, 2), state.gcRootBlockFieldSlotCount());
    try std.testing.expectEqual(@as(RawValue, 0x11), state.gcRootBlockFieldSlotAt(0).?.*);
    try std.testing.expectEqual(@as(RawValue, 0x13), state.gcRootBlockFieldSlotAt(1).?.*);
    try std.testing.expect(state.gcRootBlockFieldSlotAt(2) == null);
    try std.testing.expect(state.findCodeFragment(@ptrFromInt(0x18)) != null);
    try std.testing.expect(state.findCodeFragment(@ptrFromInt(0x28)) == null);

    var seen = std.ArrayListUnmanaged(RawValue){};
    defer seen.deinit(std.testing.allocator);

    const Collect = struct {
        fn visit(ctx: ?*anyopaque, slot: *RawValue) void {
            const items: *std.ArrayListUnmanaged(RawValue) = @ptrCast(@alignCast(ctx.?));
            items.append(std.testing.allocator, slot.*) catch unreachable;
        }
    };

    state.visitGcRootBlockFields(.{
        .ctx = &seen,
        .visit_fn = Collect.visit,
    });

    try std.testing.expectEqual(@as(usize, 2), seen.items.len);
    try std.testing.expectEqual(@as(RawValue, 0x11), seen.items[0]);
    try std.testing.expectEqual(@as(RawValue, 0x13), seen.items[1]);
}

test "caml compat: state reset clears registered startup metadata" {
    const frametable = [_]Intnat{1};
    const frametables = [_]?[*]const Intnat{
        frametable[0..].ptr,
        null,
    };
    const roots = [_]RawValue{ raw_unit, 0 };
    const globals = [_]?[*]const RawValue{
        roots[0..].ptr,
        null,
    };
    const segments = [_]Segment{
        .{ .begin = @ptrFromInt(0x10), .end = @ptrFromInt(0x20) },
        .{},
    };

    var state = CamlCompatState.init(std.testing.allocator);
    try state.registerStartupMetadata(.{
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

test "caml compat: startup ownership is reference counted until final shutdown" {
    var state = CamlCompatState.init(std.testing.allocator);
    defer state.reset();

    try std.testing.expect(try state.beginStartup());
    try std.testing.expectEqual(@as(usize, 1), state.startupDepth());

    try std.testing.expect(!(try state.beginStartup()));
    try std.testing.expectEqual(@as(usize, 2), state.startupDepth());

    try std.testing.expect(!(try state.beginShutdown()));
    try std.testing.expectEqual(@as(usize, 1), state.startupDepth());
    try std.testing.expect(!state.shutdown_happened);

    try std.testing.expect(try state.beginShutdown());
    try std.testing.expectEqual(@as(usize, 0), state.startupDepth());
    try std.testing.expect(state.shutdown_happened);
}

test "caml compat: startup ownership rejects invalid transitions" {
    var state = CamlCompatState.init(std.testing.allocator);
    defer state.reset();

    try std.testing.expectError(CamlCompatState.LifecycleError.ShutdownWithoutStartup, state.beginShutdown());

    _ = try state.beginStartup();
    _ = try state.beginShutdown();

    try std.testing.expectError(CamlCompatState.LifecycleError.StartupAfterShutdown, state.beginStartup());
}

test "caml compat: startup ownership rejects extra shutdown after balanced nested shutdown" {
    var state = CamlCompatState.init(std.testing.allocator);
    defer state.reset();

    _ = try state.beginStartup();
    _ = try state.beginStartup();
    _ = try state.beginShutdown();
    _ = try state.beginShutdown();

    try std.testing.expect(state.shutdown_happened);
    try std.testing.expectEqual(@as(usize, 0), state.startupDepth());
    try std.testing.expectError(CamlCompatState.LifecycleError.ShutdownWithoutStartup, state.beginShutdown());
}

test "caml compat: fake domain matches emitted startup offsets" {
    try std.testing.expectEqual(@as(usize, 0x28), @offsetOf(FakeDomainState, "current_stack"));
    try std.testing.expectEqual(@as(usize, 0x40), @offsetOf(FakeDomainState, "c_stack"));
}

test "caml compat: caml_initialize stores into startup-owned slots" {
    var slot: RawValue = raw_unit;
    caml_initialize(&slot, 0x24);
    try std.testing.expectEqual(@as(RawValue, 0x24), slot);
}

test "caml compat: reregistering startup metadata replaces prior registrations" {
    const first_frametable = [_]Intnat{ 1, 0 };
    const second_frametable = [_]Intnat{ 2, 0, 0 };
    const first_frametables = [_]?[*]const Intnat{
        first_frametable[0..].ptr,
        null,
    };
    const second_frametables = [_]?[*]const Intnat{
        second_frametable[0..].ptr,
        null,
    };
    const first_roots = [_]RawValue{ raw_unit, 0 };
    const second_roots = [_]RawValue{ raw_unit, 3, 0 };
    const first_globals = [_]?[*]const RawValue{
        first_roots[0..].ptr,
        null,
    };
    const second_globals = [_]?[*]const RawValue{
        second_roots[0..].ptr,
        null,
    };
    const first_segments = [_]Segment{
        .{ .begin = @ptrFromInt(0x1000), .end = @ptrFromInt(0x1100) },
        .{},
    };
    const second_segments = [_]Segment{
        .{ .begin = @ptrFromInt(0x2000), .end = @ptrFromInt(0x2200) },
        .{},
    };

    var state = CamlCompatState.init(std.testing.allocator);
    defer state.reset();

    try state.registerStartupMetadata(.{
        .frametables = first_frametables[0..].ptr,
        .globals = first_globals[0..].ptr,
        .code_segments = first_segments[0..].ptr,
        .data_segments = first_segments[0..].ptr,
    });
    try std.testing.expect(state.findCodeFragment(@ptrFromInt(0x1008)) != null);

    try state.registerStartupMetadata(.{
        .frametables = second_frametables[0..].ptr,
        .globals = second_globals[0..].ptr,
        .code_segments = second_segments[0..].ptr,
        .data_segments = second_segments[0..].ptr,
    });

    const summary = state.summary();
    try std.testing.expectEqual(@as(usize, 1), summary.frametable_count);
    try std.testing.expectEqual(@as(usize, 2), summary.frame_descriptor_count);
    try std.testing.expectEqual(@as(usize, 2), summary.gc_root_entry_count);
    try std.testing.expect(state.findCodeFragment(@ptrFromInt(0x1008)) == null);
    try std.testing.expect(state.findCodeFragment(@ptrFromInt(0x2008)) != null);
}
