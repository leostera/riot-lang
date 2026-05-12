const std = @import("std");
pub const build_options = @import("build_options");
const runtime = @import("runtime.zig");

const EventCounters = runtime.EventCounters;
const GcSnapshotEvent = runtime.GcSnapshotEvent;
const RootProviderEvent = runtime.RootProviderEvent;
const Runtime = runtime.Runtime;
const TraceEntry = runtime.TraceEntry;
const TraceRecorder = runtime.TraceRecorder;
const Value = runtime.Value;

const DefaultIters = 1_000;
const GraphDepth = 64;
const RootChurnSlots = 256;
const LongLivedChainDepth = 512;
const LongLivedBurstDepth = 24;
const SmallAllocTuples = [_]usize{ 4, 8, 12 };
const SmallAllocStrings = [_]usize{ 8, 12, 16 };
const MediumAllocTuples = [_]usize{ 16, 24, 32 };
const MediumAllocStrings = [_]usize{ 64, 128, 256 };
const LargeAllocTuples = [_]usize{ 96, 128, 192 };
const LargeAllocStrings = [_]usize{ 768, 1024, 1400 };

var bench_sink: usize = 0;

const BenchCase = struct {
    label: []const u8,
    run: *const fn (*Runtime, usize) anyerror!u64,
};

const TraceMode = enum {
    none,
    all,
    gc,
    effects,
    memprof,
};

const CaseProfile = struct {
    label: []const u8,
    strategy: Runtime.GcStrategy,
    iters: usize,
    ns_per_op: f64,
    counters: EventCounters,
    gc_snapshot: ?GcSnapshotEvent = null,
    root_providers: []RootProviderEvent = &.{},
};

const Config = struct {
    iters: usize,
    filter: ?[]const u8 = null,
    csv_path: ?[]const u8 = null,
    profile_json_path: ?[]const u8 = null,
    trace_mode: TraceMode = .none,
    gc_strategy: Runtime.GcStrategy = .mark_sweep,
    compare_strategies: bool = false,
};

const MaxTraceEntries = 128;

const all_strategies = [_]Runtime.GcStrategy{
    .mark_sweep,
    .generational,
    .bump,
};

fn parseGcStrategy(raw: []const u8) ?Runtime.GcStrategy {
    if (std.mem.eql(u8, raw, "mark-sweep") or std.mem.eql(u8, raw, "mark_sweep")) {
        return .mark_sweep;
    }
    if (std.mem.eql(u8, raw, "generational")) return .generational;
    if (std.mem.eql(u8, raw, "bump")) return .bump;
    return null;
}

fn parseConfig() Config {
    var args = std.process.args();
    _ = args.next();

    var iters: usize = DefaultIters;
    var filter: ?[]const u8 = null;
    var csv_path: ?[]const u8 = null;
    var profile_json_path: ?[]const u8 = null;
    var trace_mode: TraceMode = .none;
    var next_is_iters = false;
    var gc_strategy: Runtime.GcStrategy = .mark_sweep;
    var compare_strategies = false;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "help")) break;
        if (std.mem.eql(u8, arg, "--help")) break;

        if (next_is_iters) {
            iters = std.fmt.parseInt(usize, arg, 10) catch DefaultIters;
            next_is_iters = false;
            continue;
        }

        if (std.mem.startsWith(u8, arg, "--iters=")) {
            iters = std.fmt.parseInt(usize, arg[8..], 10) catch DefaultIters;
            continue;
        }
        if (std.mem.eql(u8, arg, "--iters")) {
            next_is_iters = true;
            continue;
        }

        if (std.mem.startsWith(u8, arg, "--filter=")) {
            filter = arg[9..];
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--csv=")) {
            csv_path = arg[6..];
            continue;
        }
        if (std.mem.eql(u8, arg, "--profile-json")) {
            profile_json_path = "notes/bench-profile.json";
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--profile-json=")) {
            profile_json_path = arg[15..];
            continue;
        }
        if (std.mem.eql(u8, arg, "--trace")) {
            trace_mode = .all;
            continue;
        }
        if (std.mem.eql(u8, arg, "--trace-gc")) {
            trace_mode = .gc;
            continue;
        }
        if (std.mem.eql(u8, arg, "--trace-effects")) {
            trace_mode = .effects;
            continue;
        }
        if (std.mem.eql(u8, arg, "--trace-memprof")) {
            trace_mode = .memprof;
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--gc-strategy=")) {
            const raw = arg[14..];
            if (std.mem.eql(u8, raw, "both")) {
                compare_strategies = true;
            } else if (parseGcStrategy(raw)) |strategy| {
                gc_strategy = strategy;
            }
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--strategy=")) {
            const raw = arg[11..];
            if (std.mem.eql(u8, raw, "both")) {
                compare_strategies = true;
            } else if (parseGcStrategy(raw)) |strategy| {
                gc_strategy = strategy;
            }
            continue;
        }

        if (std.fmt.parseInt(usize, arg, 10)) |value| {
            iters = value;
            continue;
        } else |_| {}
    }

    return .{
        .iters = iters,
        .filter = filter,
        .csv_path = csv_path,
        .profile_json_path = profile_json_path,
        .trace_mode = trace_mode,
        .gc_strategy = gc_strategy,
        .compare_strategies = compare_strategies,
    };
}

fn runNanos(duration_ns: u64, iters: usize) f64 {
    if (iters == 0) return 0;
    const count = @as(f64, @floatFromInt(iters));
    return @as(f64, @floatFromInt(duration_ns)) / count;
}

fn shouldRun(label: []const u8, filter: ?[]const u8) bool {
    if (filter == null) return true;
    return std.mem.indexOf(u8, label, filter.?) != null;
}

fn consume(value: Value) void {
    bench_sink = bench_sink +% value.toFingerprint();
}

fn runSuite(
    allocator: std.mem.Allocator,
    rt: *Runtime,
    recorder: *TraceRecorder,
    csv_file: ?*std.fs.File,
    profiles: ?*std.ArrayListUnmanaged(CaseProfile),
    iters: usize,
    label: []const u8,
    strategy: Runtime.GcStrategy,
    trace_mode: TraceMode,
    f: *const fn (*Runtime, usize) anyerror!u64,
) !void {
    recorder.clearCase();
    const before = recorder.snapshot();
    const elapsed = try f(rt, iters);
    const delta = EventCounters.diff(recorder.snapshot(), before);
    const ns_per_op = runNanos(elapsed, iters);
    std.log.info(
        "{s}:{s}: iters={d} ns/op={d:.2} alloc={d} field={d} bytes={d} barrier={d} root+={d} root-={d} collect={d} reclaim={d}",
        .{
            @tagName(strategy),
            label,
            iters,
            ns_per_op,
            delta.allocations,
            delta.field_writes,
            delta.bytes_writes,
            delta.barrier_records,
            delta.root_registrations,
            delta.root_unregistrations,
            delta.collections,
            delta.reclaims,
        },
    );
    if (delta.memprof_samples > 0 or delta.memprof_promotions > 0 or delta.memprof_reclaims > 0) {
        std.log.info(
            "{s}:{s}: memprof sampled={d} promoted={d} reclaimed={d}",
            .{
                @tagName(strategy),
                label,
                delta.memprof_samples,
                delta.memprof_promotions,
                delta.memprof_reclaims,
            },
        );
    }
    if (trace_mode != .none) {
        emitTrace(label, strategy, recorder.traceEntries(), trace_mode);
    }
    if (csv_file) |file| {
        try appendCsvRow(file, label, strategy, iters, ns_per_op, delta);
    }
    if (profiles) |items| {
        try appendCaseProfile(allocator, items, label, strategy, iters, ns_per_op, delta, recorder.last_gc_snapshot, recorder.rootProviderEntries());
    }
}

fn runAllCases(
    allocator: std.mem.Allocator,
    rt: *Runtime,
    recorder: *TraceRecorder,
    csv_file: ?*std.fs.File,
    profiles: ?*std.ArrayListUnmanaged(CaseProfile),
    iters: usize,
    filter: ?[]const u8,
    strategy: Runtime.GcStrategy,
    trace_mode: TraceMode,
    cases: []const BenchCase,
) !void {
    for (cases) |case| {
        if (!shouldRun(case.label, filter)) continue;
        try runSuite(allocator, rt, recorder, csv_file, profiles, iters, case.label, strategy, trace_mode, case.run);
    }
}

fn runBenchcases(
    allocator: std.mem.Allocator,
    config: Config,
    gc_strategy: Runtime.GcStrategy,
    profiles: ?*std.ArrayListUnmanaged(CaseProfile),
) !void {
    var recorder = TraceRecorder.init(allocator, .{
        .capture_events = config.trace_mode != .none,
    });
    defer recorder.deinit();
    var rt = Runtime.initWithConfig(allocator, .{
        .gcStrategy = gc_strategy,
        .eventSink = recorder.sink(),
        .memprof = .{
            .enabled = config.trace_mode == .memprof,
            .sample_interval_units = 16,
            .capture_backtraces = true,
            .sampling = .probabilistic_allocation_units,
            .seed = 7,
        },
    });
    defer rt.deinit();
    var csv_file: ?std.fs.File = null;
    defer if (csv_file) |*file| file.close();

    if (config.csv_path) |path| {
        csv_file = try openCsvAppend(path);
    }

    const cases = [_]BenchCase{
        .{ .label = "tuple-alloc", .run = benchmarkTupleAlloc },
        .{ .label = "tuple-update", .run = benchmarkTupleUpdate },
        .{ .label = "tuple-read", .run = benchmarkTupleRead },
        .{ .label = "string-alloc", .run = benchmarkStringAlloc },
        .{ .label = "string-overwrite", .run = benchmarkStringOverwrite },
        .{ .label = "alloc-pressure-small", .run = benchmarkAllocPressureSmall },
        .{ .label = "alloc-pressure-medium", .run = benchmarkAllocPressureMedium },
        .{ .label = "alloc-pressure-large", .run = benchmarkAllocPressureLarge },
        .{ .label = "gc-chain-reachable", .run = benchmarkGcReachableChain },
        .{ .label = "gc-chain-unrooted", .run = benchmarkGcNoRoots },
        .{ .label = "root-churn", .run = benchmarkRootChurn },
        .{ .label = "long-lived-sweep", .run = benchmarkLongLivedSweep },
        .{ .label = "effect-roundtrip", .run = benchmarkEffectRoundtrip },
        .{ .label = "mixed", .run = benchmarkMixed },
    };

    bench_sink = 0;
    std.log.info("gc-strategy={s} iters={d}", .{ @tagName(gc_strategy), config.iters });
    try runAllCases(
        allocator,
        &rt,
        &recorder,
        if (csv_file) |*file| file else null,
        profiles,
        config.iters,
        config.filter,
        gc_strategy,
        config.trace_mode,
        &cases,
    );

    rt.collect();
    const totals = recorder.snapshot();
    std.log.info(
        "gc-strategy={s} sink={d} totals alloc={d} field={d} bytes={d} barrier={d} root+={d} root-={d} collect={d} reclaim={d} memsample={d} mempromote={d} memreclaim={d}",
        .{
            @tagName(gc_strategy),
            bench_sink,
            totals.allocations,
            totals.field_writes,
            totals.bytes_writes,
            totals.barrier_records,
            totals.root_registrations,
            totals.root_unregistrations,
            totals.collections,
            totals.reclaims,
            totals.memprof_samples,
            totals.memprof_promotions,
            totals.memprof_reclaims,
        },
    );
}

fn appendCaseProfile(
    allocator: std.mem.Allocator,
    items: *std.ArrayListUnmanaged(CaseProfile),
    label: []const u8,
    strategy: Runtime.GcStrategy,
    iters: usize,
    ns_per_op: f64,
    counters: EventCounters,
    gc_snapshot: ?GcSnapshotEvent,
    providers: []const RootProviderEvent,
) !void {
    const provider_copy = try allocator.dupe(RootProviderEvent, providers);
    try items.append(allocator, .{
        .label = label,
        .strategy = strategy,
        .iters = iters,
        .ns_per_op = ns_per_op,
        .counters = counters,
        .gc_snapshot = gc_snapshot,
        .root_providers = provider_copy,
    });
}

fn deinitProfiles(allocator: std.mem.Allocator, profiles: *std.ArrayListUnmanaged(CaseProfile)) void {
    for (profiles.items) |profile| allocator.free(profile.root_providers);
    profiles.deinit(allocator);
}

fn shouldTraceEntry(mode: TraceMode, entry: TraceEntry) bool {
    return switch (mode) {
        .none => false,
        .all => true,
        .gc => switch (entry.event) {
            .root_provider,
            .collect,
            .gc_phase,
            .gc_snapshot,
            .reclaim,
            => true,
            else => false,
        },
        .effects => switch (entry.event) {
            .control => true,
            else => false,
        },
        .memprof => switch (entry.event) {
            .memprof => true,
            else => false,
        },
    };
}

fn emitTrace(label: []const u8, strategy: Runtime.GcStrategy, entries: []const TraceEntry, mode: TraceMode) void {
    var printed: usize = 0;
    var suppressed: usize = 0;
    for (entries) |entry| {
        if (!shouldTraceEntry(mode, entry)) continue;
        if (printed >= MaxTraceEntries) {
            suppressed += 1;
            continue;
        }
        switch (entry.event) {
            .alloc => |event| std.log.info(
                "trace:{s}:{s}: alloc ts={d} handle={d}:{d} kind={s} payload_bytes={d} storage_bytes={d} scan_words={d} units={d}",
                .{
                    @tagName(strategy),
                    label,
                    entry.timestamp_ms,
                    event.handle.index,
                    event.handle.generation,
                    @tagName(event.kind),
                    event.payload_bytes,
                    event.storage_bytes,
                    event.scan_words,
                    event.allocation_cost_units,
                },
            ),
            .field_write => |event| std.log.info(
                "trace:{s}:{s}: field ts={d} target={d}:{d} index={d} phase={s}",
                .{ @tagName(strategy), label, entry.timestamp_ms, event.target.index, event.target.generation, event.index, @tagName(event.phase) },
            ),
            .bytes_write => |event| std.log.info(
                "trace:{s}:{s}: bytes ts={d} target={d}:{d} len={d} phase={s}",
                .{ @tagName(strategy), label, entry.timestamp_ms, event.target.index, event.target.generation, event.len, @tagName(event.phase) },
            ),
            .barrier => |event| std.log.info(
                "trace:{s}:{s}: barrier ts={d} target={d}:{d} block={any}",
                .{ @tagName(strategy), label, entry.timestamp_ms, event.target.index, event.target.generation, event.value_is_block },
            ),
            .root => |event| std.log.info(
                "trace:{s}:{s}: root ts={d} action={s} block={any}",
                .{ @tagName(strategy), label, entry.timestamp_ms, @tagName(event.action), event.is_block },
            ),
            .root_provider => |event| std.log.info(
                "trace:{s}:{s}: root-provider ts={d} name={s} count={d}",
                .{ @tagName(strategy), label, entry.timestamp_ms, event.name, event.count },
            ),
            .collect => |event| std.log.info(
                "trace:{s}:{s}: collect ts={d} phase={s} roots={d} reclaimed={d}",
                .{ @tagName(strategy), label, entry.timestamp_ms, @tagName(event.phase), event.root_count, event.reclaimed },
            ),
            .gc_phase => |event| std.log.info(
                "trace:{s}:{s}: gc-phase ts={d} phase={s} elapsed_ns={d}",
                .{ @tagName(strategy), label, entry.timestamp_ms, @tagName(event.phase), event.elapsed_ns },
            ),
            .gc_snapshot => |event| std.log.info(
                "trace:{s}:{s}: gc-snapshot ts={d} roots={d} marked={d}/{d}/{d}/{d}/{d} promoted={d}/{d}/{d}/{d}/{d} promoted_units={d} reclaimed={d}/{d}/{d}/{d}/{d} nursery={d}obj/{d}u major={d}obj/{d}u weak={d} finalizers={d} ns={d}/{d}/{d}/{d}/{d}/{d}",
                .{
                    @tagName(strategy),
                    label,
                    entry.timestamp_ms,
                    event.root_count,
                    event.marked.tuple,
                    event.marked.string,
                    event.marked.boxed_i64,
                    event.marked.boxed_f64,
                    event.marked.custom,
                    event.promoted.tuple,
                    event.promoted.string,
                    event.promoted.boxed_i64,
                    event.promoted.boxed_f64,
                    event.promoted.custom,
                    event.promoted_allocation_units,
                    event.reclaimed.tuple,
                    event.reclaimed.string,
                    event.reclaimed.boxed_i64,
                    event.reclaimed.boxed_f64,
                    event.reclaimed.custom,
                    event.nursery_objects,
                    event.nursery_allocation_units,
                    event.major_objects,
                    event.major_allocation_units,
                    event.weak_processed,
                    event.finalizers_ready,
                    event.timings.root_enumeration_ns,
                    event.timings.mark_ns,
                    event.timings.weak_ns,
                    event.timings.finalizers_ns,
                    event.timings.sweep_ns,
                    event.timings.total_ns,
                },
            ),
            .reclaim => |event| std.log.info(
                "trace:{s}:{s}: reclaim ts={d} handle={d}:{d} kind={s}",
                .{ @tagName(strategy), label, entry.timestamp_ms, event.handle.index, event.handle.generation, @tagName(event.kind) },
            ),
            .memprof => |event| std.log.info(
                "trace:{s}:{s}: memprof ts={d} action={s} handle={d}:{d} sample={d} kind={s} payload_bytes={d} storage_bytes={d} scan_words={d} units={d} space={s} promotions={d} depth={d}",
                .{
                    @tagName(strategy),
                    label,
                    entry.timestamp_ms,
                    @tagName(event.action),
                    event.handle.index,
                    event.handle.generation,
                    event.sample_ordinal,
                    @tagName(event.kind),
                    event.payload_bytes,
                    event.storage_bytes,
                    event.scan_words,
                    event.allocation_cost_units,
                    @tagName(event.space),
                    event.promotion_count,
                    event.backtrace_depth,
                },
            ),
            .control => |event| std.log.info(
                "trace:{s}:{s}: control ts={d} action={s} site={d} effect={any} fiber={any} cont={any} handler={any}:{any} depth={d}",
                .{
                    @tagName(strategy),
                    label,
                    entry.timestamp_ms,
                    @tagName(event.action),
                    event.site_id,
                    event.effect,
                    event.fiber,
                    event.continuation,
                    event.handler_fiber,
                    event.handler_index,
                    event.parent_depth,
                },
            ),
        }
        printed += 1;
    }
    if (suppressed > 0) {
        std.log.info(
            "trace:{s}:{s}: truncated {d} additional events (limit={d})",
            .{ @tagName(strategy), label, suppressed, MaxTraceEntries },
        );
    }
}

fn writeProfileJson(path: []const u8, config: Config, profiles: []const CaseProfile) !void {
    if (std.fs.path.dirname(path)) |dir| {
        if (dir.len > 0) try std.fs.cwd().makePath(dir);
    }
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();
    var write_buffer: [4096]u8 = undefined;
    var file_writer = file.writer(&write_buffer);
    const writer = &file_writer.interface;
    try std.json.Stringify.value(.{
        .generated_at_ms = std.time.milliTimestamp(),
        .iters = config.iters,
        .trace_mode = @tagName(config.trace_mode),
        .cases = profiles,
    }, .{ .whitespace = .indent_2 }, writer);
    try writer.flush();
}

fn openCsvAppend(path: []const u8) !std.fs.File {
    if (std.fs.path.dirname(path)) |dir| {
        if (dir.len > 0) try std.fs.cwd().makePath(dir);
    }

    var file = std.fs.cwd().openFile(path, .{ .mode = .read_write }) catch |err| switch (err) {
        error.FileNotFound => try std.fs.cwd().createFile(path, .{ .read = true }),
        else => return err,
    };

    const stat = try file.stat();
    if (stat.size == 0) {
        try file.writeAll("timestamp_ms,strategy,label,iters,ns_per_op,allocations,field_writes,bytes_writes,barrier_records,root_registrations,root_unregistrations,collections,reclaims\n");
    }
    try file.seekFromEnd(0);
    return file;
}

fn appendCsvRow(
    file: *std.fs.File,
    label: []const u8,
    strategy: Runtime.GcStrategy,
    iters: usize,
    ns_per_op: f64,
    counters: EventCounters,
) !void {
    var buffer: [256]u8 = undefined;
    const row = try std.fmt.bufPrint(
        &buffer,
        "{d},{s},{s},{d},{d:.2},{d},{d},{d},{d},{d},{d},{d},{d}\n",
        .{
            std.time.milliTimestamp(),
            @tagName(strategy),
            label,
            iters,
            ns_per_op,
            counters.allocations,
            counters.field_writes,
            counters.bytes_writes,
            counters.barrier_records,
            counters.root_registrations,
            counters.root_unregistrations,
            counters.collections,
            counters.reclaims,
        },
    );
    try file.writeAll(row);
}

fn benchmarkTupleAlloc(rt: *Runtime, iters: usize) !u64 {
    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        const tuple = try rt.allocTuple(4);
        try rt.setField(tuple, 0, Value.fromInt(@as(i64, @intCast(i))));
        try rt.setField(tuple, 1, Value.fromInt(@as(i64, @intCast(i + 1))));
        try rt.setField(tuple, 2, Value.fromInt(@as(i64, @intCast(i + 2))));
        try rt.setField(tuple, 3, Value.fromInt(@as(i64, @intCast(i + 3))));
        consume(tuple);
        if ((i % 2_048) == 0) rt.collect();
    }
    return timer.read();
}

fn benchmarkTupleUpdate(rt: *Runtime, iters: usize) !u64 {
    var frame = rt.beginRootFrame();
    defer frame.end();
    var tuple = try frame.bind(try rt.allocTuple(16));

    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        var index: usize = 0;
        while (index < 16) : (index += 1) {
            try rt.setField(tuple.get(), index, Value.fromInt(@as(i64, @intCast(i + index))));
        }
        if ((i % 4_096) == 0) rt.collect();
    }
    consume(tuple.get());
    return timer.read();
}

fn benchmarkTupleRead(rt: *Runtime, iters: usize) !u64 {
    var frame = rt.beginRootFrame();
    defer frame.end();
    var tuple = try frame.bind(try rt.allocTuple(16));

    var i: usize = 0;
    while (i < 16) : (i += 1) {
        try rt.setField(tuple.get(), i, Value.fromInt(@as(i64, @intCast(i))));
    }

    var timer = try std.time.Timer.start();
    i = 0;
    while (i < iters) : (i += 1) {
        var total: usize = 0;
        var index: usize = 0;
        while (index < 16) : (index += 1) {
            const field = try rt.field(tuple.get(), index);
            if (field.isImmediate()) {
                total += 1;
            }
        }
        consume(Value.fromInt(@as(i64, @intCast(total))));
        if ((i % 2_048) == 0) rt.collect();
    }
    return timer.read();
}

fn benchmarkStringAlloc(rt: *Runtime, iters: usize) !u64 {
    const literal = "zort-runtime-bench";
    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        const text = try rt.allocString(literal);
        consume(text);
        if ((i % 1_024) == 0) rt.collect();
    }
    return timer.read();
}

fn benchmarkStringOverwrite(rt: *Runtime, iters: usize) !u64 {
    const initial = "zzzzzzzzzzzzzzzzzzzzzz";
    const updates = "zig";
    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        const text = try rt.allocStringWithFill(initial.len, 0);
        if ((i & 1) == 0) {
            try rt.setStringBytes(text, initial);
        } else {
            try rt.setStringBytes(text, updates);
        }
        const bytes = try rt.stringSlice(text);
        consume(Value.fromInt(@as(i64, @intCast(bytes.len))));
        if ((i % 1_024) == 0) rt.collect();
    }
    return timer.read();
}

fn benchmarkAllocPressure(rt: *Runtime, iters: usize, tuple_sizes: []const usize, string_sizes: []const usize, collect_every: usize) !u64 {
    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        const tuple_len = tuple_sizes[i % tuple_sizes.len];
        const string_len = string_sizes[(i / 3) % string_sizes.len];

        const tuple = try rt.allocTuple(tuple_len);
        var field: usize = 0;
        while (field < tuple_len) : (field += 1) {
            const base = @as(i64, @intCast(i));
            try rt.setField(tuple, field, Value.fromInt(base + @as(i64, @intCast(field))));
        }

        const string = try rt.allocStringWithFill(string_len, @as(u8, @intCast('a' + (i % 26))));
        const number = try rt.allocI64(@as(i64, @intCast(i)));
        consume(tuple);
        consume(string);
        consume(number);

        if (collect_every > 0 and ((i % collect_every) == 0)) {
            rt.collect();
        }
    }
    return timer.read();
}

fn benchmarkAllocPressureSmall(rt: *Runtime, iters: usize) !u64 {
    return benchmarkAllocPressure(rt, iters, &SmallAllocTuples, &SmallAllocStrings, 256);
}

fn benchmarkAllocPressureMedium(rt: *Runtime, iters: usize) !u64 {
    return benchmarkAllocPressure(rt, iters, &MediumAllocTuples, &MediumAllocStrings, 128);
}

fn benchmarkAllocPressureLarge(rt: *Runtime, iters: usize) !u64 {
    return benchmarkAllocPressure(rt, iters, &LargeAllocTuples, &LargeAllocStrings, 64);
}

fn buildChain(rt: *Runtime, length: usize) !Value {
    const head = try rt.allocTuple(2);
    var previous = head;
    var i: usize = 1;

    while (i < length) : (i += 1) {
        const next = try rt.allocTuple(2);
        try rt.setField(previous, 0, next);
        try rt.setField(previous, 1, Value.fromInt(@as(i64, @intCast(i))));
        previous = next;
    }
    return head;
}

fn benchmarkGcReachableChain(rt: *Runtime, iters: usize) !u64 {
    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        var frame = rt.beginRootFrame();
        _ = try frame.bind(try buildChain(rt, GraphDepth));
        rt.collect();
        frame.end();
        rt.collect();
    }
    return timer.read();
}

fn benchmarkGcNoRoots(rt: *Runtime, iters: usize) !u64 {
    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        _ = try buildChain(rt, 16);
        rt.collect();
    }
    return timer.read();
}

fn benchmarkRootChurn(rt: *Runtime, iters: usize) !u64 {
    var root_slots = [_]Value{Value.fromInt(0)} ** RootChurnSlots;
    var active = [_]bool{false} ** RootChurnSlots;
    var i: usize = 0;
    var timer = try std.time.Timer.start();

    while (i < iters) : (i += 1) {
        const slot = i % RootChurnSlots;
        if (active[slot]) {
            rt.unregisterRoot(&root_slots[slot]);
            active[slot] = false;
        }

        const tuple = try rt.allocTuple(4);
        try rt.setField(tuple, 0, Value.fromInt(@as(i64, @intCast(i))));
        try rt.setField(tuple, 1, Value.fromInt(@as(i64, @intCast(slot))));
        try rt.setField(tuple, 2, Value.fromInt(@as(i64, @intCast(iters))));
        try rt.setField(tuple, 3, Value.fromInt(@as(i64, @intCast(i + slot))));
        root_slots[slot] = tuple;
        try rt.registerRoot(&root_slots[slot]);
        active[slot] = true;
        consume(tuple);

        if ((i & 0x7FF) == 0) rt.collect();
    }

    for (active, 0..) |slot_active, idx| {
        if (slot_active) rt.unregisterRoot(&root_slots[idx]);
    }
    return timer.read();
}

fn benchmarkLongLivedSweep(rt: *Runtime, iters: usize) !u64 {
    var frame = rt.beginRootFrame();
    defer frame.end();
    var long_lived = try frame.bind(try buildChain(rt, LongLivedChainDepth));

    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        _ = try buildChain(rt, LongLivedBurstDepth);
        if ((i % 4) == 0) {
            const marker = try rt.allocI64(@as(i64, @intCast(i)));
            try rt.setField(long_lived.get(), 1, marker);
        }
        if ((i & 0x7) == 0) {
            consume(try rt.field(long_lived.get(), 0));
        }
        rt.collect();
    }
    return timer.read();
}

fn benchmarkEffectRoundtrip(rt: *Runtime, iters: usize) !u64 {
    const effect: runtime.EffectId = 41;
    const main_fiber = rt.currentFiber();
    try rt.pushEffectHandler(main_fiber, .{
        .effect = effect,
        .handle_effect = Value.fromInt(1),
    });
    defer _ = rt.popEffectHandler(main_fiber) catch {};

    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        try rt.pushFiberFrame(main_fiber, @intCast(i));
        const payload = try rt.allocTuple(1);
        try rt.setField(payload, 0, Value.fromInt(@as(i64, @intCast(i))));
        try rt.pushFiberFrameRoot(main_fiber, payload);

        const performed = try rt.performEffectAt(@intCast(i), effect, payload, &.{payload});
        _ = try rt.resumeContinuation(performed.continuation, Value.fromInt(@as(i64, @intCast(i))));
        _ = rt.dropContinuation(performed.continuation);
        _ = try rt.popFiberFrame(main_fiber);

        if ((i & 0x7F) == 0) rt.collect();
    }
    return timer.read();
}

fn benchmarkMixed(rt: *Runtime, iters: usize) !u64 {
    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        const tuple = try rt.allocTuple(3);
        const text = try rt.allocStringWithFill(8, 'z');
        const number = try rt.allocInt64(@as(i64, @intCast(i * 2)));
        try rt.setField(tuple, 0, text);
        try rt.setField(tuple, 1, number);
        try rt.setField(tuple, 2, Value.fromInt(@as(i64, @intCast(i))));
        consume(try rt.field(tuple, 0));
        consume(try rt.field(tuple, 1));
        consume(try rt.field(tuple, 2));
        if ((i % 4_096) == 0) rt.collect();
    }
    return timer.read();
}

pub fn main() !void {
    const config = parseConfig();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var profiles = std.ArrayListUnmanaged(CaseProfile){};
    defer deinitProfiles(gpa.allocator(), &profiles);

    if (config.compare_strategies) {
        for (all_strategies) |strategy| {
            try runBenchcases(gpa.allocator(), config, strategy, if (config.profile_json_path != null) &profiles else null);
        }
    } else {
        try runBenchcases(gpa.allocator(), config, config.gc_strategy, if (config.profile_json_path != null) &profiles else null);
    }

    if (config.profile_json_path) |path| {
        try writeProfileJson(path, config, profiles.items);
    }
}
