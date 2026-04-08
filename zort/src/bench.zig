const std = @import("std");
const runtime = @import("runtime.zig");

const Runtime = runtime.Runtime;
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

const Config = struct {
    iters: usize,
    filter: ?[]const u8 = null,
    gc_strategy: Runtime.GcStrategy = .mark_sweep,
    compare_strategies: bool = false,
};

const all_strategies = [_]Runtime.GcStrategy{
    .mark_sweep,
    .bump,
};

fn parseGcStrategy(raw: []const u8) ?Runtime.GcStrategy {
    if (std.mem.eql(u8, raw, "mark-sweep") or std.mem.eql(u8, raw, "mark_sweep")) {
        return .mark_sweep;
    }
    if (std.mem.eql(u8, raw, "bump")) return .bump;
    return null;
}

fn parseConfig() Config {
    var args = std.process.args();
    _ = args.next();

    var iters: usize = DefaultIters;
    var filter: ?[]const u8 = null;
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
    rt: *Runtime,
    iters: usize,
    label: []const u8,
    strategy: Runtime.GcStrategy,
    f: *const fn (*Runtime, usize) anyerror!u64,
) !void {
    const elapsed = try f(rt, iters);
    const ns_per_op = runNanos(elapsed, iters);
    std.log.info("{s}:{s}: iters={d} ns/op={d:.2}", .{ @tagName(strategy), label, iters, ns_per_op });
}

fn runAllCases(rt: *Runtime, iters: usize, filter: ?[]const u8, strategy: Runtime.GcStrategy, cases: []const BenchCase) !void {
    for (cases) |case| {
        if (!shouldRun(case.label, filter)) continue;
        try runSuite(rt, iters, case.label, strategy, case.run);
    }
}

fn runBenchcases(allocator: std.mem.Allocator, iters: usize, filter: ?[]const u8, gc_strategy: Runtime.GcStrategy) !void {
    var rt = Runtime.initWithConfig(allocator, .{ .gcStrategy = gc_strategy });
    defer rt.deinit();

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
        .{ .label = "mixed", .run = benchmarkMixed },
    };

    bench_sink = 0;
    std.log.info("gc-strategy={s} iters={d}", .{ @tagName(gc_strategy), iters });
    try runAllCases(&rt, iters, filter, gc_strategy, &cases);

    rt.collect();
    std.log.info("gc-strategy={s} sink={d}", .{ @tagName(gc_strategy), bench_sink });
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
    var tuple = try rt.allocTuple(16);
    try rt.registerRoot(&tuple);
    defer rt.unregisterRoot(&tuple);

    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        var index: usize = 0;
        while (index < 16) : (index += 1) {
            try rt.setField(tuple, index, Value.fromInt(@as(i64, @intCast(i + index))));
        }
        if ((i % 4_096) == 0) rt.collect();
    }
    consume(tuple);
    return timer.read();
}

fn benchmarkTupleRead(rt: *Runtime, iters: usize) !u64 {
    var tuple = try rt.allocTuple(16);
    try rt.registerRoot(&tuple);
    defer rt.unregisterRoot(&tuple);

    var i: usize = 0;
    while (i < 16) : (i += 1) {
        try rt.setField(tuple, i, Value.fromInt(@as(i64, @intCast(i))));
    }

    var timer = try std.time.Timer.start();
    i = 0;
    while (i < iters) : (i += 1) {
        var total: usize = 0;
        var index: usize = 0;
        while (index < 16) : (index += 1) {
            const field = try rt.field(tuple, index);
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
        var root = try buildChain(rt, GraphDepth);
        try rt.registerRoot(&root);
        rt.collect();
        rt.unregisterRoot(&root);
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
    var long_lived = try buildChain(rt, LongLivedChainDepth);
    try rt.registerRoot(&long_lived);
    defer rt.unregisterRoot(&long_lived);

    var i: usize = 0;
    var timer = try std.time.Timer.start();
    while (i < iters) : (i += 1) {
        _ = try buildChain(rt, LongLivedBurstDepth);
        if ((i % 4) == 0) {
            const marker = try rt.allocI64(@as(i64, @intCast(i)));
            try rt.setField(long_lived, 1, marker);
        }
        if ((i & 0x7) == 0) {
            consume(try rt.field(long_lived, 0));
        }
        rt.collect();
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

    if (config.compare_strategies) {
        for (all_strategies) |strategy| {
            try runBenchcases(gpa.allocator(), config.iters, config.filter, strategy);
        }
    } else {
        try runBenchcases(gpa.allocator(), config.iters, config.filter, config.gc_strategy);
    }
}
