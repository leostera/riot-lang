const std = @import("std");
const zort = @import("zort");
const common = @import("common.zig");

const Runtime = zort.Runtime;
const TraceRecorder = zort.TraceRecorder;
const EventCounters = zort.EventCounters;
const Value = zort.Value;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    const allocator = gpa.allocator();
    var trace = TraceRecorder.init(allocator, .{
        .track_object_events = true,
    });
    defer trace.deinit();

    var rt = Runtime.initWithConfig(allocator, .{
        .eventSink = trace.sink(),
        .debugChecks = .{
            .verify_heap_store = true,
            .verify_roots = true,
            .verify_after_collect = true,
        },
    });
    defer rt.deinit();

    const smoke_before = trace.snapshot();

    const left = try rt.allocI64(7);
    const right = try rt.allocString("hello from zort");
    var rooted = try rt.tuple(&.{ left, right });
    try rt.registerRoot(&rooted);

    try common.expectEqual(usize, try rt.tupleLength(rooted), 2, "tuple length");
    try common.expectEqual(i64, try rt.unboxI64(try rt.field(rooted, 0)), 7, "boxed i64 payload");
    try common.expectBytesEqual(try rt.stringSlice(try rt.field(rooted, 1)), "hello from zort", "string payload");

    const rooted_explain = try rt.explainValue(rooted, &trace);
    try common.expectEqual(usize, rooted_explain.explicit_roots, 1, "tuple explicit root ownership");

    rt.collectMajor();
    try rt.verifyDebugState();
    try common.expectEqual(usize, rt.objectCount(), 3, "rooted graph survives collection");

    rooted = try rt.allocString("kept alive");
    rt.collectMajor();
    try rt.verifyDebugState();
    try common.expectEqual(usize, rt.objectCount(), 1, "replacing the root slot releases old graph");
    try common.expectBytesEqual(try rt.stringSlice(rooted), "kept alive", "replacement root string");

    rt.unregisterRoot(&rooted);
    rt.collectMajor();
    try rt.verifyDebugState();
    try common.expectEqual(usize, rt.objectCount(), 0, "unrooted values reclaim");

    const smoke_delta = EventCounters.diff(trace.snapshot(), smoke_before);
    try common.expect(smoke_delta.allocations >= 4, "alloc/gc smoke should allocate runtime objects");
    try common.expect(smoke_delta.collections >= 3, "alloc/gc smoke should collect repeatedly");
    try common.expect(smoke_delta.reclaims >= 3, "alloc/gc smoke should reclaim old graph");

    var bench = Runtime.init(allocator);
    defer bench.deinit();

    const bench_iters: usize = 128;
    var timer = try std.time.Timer.start();
    var i: usize = 0;
    while (i < bench_iters) : (i += 1) {
        const tuple = try bench.allocTuple(2);
        try bench.setField(tuple, 0, Value.fromInt(@intCast(i)));
        try bench.setField(tuple, 1, try bench.allocString("bench"));
        if ((i & 31) == 0) bench.collect();
    }
    bench.collectMajor();
    const ns_per_op = @as(f64, @floatFromInt(timer.read())) / @as(f64, @floatFromInt(bench_iters));

    std.debug.print(
        "e2e alloc-gc-smoke ok output=hello from zort alloc={d} collect={d} reclaim={d} bench_ns_per_op={d:.2}\n",
        .{
            smoke_delta.allocations,
            smoke_delta.collections,
            smoke_delta.reclaims,
            ns_per_op,
        },
    );
}
