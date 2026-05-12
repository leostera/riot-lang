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
        .capture_events = true,
    });
    defer trace.deinit();

    var rt = Runtime.initWithConfig(allocator, .{
        .eventSink = trace.sink(),
        .debugChecks = .{
            .verify_control_kernel = true,
            .verify_roots = true,
            .verify_after_collect = true,
        },
    });
    defer rt.deinit();

    const effect: zort.EffectId = 41;
    const main_fiber = rt.currentFiber();
    try rt.pushEffectHandler(main_fiber, .{
        .effect = effect,
        .handle_effect = Value.fromInt(1),
    });
    defer _ = rt.popEffectHandler(main_fiber) catch {};

    const smoke_before = trace.snapshot();

    const child = try rt.spawnFiberInDomain(main_fiber, rt.currentDomain());
    try rt.activateFiberInDomain(rt.currentDomain(), child);

    const rooted = try rt.allocString("resume me");
    try rt.pushFiberFrame(child, 9001);
    try rt.pushFiberFrameRoot(child, rooted);

    const performed = try rt.performEffectAt(9001, effect, Value.fromInt(7), &.{rooted});
    try common.expect(common.sameHandle(performed.handler_fiber, main_fiber), "effect should land in the parent handler");

    const snapshot = try rt.snapshotContinuationStack(allocator, performed.continuation);
    defer {
        var owned_snapshot = snapshot;
        owned_snapshot.deinit(allocator);
    }
    try common.expectEqual(usize, snapshot.frame_count, 1, "captured stack frame count");
    try common.expectEqual(usize, snapshot.root_count, 1, "captured stack root count");
    try common.expectEqual(u32, snapshot.capture_site_id, 9001, "captured stack site id");

    const continuation_backtrace = try rt.captureContinuationBacktrace(allocator, performed.continuation);
    defer allocator.free(continuation_backtrace);
    try common.expect(continuation_backtrace.len >= 1, "captured continuation backtrace should not be empty");
    try common.expectEqual(u32, continuation_backtrace[0].site_id, 9001, "backtrace should start at performed frame");

    rt.collectMajor();
    try rt.verifyDebugState();
    try common.expectEqual(usize, rt.objectCount(), 1, "suspended continuation keeps rooted payload alive");
    const suspended_explain = try rt.explainValue(rooted, &trace);
    try common.expect(suspended_explain.control_roots >= 1, "captured payload should be control-rooted while suspended");

    const resumed = try rt.resumeContinuation(performed.continuation, Value.fromInt(99));
    try common.expect(common.sameHandle(resumed.fiber, child), "resume should reactivate the suspended fiber");
    try common.expectEqual(i64, resumed.value.asInt(), 99, "resume payload should round-trip");
    try common.expect(common.sameHandle(rt.currentFiber(), child), "current fiber should switch to the resumed fiber");
    try common.expect(rt.dropContinuation(performed.continuation), "dropping resumed continuation should succeed");

    _ = try rt.popFiberFrame(child);
    rt.collectMajor();
    try rt.verifyDebugState();
    try common.expectEqual(usize, rt.objectCount(), 0, "resumed stack roots should release after frame pop");

    const smoke_delta = EventCounters.diff(trace.snapshot(), smoke_before);
    try common.expect(smoke_delta.continuation_captures >= 1, "effect smoke should capture a continuation");
    try common.expect(smoke_delta.continuation_resumes >= 1, "effect smoke should resume a continuation");
    try common.expect(smoke_delta.fiber_activations >= 2, "effect smoke should switch fibers");

    var bench = Runtime.init(allocator);
    defer bench.deinit();
    const bench_main = bench.currentFiber();
    try bench.pushEffectHandler(bench_main, .{
        .effect = effect,
        .handle_effect = Value.fromInt(1),
    });
    defer _ = bench.popEffectHandler(bench_main) catch {};

    const bench_iters: usize = 64;
    var timer = try std.time.Timer.start();
    var i: usize = 0;
    while (i < bench_iters) : (i += 1) {
        try bench.pushFiberFrame(bench_main, @intCast(10_000 + i));
        const payload = try bench.allocTuple(1);
        try bench.setField(payload, 0, Value.fromInt(@intCast(i)));
        try bench.pushFiberFrameRoot(bench_main, payload);
        const loop_performed = try bench.performEffectAt(@intCast(10_000 + i), effect, payload, &.{payload});
        _ = try bench.resumeContinuation(loop_performed.continuation, Value.fromInt(@intCast(i)));
        try common.expect(bench.dropContinuation(loop_performed.continuation), "bench continuation drop should succeed");
        _ = try bench.popFiberFrame(bench_main);
    }
    const ns_per_op = @as(f64, @floatFromInt(timer.read())) / @as(f64, @floatFromInt(bench_iters));

    std.debug.print(
        "e2e effects-roundtrip-smoke ok output=99 trace_capture={d} trace_resume={d} bench_ns_per_op={d:.2}\n",
        .{
            smoke_delta.continuation_captures,
            smoke_delta.continuation_resumes,
            ns_per_op,
        },
    );
}
