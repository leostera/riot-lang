const std = @import("std");
const runtime = @import("runtime.zig");

const Runtime = runtime.Runtime;
const Value = runtime.Value;
const FiberHandle = runtime.FiberHandle;

test "stress: nested perform and reperform keep suspended roots alive across gc" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const effect: runtime.EffectId = 77;
    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = effect,
        .handle_effect = Value.fromInt(1),
    });
    defer _ = rt.popEffectHandler(main) catch {};

    const parent = try rt.spawnFiberInDomain(main, rt.currentDomain());
    try rt.pushEffectHandler(parent, .{
        .effect = effect,
        .handle_effect = Value.fromInt(2),
    });
    defer _ = rt.popEffectHandler(parent) catch {};

    const child = try rt.spawnFiberInDomain(parent, rt.currentDomain());
    try rt.activateFiberInDomain(rt.currentDomain(), child);

    const child_root = try rt.allocTuple(0);
    try rt.pushFiberFrame(child, 1001);
    try rt.pushFiberFrameRoot(child, child_root);

    const inner = try rt.performEffectAt(1001, effect, Value.fromInt(10), &.{child_root});
    try std.testing.expectEqual(parent, inner.handler_fiber);

    const parent_root = try rt.allocTuple(0);
    try rt.pushFiberFrame(parent, 1002);
    try rt.pushFiberFrameRoot(parent, parent_root);

    const outer = try rt.reperformEffectAt(1002, effect, Value.fromInt(11), &.{parent_root});
    try std.testing.expectEqual(main, outer.handler_fiber);

    rt.collectMajor();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
    try rt.verifyDebugState();

    _ = try rt.resumeContinuation(outer.continuation, Value.fromInt(20));
    try std.testing.expectEqual(parent, rt.currentFiber());
    _ = try rt.resumeContinuation(inner.continuation, Value.fromInt(21));
    try std.testing.expectEqual(child, rt.currentFiber());
    try std.testing.expect(rt.dropContinuation(outer.continuation));
    try std.testing.expect(rt.dropContinuation(inner.continuation));

    _ = try rt.popFiberFrame(parent);
    _ = try rt.popFiberFrame(child);
    rt.collectMajor();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    try rt.verifyDebugState();
}

test "stress: parked and suspended fibers stay live through scheduler churn and repeated gc" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
        .debugChecks = .{
            .verify_control_kernel = true,
            .verify_roots = true,
        },
    });
    defer rt.deinit();

    const effect: runtime.EffectId = 9;
    const main_domain = rt.currentDomain();
    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = effect,
        .handle_effect = Value.fromInt(1),
    });
    defer _ = rt.popEffectHandler(main) catch {};

    const parked = try rt.spawnFiberInDomain(main, main_domain);
    const worker = try rt.spawnFiberInDomain(main, main_domain);

    try std.testing.expectEqual(parked, (try rt.scheduleNextFiber(main_domain)).?);
    const parked_root = try rt.allocTuple(0);
    try rt.pushFiberFrame(parked, 2001);
    try rt.pushFiberFrameRoot(parked, parked_root);

    try std.testing.expectEqual(worker, (try rt.parkCurrentFiber()).?);
    const suspended_root = try rt.allocTuple(0);
    try rt.pushFiberFrame(worker, 2002);
    try rt.pushFiberFrameRoot(worker, suspended_root);

    const performed = try rt.performEffectAt(2002, effect, Value.fromInt(4), &.{suspended_root});
    try std.testing.expectEqual(main, performed.handler_fiber);

    var pass: usize = 0;
    while (pass < 4) : (pass += 1) {
        rt.collectMinor();
        rt.collectMajor();
        try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
        try rt.verifyDebugState();
    }

    try rt.unparkFiber(main_domain, parked);
    const switched = try rt.scheduleNextFiber(main_domain);
    try std.testing.expect(switched != null);
    try std.testing.expect(rt.fiberScheduler().parkedCount(main_domain) == 0);

    _ = try rt.popFiberFrame(parked);
    try rt.activateFiberInDomain(main_domain, main);
    _ = try rt.resumeContinuation(performed.continuation, Value.fromInt(5));
    try std.testing.expect(rt.dropContinuation(performed.continuation));
    _ = try rt.popFiberFrame(worker);

    rt.collectMajor();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    try rt.verifyDebugState();
}

test "stress: cross-domain continuation resume stays valid while domains attach and detach" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .debugChecks = .{
            .verify_control_kernel = true,
        },
    });
    defer rt.deinit();

    const effect: runtime.EffectId = 12;
    const main_domain = rt.currentDomain();
    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = effect,
        .handle_effect = Value.fromInt(1),
    });
    defer _ = rt.popEffectHandler(main) catch {};

    const resume_domain = try rt.createDomain();
    const churn_domain = try rt.createDomain();
    try rt.attachDomain(resume_domain);
    try std.testing.expect(try rt.startDomainWorker(resume_domain, 44));

    const worker = try rt.spawnFiberInDomain(null, resume_domain);

    var iteration: usize = 0;
    while (iteration < 8) : (iteration += 1) {
        try rt.activateFiberInDomain(main_domain, main);
        const child = try rt.spawnFiberInDomain(main, main_domain);
        try rt.activateFiberInDomain(main_domain, child);

        const rooted = try rt.allocTuple(0);
        try rt.pushFiberFrame(child, @intCast(3000 + iteration));
        try rt.pushFiberFrameRoot(child, rooted);

        const performed = try rt.performEffectAt(@intCast(3000 + iteration), effect, Value.fromInt(@intCast(iteration)), &.{rooted});
        try std.testing.expectEqual(main, performed.handler_fiber);

        try rt.activateFiberInDomain(resume_domain, worker);
        _ = try rt.resumeContinuation(performed.continuation, Value.fromInt(@intCast(iteration * 2)));
        try std.testing.expectEqual(resume_domain, rt.currentDomain());
        try std.testing.expectEqual(resume_domain, rt.controlKernel().fiber(child).?.domain);
        try std.testing.expect(rt.dropContinuation(performed.continuation));

        if ((iteration & 1) == 0) {
            try rt.attachDomain(churn_domain);
            try rt.verifyDebugState();
            try rt.detachDomain(churn_domain);
        } else {
            try rt.attachDomain(churn_domain);
            rt.collectMajor();
            try rt.detachDomain(churn_domain);
        }

        _ = try rt.popFiberFrame(child);
        rt.collectMajor();
        try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
        try rt.verifyDebugState();
    }
}
