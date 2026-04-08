const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib_module = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    const zort_lib = b.addLibrary(.{
        .name = "zort",
        .root_module = lib_module,
        .linkage = .static,
    });

    b.installArtifact(zort_lib);

    const tests = b.addTest(.{
        .root_module = lib_module,
    });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run zort tests");
    test_step.dependOn(&run_tests.step);

    const bench_module = b.createModule(.{
        .root_source_file = b.path("src/bench.zig"),
        .target = target,
        .optimize = optimize,
    });

    const bench = b.addExecutable(.{
        .name = "zort-bench",
        .root_module = bench_module,
    });
    const run_bench = b.addRunArtifact(bench);
    const bench_step = b.step("bench", "Run zort benchmarks");
    bench_step.dependOn(&run_bench.step);

    _ = b.addModule("zort", .{
        .root_source_file = b.path("src/lib.zig"),
    });
}
