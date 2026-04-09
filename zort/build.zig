const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const compat_shim_enabled = b.option(bool, "compat-shim", "Build the legacy compatibility shim") orelse true;

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

    if (compat_shim_enabled) {
        const compat_options = b.addOptions();
        compat_options.addOption(bool, "compat_shim_enabled", compat_shim_enabled);

        const compat_module = b.createModule(.{
            .root_source_file = b.path("src/api.zig"),
            .target = target,
            .optimize = optimize,
        });
        compat_module.addOptions("build_options", compat_options);

        const compat_lib = b.addLibrary(.{
            .name = "zort-compat",
            .root_module = compat_module,
            .linkage = .static,
        });
        b.installArtifact(compat_lib);

        const compat_tests = b.addTest(.{
            .root_module = compat_module,
        });
        const run_compat_tests = b.addRunArtifact(compat_tests);
        test_step.dependOn(&run_compat_tests.step);

        const compat_step = b.step("compat", "Build zort compatibility shim");
        compat_step.dependOn(&compat_lib.step);
    }

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
    if (b.args) |args| {
        run_bench.addArgs(args);
    }
    const bench_step = b.step("bench", "Run zort benchmarks");
    bench_step.dependOn(&run_bench.step);

    _ = b.addModule("zort", .{
        .root_source_file = b.path("src/lib.zig"),
    });
}
