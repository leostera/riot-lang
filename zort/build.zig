const std = @import("std");

const E2eCase = struct {
    name: []const u8,
    path: []const u8,
};

const e2e_cases = [_]E2eCase{
    .{ .name = "zort-e2e-alloc-gc-smoke", .path = "e2e/alloc_gc_smoke.zig" },
    .{ .name = "zort-e2e-effects-roundtrip-smoke", .path = "e2e/effects_roundtrip_smoke.zig" },
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const compat_shim_enabled = b.option(bool, "compat-shim", "Build the legacy compatibility shim") orelse true;
    const disable_threads = b.option(bool, "disable-threads", "Compile zort without thread/domain worker capabilities") orelse false;
    const disable_filesystem = b.option(bool, "disable-filesystem", "Compile zort without filesystem-backed host capabilities") orelse false;
    const disable_network = b.option(bool, "disable-network", "Compile zort without network host capabilities") orelse false;
    const disable_environment = b.option(bool, "disable-environment", "Compile zort without environment variable host capabilities") orelse false;
    const disable_subprocesses = b.option(bool, "disable-subprocesses", "Compile zort without subprocess host capabilities") orelse false;
    const disable_blocking_syscalls = b.option(bool, "disable-blocking-syscalls", "Compile zort without blocking syscall host capabilities") orelse false;
    const disable_posix_signals = b.option(bool, "disable-posix-signals", "Compile zort without POSIX signal ingress support") orelse false;
    const disable_alternate_signal_stack = b.option(bool, "disable-alternate-signal-stack", "Compile zort without alternate signal-stack support") orelse false;
    const disable_native_plugin_loading = b.option(bool, "disable-native-plugin-loading", "Compile zort without native plugin loading support") orelse false;
    const disable_monotonic_clock = b.option(bool, "disable-monotonic-clock", "Compile zort without monotonic clock support") orelse false;

    const runtime_options = b.addOptions();
    runtime_options.addOption(bool, "compat_shim_enabled", compat_shim_enabled);
    runtime_options.addOption(bool, "disable_threads", disable_threads);
    runtime_options.addOption(bool, "disable_filesystem", disable_filesystem);
    runtime_options.addOption(bool, "disable_network", disable_network);
    runtime_options.addOption(bool, "disable_environment", disable_environment);
    runtime_options.addOption(bool, "disable_subprocesses", disable_subprocesses);
    runtime_options.addOption(bool, "disable_blocking_syscalls", disable_blocking_syscalls);
    runtime_options.addOption(bool, "disable_posix_signals", disable_posix_signals);
    runtime_options.addOption(bool, "disable_alternate_signal_stack", disable_alternate_signal_stack);
    runtime_options.addOption(bool, "disable_native_plugin_loading", disable_native_plugin_loading);
    runtime_options.addOption(bool, "disable_monotonic_clock", disable_monotonic_clock);

    const lib_module = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib_module.addOptions("build_options", runtime_options);
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
        const compat_module = b.createModule(.{
            .root_source_file = b.path("src/api.zig"),
            .target = target,
            .optimize = optimize,
        });
        compat_module.addOptions("build_options", runtime_options);

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
    bench_module.addOptions("build_options", runtime_options);

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

    const e2e_step = b.step("e2e", "Run zort end-to-end smoke programs");
    for (e2e_cases) |case| {
        const case_module = b.createModule(.{
            .root_source_file = b.path(case.path),
            .target = target,
            .optimize = optimize,
        });
        case_module.addImport("zort", lib_module);
        const exe = b.addExecutable(.{
            .name = case.name,
            .root_module = case_module,
        });
        const run_case = b.addRunArtifact(exe);
        e2e_step.dependOn(&run_case.step);
        test_step.dependOn(&run_case.step);
    }

    const installed_module = b.addModule("zort", .{
        .root_source_file = b.path("src/lib.zig"),
    });
    installed_module.addOptions("build_options", runtime_options);
}
