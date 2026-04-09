const builtin = @import("builtin");
const std = @import("std");

fn rootBuildOption(comptime name: []const u8) bool {
    const root = @import("root");
    if (!@hasDecl(root, "build_options")) return false;
    const options = root.build_options;
    if (!@hasDecl(options, name)) return false;
    return @field(options, name);
}

/// Compile-time reductions selected by the build. These may only subtract
/// capability from the target profile.
pub const BuildCaps = struct {
    disable_threads: bool = false,
    disable_filesystem: bool = false,
    disable_network: bool = false,
    disable_environment: bool = false,
    disable_subprocesses: bool = false,
    disable_blocking_syscalls: bool = false,
    disable_posix_signals: bool = false,
    disable_alternate_signal_stack: bool = false,
    disable_native_plugin_loading: bool = false,
    disable_monotonic_clock: bool = false,

    pub fn fromRoot() BuildCaps {
        return .{
            .disable_threads = rootBuildOption("disable_threads"),
            .disable_filesystem = rootBuildOption("disable_filesystem"),
            .disable_network = rootBuildOption("disable_network"),
            .disable_environment = rootBuildOption("disable_environment"),
            .disable_subprocesses = rootBuildOption("disable_subprocesses"),
            .disable_blocking_syscalls = rootBuildOption("disable_blocking_syscalls"),
            .disable_posix_signals = rootBuildOption("disable_posix_signals"),
            .disable_alternate_signal_stack = rootBuildOption("disable_alternate_signal_stack"),
            .disable_native_plugin_loading = rootBuildOption("disable_native_plugin_loading"),
            .disable_monotonic_clock = rootBuildOption("disable_monotonic_clock"),
        };
    }
};

/// The compiled host capability profile for the current target/build pair.
/// This is a compile-time boundary, not a userland request surface.
pub const PlatformCaps = struct {
    os: std.Target.Os.Tag,
    threads: bool,
    stop_the_world: bool,
    monotonic_clock: bool,
    filesystem: bool,
    network: bool,
    environment: bool,
    subprocesses: bool,
    blocking_syscalls: bool,
    posix_signals: bool,
    alternate_signal_stack: bool,
    native_plugin_loading: bool,

    pub fn target() PlatformCaps {
        const os = builtin.os.tag;
        const is_unix_signal_target = switch (os) {
            .linux,
            .macos,
            .ios,
            .tvos,
            .watchos,
            .visionos,
            .freebsd,
            .netbsd,
            .openbsd,
            .dragonfly,
            => true,
            else => false,
        };

        const has_threads = switch (os) {
            .wasi => false,
            else => true,
        };

        const has_processes = switch (os) {
            .wasi => false,
            else => true,
        };

        const has_plugin_loading = switch (os) {
            .wasi => false,
            else => true,
        };

        return .{
            .os = os,
            .threads = has_threads,
            .stop_the_world = has_threads,
            .monotonic_clock = true,
            .filesystem = os != .freestanding,
            .network = os != .freestanding and os != .wasi,
            .environment = os != .freestanding,
            .subprocesses = has_processes,
            .blocking_syscalls = os != .freestanding and os != .wasi,
            .posix_signals = is_unix_signal_target,
            .alternate_signal_stack = is_unix_signal_target,
            .native_plugin_loading = has_plugin_loading,
        };
    }

    pub fn applyBuildCaps(self: PlatformCaps, build_caps: BuildCaps) PlatformCaps {
        var caps = self;
        if (build_caps.disable_threads) {
            caps.threads = false;
            caps.stop_the_world = false;
        }
        if (build_caps.disable_filesystem) caps.filesystem = false;
        if (build_caps.disable_network) caps.network = false;
        if (build_caps.disable_environment) caps.environment = false;
        if (build_caps.disable_subprocesses) caps.subprocesses = false;
        if (build_caps.disable_blocking_syscalls) caps.blocking_syscalls = false;
        if (build_caps.disable_posix_signals) {
            caps.posix_signals = false;
            caps.alternate_signal_stack = false;
        }
        if (build_caps.disable_alternate_signal_stack) caps.alternate_signal_stack = false;
        if (build_caps.disable_native_plugin_loading) caps.native_plugin_loading = false;
        if (build_caps.disable_monotonic_clock) caps.monotonic_clock = false;
        return caps;
    }

    pub fn detect() PlatformCaps {
        return PlatformCaps.target().applyBuildCaps(BuildCaps.fromRoot());
    }
};

/// Userland policy for host access. Permissions may only narrow what the
/// compiled build already supports.
pub const RuntimePermissions = struct {
    allow_all: bool = false,
    allow_read: bool = false,
    allow_write: bool = false,
    allow_net: bool = false,
    allow_env: bool = false,
    allow_run: bool = false,
    allow_ffi: bool = false,
    allow_hrtime: bool = false,

    pub fn normalized(self: RuntimePermissions) RuntimePermissions {
        if (!self.allow_all) return self;
        return .{
            .allow_all = true,
            .allow_read = true,
            .allow_write = true,
            .allow_net = true,
            .allow_env = true,
            .allow_run = true,
            .allow_ffi = true,
            .allow_hrtime = true,
        };
    }
};

/// The effective runtime-visible host access after intersecting compiled caps
/// with runtime permissions.
pub const HostAccess = struct {
    read: bool,
    write: bool,
    net: bool,
    env: bool,
    run: bool,
    ffi: bool,
    hrtime: bool,

    pub fn from(caps: PlatformCaps, permissions: RuntimePermissions) HostAccess {
        const normalized = permissions.normalized();
        return .{
            .read = caps.filesystem and normalized.allow_read,
            .write = caps.filesystem and normalized.allow_write,
            .net = caps.network and normalized.allow_net,
            .env = caps.environment and normalized.allow_env,
            .run = caps.subprocesses and normalized.allow_run,
            .ffi = caps.native_plugin_loading and normalized.allow_ffi,
            .hrtime = caps.monotonic_clock and normalized.allow_hrtime,
        };
    }
};

test "platform_caps: detected target capabilities are internally consistent" {
    const caps = PlatformCaps.detect();

    if (!caps.threads) {
        try std.testing.expect(!caps.stop_the_world);
    }
    if (!caps.posix_signals) {
        try std.testing.expect(!caps.alternate_signal_stack);
    }
}

test "platform_caps: build caps only subtract platform capability" {
    const target = PlatformCaps{
        .os = .linux,
        .threads = true,
        .stop_the_world = true,
        .monotonic_clock = true,
        .filesystem = true,
        .network = true,
        .environment = true,
        .subprocesses = true,
        .blocking_syscalls = true,
        .posix_signals = true,
        .alternate_signal_stack = true,
        .native_plugin_loading = true,
    };

    const caps = target.applyBuildCaps(.{
        .disable_threads = true,
        .disable_network = true,
        .disable_posix_signals = true,
        .disable_native_plugin_loading = true,
    });

    try std.testing.expect(!caps.threads);
    try std.testing.expect(!caps.stop_the_world);
    try std.testing.expect(!caps.network);
    try std.testing.expect(!caps.posix_signals);
    try std.testing.expect(!caps.alternate_signal_stack);
    try std.testing.expect(!caps.native_plugin_loading);
    try std.testing.expect(caps.filesystem);
}

test "platform_caps: allow_all enables all host access that the platform supports" {
    const caps = PlatformCaps{
        .os = .linux,
        .threads = true,
        .stop_the_world = true,
        .monotonic_clock = true,
        .filesystem = true,
        .network = true,
        .environment = true,
        .subprocesses = true,
        .blocking_syscalls = true,
        .posix_signals = true,
        .alternate_signal_stack = true,
        .native_plugin_loading = true,
    };

    const access = HostAccess.from(caps, .{ .allow_all = true });
    try std.testing.expect(access.read);
    try std.testing.expect(access.write);
    try std.testing.expect(access.net);
    try std.testing.expect(access.env);
    try std.testing.expect(access.run);
    try std.testing.expect(access.ffi);
    try std.testing.expect(access.hrtime);
}

test "platform_caps: permissions cannot exceed platform capability" {
    const caps = PlatformCaps{
        .os = .wasi,
        .threads = false,
        .stop_the_world = false,
        .monotonic_clock = true,
        .filesystem = true,
        .network = false,
        .environment = false,
        .subprocesses = false,
        .blocking_syscalls = false,
        .posix_signals = false,
        .alternate_signal_stack = false,
        .native_plugin_loading = false,
    };

    const access = HostAccess.from(caps, .{ .allow_all = true });
    try std.testing.expect(access.read);
    try std.testing.expect(access.write);
    try std.testing.expect(!access.net);
    try std.testing.expect(!access.env);
    try std.testing.expect(!access.run);
    try std.testing.expect(!access.ffi);
    try std.testing.expect(access.hrtime);
}
