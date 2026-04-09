const std = @import("std");
const platform_caps_mod = @import("platform_caps.zig");
const root_provider = @import("root_provider.zig");
const value = @import("value.zig");

pub const PlatformCaps = platform_caps_mod.PlatformCaps;
pub const RuntimePermissions = platform_caps_mod.RuntimePermissions;
pub const HostAccess = platform_caps_mod.HostAccess;
pub const RootProvider = root_provider.RootProvider;
pub const RootVisitor = root_provider.RootVisitor;
pub const Value = value.Value;

const compiled_platform_caps = PlatformCaps.detect();

pub const supports_native_signal_ingress = compiled_platform_caps.posix_signals;

pub const SignalIngressSnapshot = struct {
    installed: bool,
    installed_signals: u64,
    owns_alternate_stack: bool,
    alternate_stack_size: usize,
    restored_foreign_stack: bool,
    named_value_count: usize,
    registered_signal_handlers: usize,
};

const PosixSignalRuntime = if (compiled_platform_caps.posix_signals) struct {
    const GlobalSignalBridge = struct {
        mutex: std.Thread.Mutex = .{},
        signal_owner: ?*RuntimeServices = null,
        installed_mask: u64 = 0,
        previous_actions: [64]?std.posix.Sigaction = [_]?std.posix.Sigaction{null} ** 64,
        alt_stack_owner: ?*RuntimeServices = null,
        alt_stack_previous: ?std.posix.stack_t = null,
        alt_stack_memory: ?[]u8 = null,
        restored_foreign_stack: bool = false,
    };

    var global_signal_bridge: GlobalSignalBridge = .{};
    var global_signal_owner_ptr = std.atomic.Value(usize).init(0);

    fn signalIngressHandler(sig: i32) callconv(.c) void {
        if (sig < 0 or sig >= 64) return;
        const raw_owner = global_signal_owner_ptr.load(.acquire);
        if (raw_owner == 0) return;
        const owner: *RuntimeServices = @ptrFromInt(raw_owner);
        owner.recordSignalFromHandler(@intCast(sig));
    }

    fn runtimeSignalAction(use_onstack: bool) std.posix.Sigaction {
        var action: std.posix.Sigaction = .{
            .handler = .{ .handler = signalIngressHandler },
            .mask = std.posix.sigemptyset(),
            .flags = std.posix.SA.RESTART,
        };
        if (use_onstack) action.flags |= std.posix.SA.ONSTACK;
        return action;
    }

    fn defaultSignalStackSize() usize {
        const preferred = std.math.cast(usize, std.c.SIGSTKSZ) orelse 8192;
        const minimum = std.math.cast(usize, std.c.MINSIGSTKSZ) orelse preferred;
        return @max(preferred, minimum);
    }

    fn disabledAltStack() std.posix.stack_t {
        var stack: std.posix.stack_t = undefined;
        @field(stack, "sp") = @ptrFromInt(@as(usize, 1));
        @field(stack, "size") = @as(@TypeOf(@field(stack, "size")), @intCast(defaultSignalStackSize()));
        @field(stack, "flags") = std.c.SS.DISABLE;
        return stack;
    }

    fn enabledAltStack(memory: []u8) std.posix.stack_t {
        var stack: std.posix.stack_t = undefined;
        @field(stack, "sp") = memory.ptr;
        @field(stack, "size") = @as(@TypeOf(@field(stack, "size")), @intCast(memory.len));
        @field(stack, "flags") = 0;
        return stack;
    }

    fn altStackWasEnabled(stack: std.posix.stack_t) bool {
        return (@field(stack, "flags") & std.c.SS.DISABLE) == 0 and @field(stack, "size") != 0;
    }
} else struct {};

fn platformDefaultSignalStackSize() usize {
    if (comptime compiled_platform_caps.alternate_signal_stack) {
        return PosixSignalRuntime.defaultSignalStackSize();
    }
    return 0;
}

pub const RuntimeServices = struct {
    allocator: std.mem.Allocator,
    compiled_caps: PlatformCaps,
    runtime_permissions: RuntimePermissions,
    host_access: HostAccess,
    state_lock: std.Thread.Mutex = .{},
    startup_depth: usize = 0,
    was_shutdown: bool = false,
    pending_signals: std.atomic.Value(u64) = std.atomic.Value(u64).init(0),
    blocking_sections: usize = 0,
    named_values: std.ArrayListUnmanaged(NamedValue) = .{},
    signal_handlers: [64]?Value = [_]?Value{null} ** 64,

    pub const Error = error{
        RuntimeAlreadyShutdown,
        ShutdownWithoutStartup,
        BlockingSectionUnderflow,
        UnsupportedSignal,
        OutOfMemory,
        UnsupportedPlatform,
        SignalIngressBusy,
        SignalIngressNotInstalled,
        AlternateSignalStackBusy,
        AlternateSignalStackNotInstalled,
    } || std.posix.SigaltstackError || std.posix.RaiseError;

    pub const NamedValue = struct {
        name: []u8,
        value: Value,
    };

    pub fn init(allocator: std.mem.Allocator, compiled_caps: PlatformCaps, runtime_permissions: RuntimePermissions) RuntimeServices {
        return .{
            .allocator = allocator,
            .compiled_caps = compiled_caps,
            .runtime_permissions = runtime_permissions.normalized(),
            .host_access = HostAccess.from(compiled_caps, runtime_permissions),
        };
    }

    pub fn platformCaps(self: *const RuntimeServices) PlatformCaps {
        return self.compiled_caps;
    }

    pub fn permissions(self: *const RuntimeServices) RuntimePermissions {
        return self.runtime_permissions;
    }

    pub fn hostAccess(self: *const RuntimeServices) HostAccess {
        return self.host_access;
    }

    pub fn deinit(self: *RuntimeServices) void {
        self.disableAllSignalIngress() catch {};
        self.disableAlternateSignalStack() catch {};

        self.state_lock.lock();
        defer self.state_lock.unlock();
        for (self.named_values.items) |entry| self.allocator.free(entry.name);
        self.named_values.deinit(self.allocator);
    }

    pub fn provider(self: *RuntimeServices) RootProvider {
        return .{
            .name = "runtime_services",
            .ctx = self,
            .count_fn = countRoots,
            .visit_fn = visitRoots,
        };
    }

    pub fn ownerCount(self: *const RuntimeServices, needle: Value) usize {
        const mutable: *RuntimeServices = @constCast(self);
        mutable.state_lock.lock();
        defer mutable.state_lock.unlock();

        var count: usize = 0;
        for (mutable.named_values.items) |entry| {
            if (entry.value.isBlock() and std.meta.eql(entry.value, needle)) count += 1;
        }
        for (mutable.signal_handlers) |handler| {
            if (handler) |value_ref| {
                if (value_ref.isBlock() and std.meta.eql(value_ref, needle)) count += 1;
            }
        }
        return count;
    }

    pub fn startup(self: *RuntimeServices) Error!void {
        self.state_lock.lock();
        defer self.state_lock.unlock();
        if (self.was_shutdown and self.startup_depth == 0) return error.RuntimeAlreadyShutdown;
        self.startup_depth +%= 1;
    }

    pub fn shutdown(self: *RuntimeServices) Error!void {
        self.state_lock.lock();
        defer self.state_lock.unlock();
        if (self.startup_depth == 0) return error.ShutdownWithoutStartup;
        self.startup_depth -= 1;
        if (self.startup_depth == 0) self.was_shutdown = true;
    }

    pub fn isStarted(self: *const RuntimeServices) bool {
        const mutable: *RuntimeServices = @constCast(self);
        mutable.state_lock.lock();
        defer mutable.state_lock.unlock();
        return mutable.startup_depth > 0;
    }

    pub fn enterBlockingSection(self: *RuntimeServices) void {
        self.state_lock.lock();
        defer self.state_lock.unlock();
        self.blocking_sections +%= 1;
    }

    pub fn exitBlockingSection(self: *RuntimeServices) Error!void {
        self.state_lock.lock();
        defer self.state_lock.unlock();
        if (self.blocking_sections == 0) return error.BlockingSectionUnderflow;
        self.blocking_sections -= 1;
    }

    pub fn blockingDepth(self: *const RuntimeServices) usize {
        const mutable: *RuntimeServices = @constCast(self);
        mutable.state_lock.lock();
        defer mutable.state_lock.unlock();
        return mutable.blocking_sections;
    }

    pub fn pendingSignalBits(self: *const RuntimeServices) u64 {
        return self.pending_signals.load(.acquire);
    }

    pub fn hasPendingSignals(self: *const RuntimeServices) bool {
        return self.pendingSignalBits() != 0;
    }

    pub fn nextPendingSignal(self: *const RuntimeServices) ?u8 {
        const bits = self.pendingSignalBits();
        if (bits == 0) return null;
        return @intCast(@ctz(bits));
    }

    pub fn recordSignal(self: *RuntimeServices, signo: u8) Error!void {
        if (signo >= 64) return error.UnsupportedSignal;
        self.recordSignalFromHandler(signo);
    }

    pub fn clearPendingSignal(self: *RuntimeServices, signo: u8) Error!bool {
        if (signo >= 64) return error.UnsupportedSignal;
        const bit = (@as(u64, 1) << @intCast(signo));
        var current = self.pending_signals.load(.acquire);
        while (true) {
            if ((current & bit) == 0) return false;
            const next = current & ~bit;
            if (self.pending_signals.cmpxchgWeak(current, next, .acq_rel, .acquire) == null) return true;
            current = self.pending_signals.load(.acquire);
        }
    }

    pub fn takePendingSignals(self: *RuntimeServices) u64 {
        return self.pending_signals.swap(0, .acq_rel);
    }

    pub fn registerSignalHandler(self: *RuntimeServices, signo: u8, handler: Value) Error!void {
        if (signo >= self.signal_handlers.len) return error.UnsupportedSignal;
        self.state_lock.lock();
        defer self.state_lock.unlock();
        self.signal_handlers[signo] = handler;
    }

    pub fn unregisterSignalHandler(self: *RuntimeServices, signo: u8) Error!void {
        if (signo >= self.signal_handlers.len) return error.UnsupportedSignal;
        self.state_lock.lock();
        defer self.state_lock.unlock();
        self.signal_handlers[signo] = null;
    }

    pub fn lookupSignalHandler(self: *const RuntimeServices, signo: u8) ?Value {
        if (signo >= self.signal_handlers.len) return null;
        const mutable: *RuntimeServices = @constCast(self);
        mutable.state_lock.lock();
        defer mutable.state_lock.unlock();
        return mutable.signal_handlers[signo];
    }

    pub fn registerNamedValue(self: *RuntimeServices, name: []const u8, val: Value) Error!void {
        self.state_lock.lock();
        defer self.state_lock.unlock();

        for (self.named_values.items) |*entry| {
            if (std.mem.eql(u8, entry.name, name)) {
                entry.value = val;
                return;
            }
        }

        try self.named_values.append(self.allocator, .{
            .name = try self.allocator.dupe(u8, name),
            .value = val,
        });
    }

    pub fn lookupNamedValue(self: *const RuntimeServices, name: []const u8) ?Value {
        const mutable: *RuntimeServices = @constCast(self);
        mutable.state_lock.lock();
        defer mutable.state_lock.unlock();
        for (mutable.named_values.items) |entry| {
            if (std.mem.eql(u8, entry.name, name)) return entry.value;
        }
        return null;
    }

    pub fn installSignalIngress(self: *RuntimeServices, signo: u8) Error!void {
        if (comptime compiled_platform_caps.posix_signals) {
            if (!self.compiled_caps.posix_signals) return error.UnsupportedPlatform;
            if (signo >= 64) return error.UnsupportedSignal;

            PosixSignalRuntime.global_signal_bridge.mutex.lock();
            defer PosixSignalRuntime.global_signal_bridge.mutex.unlock();

            if (PosixSignalRuntime.global_signal_bridge.signal_owner) |owner| {
                if (owner != self) return error.SignalIngressBusy;
            } else {
                PosixSignalRuntime.global_signal_bridge.signal_owner = self;
                PosixSignalRuntime.global_signal_owner_ptr.store(@intFromPtr(self), .release);
            }

            const bit = (@as(u64, 1) << @intCast(signo));
            if ((PosixSignalRuntime.global_signal_bridge.installed_mask & bit) != 0) return;

            const act = PosixSignalRuntime.runtimeSignalAction(PosixSignalRuntime.global_signal_bridge.alt_stack_owner == self);
            var previous: std.posix.Sigaction = undefined;
            std.posix.sigaction(signo, &act, &previous);
            PosixSignalRuntime.global_signal_bridge.previous_actions[signo] = previous;
            PosixSignalRuntime.global_signal_bridge.installed_mask |= bit;
        } else {
            return error.UnsupportedPlatform;
        }
    }

    pub fn uninstallSignalIngress(self: *RuntimeServices, signo: u8) Error!bool {
        if (comptime compiled_platform_caps.posix_signals) {
            if (!self.compiled_caps.posix_signals) return error.UnsupportedPlatform;
            if (signo >= 64) return error.UnsupportedSignal;

            PosixSignalRuntime.global_signal_bridge.mutex.lock();
            defer PosixSignalRuntime.global_signal_bridge.mutex.unlock();

            if (PosixSignalRuntime.global_signal_bridge.signal_owner != self) return error.SignalIngressNotInstalled;

            const bit = (@as(u64, 1) << @intCast(signo));
            if ((PosixSignalRuntime.global_signal_bridge.installed_mask & bit) == 0) return false;

            if (PosixSignalRuntime.global_signal_bridge.previous_actions[signo]) |previous| {
                std.posix.sigaction(signo, &previous, null);
            }
            PosixSignalRuntime.global_signal_bridge.previous_actions[signo] = null;
            PosixSignalRuntime.global_signal_bridge.installed_mask &= ~bit;

            if (PosixSignalRuntime.global_signal_bridge.installed_mask == 0) {
                PosixSignalRuntime.global_signal_bridge.signal_owner = null;
                PosixSignalRuntime.global_signal_owner_ptr.store(0, .release);
            }
            return true;
        } else {
            return error.UnsupportedPlatform;
        }
    }

    pub fn disableAllSignalIngress(self: *RuntimeServices) Error!void {
        if (comptime compiled_platform_caps.posix_signals) {
            if (!self.compiled_caps.posix_signals) return;

            PosixSignalRuntime.global_signal_bridge.mutex.lock();
            defer PosixSignalRuntime.global_signal_bridge.mutex.unlock();

            if (PosixSignalRuntime.global_signal_bridge.signal_owner != self) return;

            for (0..64) |signo| {
                const bit = (@as(u64, 1) << @intCast(signo));
                if ((PosixSignalRuntime.global_signal_bridge.installed_mask & bit) == 0) continue;
                if (PosixSignalRuntime.global_signal_bridge.previous_actions[signo]) |previous| {
                    std.posix.sigaction(@intCast(signo), &previous, null);
                }
                PosixSignalRuntime.global_signal_bridge.previous_actions[signo] = null;
            }
            PosixSignalRuntime.global_signal_bridge.installed_mask = 0;
            PosixSignalRuntime.global_signal_bridge.signal_owner = null;
            PosixSignalRuntime.global_signal_owner_ptr.store(0, .release);
        } else {
            return;
        }
    }

    pub fn enableAlternateSignalStack(self: *RuntimeServices, requested_size: ?usize) Error!void {
        if (comptime compiled_platform_caps.alternate_signal_stack) {
            if (!self.compiled_caps.alternate_signal_stack) return error.UnsupportedPlatform;

            PosixSignalRuntime.global_signal_bridge.mutex.lock();
            defer PosixSignalRuntime.global_signal_bridge.mutex.unlock();

            if (PosixSignalRuntime.global_signal_bridge.alt_stack_owner) |owner| {
                if (owner != self) return error.AlternateSignalStackBusy;
                return;
            }

            const size = requested_size orelse platformDefaultSignalStackSize();
            const memory = try self.allocator.alloc(u8, size);
            errdefer self.allocator.free(memory);

            var previous: std.posix.stack_t = undefined;
            try std.posix.sigaltstack(null, &previous);
            var next = PosixSignalRuntime.enabledAltStack(memory);
            try std.posix.sigaltstack(&next, null);

            PosixSignalRuntime.global_signal_bridge.alt_stack_owner = self;
            PosixSignalRuntime.global_signal_bridge.alt_stack_previous = previous;
            PosixSignalRuntime.global_signal_bridge.alt_stack_memory = memory;
            PosixSignalRuntime.global_signal_bridge.restored_foreign_stack = PosixSignalRuntime.altStackWasEnabled(previous);

            if (PosixSignalRuntime.global_signal_bridge.signal_owner == self and PosixSignalRuntime.global_signal_bridge.installed_mask != 0) {
                for (0..64) |signo| {
                    const bit = (@as(u64, 1) << @intCast(signo));
                    if ((PosixSignalRuntime.global_signal_bridge.installed_mask & bit) == 0) continue;
                    const act = PosixSignalRuntime.runtimeSignalAction(true);
                    std.posix.sigaction(@intCast(signo), &act, null);
                }
            }
        } else {
            return error.UnsupportedPlatform;
        }
    }

    pub fn disableAlternateSignalStack(self: *RuntimeServices) Error!void {
        if (comptime compiled_platform_caps.alternate_signal_stack) {
            if (!self.compiled_caps.alternate_signal_stack) return;

            PosixSignalRuntime.global_signal_bridge.mutex.lock();
            defer PosixSignalRuntime.global_signal_bridge.mutex.unlock();

            if (PosixSignalRuntime.global_signal_bridge.alt_stack_owner != self) return;

            if (PosixSignalRuntime.global_signal_bridge.signal_owner == self and PosixSignalRuntime.global_signal_bridge.installed_mask != 0) {
                for (0..64) |signo| {
                    const bit = (@as(u64, 1) << @intCast(signo));
                    if ((PosixSignalRuntime.global_signal_bridge.installed_mask & bit) == 0) continue;
                    const act = PosixSignalRuntime.runtimeSignalAction(false);
                    std.posix.sigaction(@intCast(signo), &act, null);
                }
            }

            var disabled = PosixSignalRuntime.disabledAltStack();
            try std.posix.sigaltstack(&disabled, null);

            if (PosixSignalRuntime.global_signal_bridge.alt_stack_previous) |previous| {
                if (PosixSignalRuntime.altStackWasEnabled(previous)) {
                    var restore_copy = previous;
                    std.posix.sigaltstack(&restore_copy, null) catch |err| switch (err) {
                        error.SizeTooSmall => {},
                        else => return err,
                    };
                }
            }

            if (PosixSignalRuntime.global_signal_bridge.alt_stack_memory) |memory| self.allocator.free(memory);
            PosixSignalRuntime.global_signal_bridge.alt_stack_owner = null;
            PosixSignalRuntime.global_signal_bridge.alt_stack_previous = null;
            PosixSignalRuntime.global_signal_bridge.alt_stack_memory = null;
            PosixSignalRuntime.global_signal_bridge.restored_foreign_stack = false;
        } else {
            return;
        }
    }

    pub fn signalIngressSnapshot(self: *const RuntimeServices) SignalIngressSnapshot {
        const mutable: *RuntimeServices = @constCast(self);
        mutable.state_lock.lock();
        const named_value_count = mutable.named_values.items.len;
        const registered_signal_handlers = mutable.countRegisteredSignalHandlersLocked();
        mutable.state_lock.unlock();

        if (comptime !compiled_platform_caps.posix_signals) {
            return .{
                .installed = false,
                .installed_signals = 0,
                .owns_alternate_stack = false,
                .alternate_stack_size = 0,
                .restored_foreign_stack = false,
                .named_value_count = named_value_count,
                .registered_signal_handlers = registered_signal_handlers,
            };
        }

        PosixSignalRuntime.global_signal_bridge.mutex.lock();
        defer PosixSignalRuntime.global_signal_bridge.mutex.unlock();
        return .{
            .installed = PosixSignalRuntime.global_signal_bridge.signal_owner == self,
            .installed_signals = if (PosixSignalRuntime.global_signal_bridge.signal_owner == self) PosixSignalRuntime.global_signal_bridge.installed_mask else 0,
            .owns_alternate_stack = PosixSignalRuntime.global_signal_bridge.alt_stack_owner == self,
            .alternate_stack_size = if (PosixSignalRuntime.global_signal_bridge.alt_stack_owner == self and PosixSignalRuntime.global_signal_bridge.alt_stack_memory != null)
                PosixSignalRuntime.global_signal_bridge.alt_stack_memory.?.len
            else
                0,
            .restored_foreign_stack = PosixSignalRuntime.global_signal_bridge.alt_stack_owner == self and PosixSignalRuntime.global_signal_bridge.restored_foreign_stack,
            .named_value_count = named_value_count,
            .registered_signal_handlers = registered_signal_handlers,
        };
    }

    pub fn raiseSignal(self: *RuntimeServices, signo: u8) Error!void {
        if (comptime compiled_platform_caps.posix_signals) {
            if (!self.compiled_caps.posix_signals) return error.UnsupportedPlatform;
            if (signo >= 64) return error.UnsupportedSignal;
            try std.posix.raise(signo);
        } else {
            return error.UnsupportedPlatform;
        }
    }

    fn countRegisteredSignalHandlersLocked(self: *const RuntimeServices) usize {
        var count: usize = 0;
        for (self.signal_handlers) |handler| {
            if (handler != null) count += 1;
        }
        return count;
    }

    fn recordSignalFromHandler(self: *RuntimeServices, signo: u8) void {
        const bit = (@as(u64, 1) << @intCast(signo));
        _ = self.pending_signals.fetchOr(bit, .acq_rel);
    }

    fn countRoots(ctx: ?*anyopaque) usize {
        const self: *RuntimeServices = @ptrCast(@alignCast(ctx.?));
        self.state_lock.lock();
        defer self.state_lock.unlock();

        var count: usize = 0;
        for (self.named_values.items) |entry| {
            if (entry.value.isBlock()) count += 1;
        }
        for (self.signal_handlers) |handler| {
            if (handler) |rooted| {
                if (rooted.isBlock()) count += 1;
            }
        }
        return count;
    }

    fn visitRoots(ctx: ?*anyopaque, visitor: RootVisitor) void {
        const self: *RuntimeServices = @ptrCast(@alignCast(ctx.?));
        self.state_lock.lock();
        defer self.state_lock.unlock();

        for (self.named_values.items) |entry| {
            if (entry.value.isBlock()) visitor.visit(entry.value);
        }
        for (self.signal_handlers) |handler| {
            if (handler) |rooted| {
                if (rooted.isBlock()) visitor.visit(rooted);
            }
        }
    }
};

test "runtime_services: startup and shutdown are reference-counted" {
    var services = RuntimeServices.init(std.testing.allocator, compiled_platform_caps, .{});
    defer services.deinit();

    try services.startup();
    try services.startup();
    try std.testing.expect(services.isStarted());
    try services.shutdown();
    try std.testing.expect(services.isStarted());
    try services.shutdown();
    try std.testing.expect(!services.isStarted());
    try std.testing.expectError(RuntimeServices.Error.RuntimeAlreadyShutdown, services.startup());
}

test "runtime_services: named values are rooted through provider" {
    var services = RuntimeServices.init(std.testing.allocator, compiled_platform_caps, .{});
    defer services.deinit();

    try services.registerNamedValue("unit", Value.fromInt(0));
    try services.registerNamedValue("tuple", Value.fromHeapRef(.{ .index = 3, .generation = 1 }));

    var seen = std.ArrayListUnmanaged(Value){};
    defer seen.deinit(std.testing.allocator);

    const Collect = struct {
        fn visit(ctx: ?*anyopaque, rooted: Value) void {
            const items: *std.ArrayListUnmanaged(Value) = @ptrCast(@alignCast(ctx.?));
            items.append(std.testing.allocator, rooted) catch unreachable;
        }
    };

    const provider = services.provider();
    try std.testing.expectEqual(@as(usize, 1), provider.count());
    provider.visit(.{
        .ctx = &seen,
        .visit_fn = Collect.visit,
    });
    try std.testing.expectEqual(@as(usize, 1), seen.items.len);
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 3, .generation = 1 }), seen.items[0]);
}

test "runtime_services: pending signals and blocking sections are explicit" {
    var services = RuntimeServices.init(std.testing.allocator, compiled_platform_caps, .{});
    defer services.deinit();

    services.enterBlockingSection();
    try std.testing.expectEqual(@as(usize, 1), services.blockingDepth());
    try services.recordSignal(2);
    try services.recordSignal(5);
    try std.testing.expect(services.hasPendingSignals());
    try std.testing.expectEqual(@as(?u8, 2), services.nextPendingSignal());
    try std.testing.expectEqual((@as(u64, 1) << 2) | (@as(u64, 1) << 5), services.pendingSignalBits());
    try std.testing.expect(try services.clearPendingSignal(2));
    try std.testing.expectEqual(@as(?u8, 5), services.nextPendingSignal());
    try std.testing.expectEqual((@as(u64, 1) << 5), services.takePendingSignals());
    try std.testing.expectEqual(@as(u64, 0), services.takePendingSignals());
    try services.exitBlockingSection();
    try std.testing.expectEqual(@as(usize, 0), services.blockingDepth());
}

test "runtime_services: signal handlers are rooted runtime state" {
    var services = RuntimeServices.init(std.testing.allocator, compiled_platform_caps, .{});
    defer services.deinit();

    try services.registerSignalHandler(2, Value.fromHeapRef(.{ .index = 8, .generation = 1 }));
    try std.testing.expectEqual(Value.fromHeapRef(.{ .index = 8, .generation = 1 }), services.lookupSignalHandler(2).?);
    try std.testing.expectEqual(@as(usize, 1), services.provider().count());
}

test "runtime_services: real signal ingress owns alternate stack and restores process handlers" {
    if (!supports_native_signal_ingress) return error.SkipZigTest;

    var services = RuntimeServices.init(std.testing.allocator, compiled_platform_caps, .{});
    defer services.deinit();

    const signo: u8 = @intCast(std.posix.SIG.USR1);
    try services.enableAlternateSignalStack(null);
    try services.installSignalIngress(signo);

    const snapshot = services.signalIngressSnapshot();
    try std.testing.expect(snapshot.installed);
    try std.testing.expect(snapshot.owns_alternate_stack);
    try std.testing.expect(snapshot.alternate_stack_size >= platformDefaultSignalStackSize());
    try std.testing.expectEqual((@as(u64, 1) << @intCast(signo)), snapshot.installed_signals);

    try services.raiseSignal(signo);

    var spins: usize = 0;
    while (!services.hasPendingSignals() and spins < 1000) : (spins += 1) {
        std.Thread.yield() catch {};
    }
    try std.testing.expectEqual(@as(?u8, signo), services.nextPendingSignal());
    try std.testing.expect(try services.uninstallSignalIngress(signo));
    try services.disableAlternateSignalStack();
    const after = services.signalIngressSnapshot();
    try std.testing.expect(!after.installed);
    try std.testing.expect(!after.owns_alternate_stack);
}

test "runtime_services: service state is synchronized and runtime-local" {
    var left = RuntimeServices.init(std.testing.allocator, compiled_platform_caps, .{});
    defer left.deinit();
    var right = RuntimeServices.init(std.testing.allocator, compiled_platform_caps, .{});
    defer right.deinit();

    try left.registerNamedValue("shared", Value.fromInt(1));
    try right.registerNamedValue("shared", Value.fromInt(2));
    try std.testing.expectEqual(@as(i64, 1), left.lookupNamedValue("shared").?.asInt());
    try std.testing.expectEqual(@as(i64, 2), right.lookupNamedValue("shared").?.asInt());

    const Worker = struct {
        fn run(services: *RuntimeServices, signo: u8, base: i64) void {
            var i: usize = 0;
            while (i < 200) : (i += 1) {
                services.registerNamedValue("counter", Value.fromInt(base + @as(i64, @intCast(i)))) catch unreachable;
                services.registerSignalHandler(signo, Value.fromInt(base + @as(i64, @intCast(i)))) catch unreachable;
                _ = services.lookupNamedValue("counter");
                _ = services.lookupSignalHandler(signo);
            }
        }
    };

    var threads: [2]std.Thread = undefined;
    threads[0] = try std.Thread.spawn(.{}, Worker.run, .{ &left, @as(u8, 2), @as(i64, 0) });
    threads[1] = try std.Thread.spawn(.{}, Worker.run, .{ &left, @as(u8, 2), @as(i64, 1000) });
    for (threads) |thread| thread.join();

    const snapshot = left.signalIngressSnapshot();
    try std.testing.expect(snapshot.named_value_count >= 2);
    try std.testing.expect(snapshot.registered_signal_handlers >= 1);
    try std.testing.expect(left.lookupNamedValue("counter") != null);
    try std.testing.expect(left.lookupSignalHandler(2) != null);
    try std.testing.expect(right.lookupNamedValue("counter") == null);
    try std.testing.expect(right.lookupSignalHandler(2) == null);
}

test "runtime_services: host access is the intersection of compiled caps and runtime permissions" {
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

    var services = RuntimeServices.init(std.testing.allocator, caps, .{
        .allow_all = true,
    });
    defer services.deinit();

    const access = services.hostAccess();
    try std.testing.expect(access.read);
    try std.testing.expect(access.write);
    try std.testing.expect(!access.net);
    try std.testing.expect(!access.env);
    try std.testing.expect(!access.run);
    try std.testing.expect(!access.ffi);
    try std.testing.expect(access.hrtime);
}
