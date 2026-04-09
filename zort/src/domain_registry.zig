const std = @import("std");

pub const DomainHandle = struct {
    index: u32,
    generation: u32,
};

pub const DomainStatus = enum {
    attached,
    detached,
    blocked,
};

pub const DomainWorkerState = enum {
    stopped,
    running,
    stopping,
};

pub const DomainWorker = struct {
    state: DomainWorkerState = .stopped,
    owner_token: ?u64 = null,
    shutdown_requested: bool = false,
};

pub const DomainState = struct {
    status: DomainStatus,
    blocking_depth: usize = 0,
    worker: DomainWorker = .{},

    fn empty() DomainState {
        return .{
            .status = .detached,
            .blocking_depth = 0,
            .worker = .{},
        };
    }
};

const DomainSlot = struct {
    generation: u32,
    alive: bool,
    domain: DomainState,
};

pub const DomainRegistry = struct {
    allocator: std.mem.Allocator,
    domains: std.ArrayListUnmanaged(DomainSlot) = .{},
    free_domains: std.ArrayListUnmanaged(u32) = .{},
    main_domain: DomainHandle,

    pub const Error = error{
        InvalidDomain,
        AlreadyAttached,
        AlreadyDetached,
        BlockingSectionUnderflow,
        CannotDetachBlockedDomain,
        CannotDetachRunningWorker,
        CannotStartDetachedDomain,
        WorkerAlreadyOwned,
        WorkerNotOwned,
        WorkerShutdownPending,
    };

    pub const VerifyError = error{
        InvalidMainDomain,
        DetachedDomainIsBlocked,
        DetachedDomainHasWorker,
        RunningWorkerMissingOwner,
        StoppedWorkerKeepsOwner,
    };

    pub fn init(allocator: std.mem.Allocator) DomainRegistry {
        var registry = DomainRegistry{
            .allocator = allocator,
            .main_domain = .{ .index = 0, .generation = 0 },
        };
        registry.main_domain = registry.addDomain(.{
            .status = .attached,
        }) catch @panic("zort: out of memory while creating main domain");
        return registry;
    }

    pub fn deinit(self: *DomainRegistry) void {
        self.domains.deinit(self.allocator);
        self.free_domains.deinit(self.allocator);
    }

    pub fn mainDomain(self: *const DomainRegistry) DomainHandle {
        return self.main_domain;
    }

    pub fn contains(self: *const DomainRegistry, handle: DomainHandle) bool {
        return self.domain(handle) != null;
    }

    pub fn attachedCount(self: *const DomainRegistry) usize {
        var count: usize = 0;
        for (self.domains.items) |slot| {
            if (!slot.alive) continue;
            if (slot.domain.status != .detached) count += 1;
        }
        return count;
    }

    pub fn visitAttached(self: *const DomainRegistry, context: anytype, comptime visit: fn (@TypeOf(context), DomainHandle) void) void {
        for (self.domains.items, 0..) |slot, index| {
            if (!slot.alive) continue;
            if (slot.domain.status == .detached) continue;
            visit(context, .{
                .index = @intCast(index),
                .generation = slot.generation,
            });
        }
    }

    pub fn domain(self: *const DomainRegistry, handle: DomainHandle) ?*const DomainState {
        if (handle.index >= self.domains.items.len) return null;
        const slot = &self.domains.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return &slot.domain;
    }

    fn domainMut(self: *DomainRegistry, handle: DomainHandle) ?*DomainState {
        if (handle.index >= self.domains.items.len) return null;
        const slot = &self.domains.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return &slot.domain;
    }

    pub fn createDomain(self: *DomainRegistry) !DomainHandle {
        return self.addDomain(.{
            .status = .detached,
        });
    }

    pub fn worker(self: *const DomainRegistry, handle: DomainHandle) ?DomainWorker {
        const state = self.domain(handle) orelse return null;
        return state.worker;
    }

    pub fn attach(self: *DomainRegistry, handle: DomainHandle) Error!void {
        const state = self.domainMut(handle) orelse return error.InvalidDomain;
        if (state.status == .attached) return error.AlreadyAttached;
        state.status = .attached;
        state.blocking_depth = 0;
    }

    pub fn detach(self: *DomainRegistry, handle: DomainHandle) Error!void {
        const state = self.domainMut(handle) orelse return error.InvalidDomain;
        if (state.status == .detached) return error.AlreadyDetached;
        if (state.blocking_depth != 0) return error.CannotDetachBlockedDomain;
        if (state.worker.state != .stopped) return error.CannotDetachRunningWorker;
        state.status = .detached;
    }

    pub fn startWorker(self: *DomainRegistry, handle: DomainHandle, owner_token: u64) Error!bool {
        const state = self.domainMut(handle) orelse return error.InvalidDomain;
        if (state.status == .detached) return error.CannotStartDetachedDomain;
        return switch (state.worker.state) {
            .stopped => blk: {
                state.worker = .{
                    .state = .running,
                    .owner_token = owner_token,
                    .shutdown_requested = false,
                };
                break :blk true;
            },
            .running => if (state.worker.owner_token == owner_token) false else error.WorkerAlreadyOwned,
            .stopping => error.WorkerShutdownPending,
        };
    }

    pub fn requestWorkerShutdown(self: *DomainRegistry, handle: DomainHandle, owner_token: u64) Error!bool {
        const state = self.domainMut(handle) orelse return error.InvalidDomain;
        if (state.worker.owner_token != owner_token) return error.WorkerNotOwned;
        return switch (state.worker.state) {
            .stopped => error.WorkerNotOwned,
            .running => blk: {
                state.worker.state = .stopping;
                state.worker.shutdown_requested = true;
                break :blk true;
            },
            .stopping => false,
        };
    }

    pub fn finishWorkerShutdown(self: *DomainRegistry, handle: DomainHandle, owner_token: u64) Error!bool {
        const state = self.domainMut(handle) orelse return error.InvalidDomain;
        if (state.worker.state == .stopped) return false;
        if (state.worker.owner_token != owner_token) return error.WorkerNotOwned;
        if (state.worker.state != .stopping) return error.WorkerShutdownPending;
        state.worker = .{};
        return true;
    }

    pub fn enterBlocking(self: *DomainRegistry, handle: DomainHandle) Error!void {
        const state = self.domainMut(handle) orelse return error.InvalidDomain;
        state.blocking_depth +%= 1;
        state.status = .blocked;
    }

    pub fn exitBlocking(self: *DomainRegistry, handle: DomainHandle) Error!void {
        const state = self.domainMut(handle) orelse return error.InvalidDomain;
        if (state.blocking_depth == 0) return error.BlockingSectionUnderflow;
        state.blocking_depth -= 1;
        state.status = if (state.blocking_depth == 0) .attached else .blocked;
    }

    pub fn verify(self: *const DomainRegistry) VerifyError!void {
        const main = self.domain(self.main_domain) orelse return error.InvalidMainDomain;
        if (main.status == .detached) return error.InvalidMainDomain;

        for (self.domains.items) |slot| {
            if (!slot.alive) continue;
            if (slot.domain.status == .detached and slot.domain.blocking_depth != 0) {
                return error.DetachedDomainIsBlocked;
            }
            if (slot.domain.status == .detached and slot.domain.worker.state != .stopped) {
                return error.DetachedDomainHasWorker;
            }
            switch (slot.domain.worker.state) {
                .stopped => {
                    if (slot.domain.worker.owner_token != null) return error.StoppedWorkerKeepsOwner;
                },
                .running, .stopping => {
                    if (slot.domain.worker.owner_token == null) return error.RunningWorkerMissingOwner;
                },
            }
        }
    }

    fn addDomain(self: *DomainRegistry, state: DomainState) !DomainHandle {
        const slot_index: usize = if (self.free_domains.items.len > 0) blk: {
            const reused = self.free_domains.pop() orelse unreachable;
            break :blk @intCast(reused);
        } else self.domains.items.len;

        if (slot_index < self.domains.items.len) {
            const slot = &self.domains.items[slot_index];
            slot.alive = true;
            slot.domain = state;
            return .{
                .index = @intCast(slot_index),
                .generation = slot.generation,
            };
        }

        try self.domains.append(self.allocator, .{
            .generation = 1,
            .alive = true,
            .domain = state,
        });
        return .{
            .index = @intCast(slot_index),
            .generation = 1,
        };
    }
};

test "domain_registry: main domain starts attached" {
    var registry = DomainRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const main = registry.mainDomain();
    try std.testing.expectEqual(@as(?DomainStatus, .attached), registry.domain(main).?.status);
    try registry.verify();
}

test "domain_registry: attach and detach explicit domains" {
    var registry = DomainRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const domain = try registry.createDomain();
    try std.testing.expectEqual(@as(?DomainStatus, .detached), registry.domain(domain).?.status);

    try registry.attach(domain);
    try std.testing.expectEqual(@as(?DomainStatus, .attached), registry.domain(domain).?.status);

    try registry.detach(domain);
    try std.testing.expectEqual(@as(?DomainStatus, .detached), registry.domain(domain).?.status);
}

test "domain_registry: blocking depth owns blocked status" {
    var registry = DomainRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const main = registry.mainDomain();
    try registry.enterBlocking(main);
    try std.testing.expectEqual(@as(?DomainStatus, .blocked), registry.domain(main).?.status);
    try std.testing.expectEqual(@as(usize, 1), registry.domain(main).?.blocking_depth);

    try registry.exitBlocking(main);
    try std.testing.expectEqual(@as(?DomainStatus, .attached), registry.domain(main).?.status);
    try std.testing.expectEqual(@as(usize, 0), registry.domain(main).?.blocking_depth);
}

test "domain_registry: detached domains cannot stay blocked" {
    var registry = DomainRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const domain = try registry.createDomain();
    try registry.attach(domain);
    try registry.enterBlocking(domain);
    try std.testing.expectError(DomainRegistry.Error.CannotDetachBlockedDomain, registry.detach(domain));
    try registry.exitBlocking(domain);
    try registry.detach(domain);
    try registry.verify();
}

test "domain_registry: worker lifecycle is explicit and detach waits for shutdown" {
    var registry = DomainRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const domain = try registry.createDomain();
    try registry.attach(domain);
    try std.testing.expect(try registry.startWorker(domain, 77));
    try std.testing.expectEqual(@as(?DomainWorkerState, .running), registry.worker(domain).?.state);
    try std.testing.expectError(DomainRegistry.Error.CannotDetachRunningWorker, registry.detach(domain));

    try std.testing.expect(try registry.requestWorkerShutdown(domain, 77));
    try std.testing.expectEqual(@as(?DomainWorkerState, .stopping), registry.worker(domain).?.state);
    try std.testing.expect(try registry.finishWorkerShutdown(domain, 77));
    try std.testing.expectEqual(@as(?DomainWorkerState, .stopped), registry.worker(domain).?.state);

    try registry.detach(domain);
    try registry.verify();
}

test "domain_registry: detached domains cannot start workers" {
    var registry = DomainRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const domain = try registry.createDomain();
    try std.testing.expectError(DomainRegistry.Error.CannotStartDetachedDomain, registry.startWorker(domain, 5));
}
