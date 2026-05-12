const std = @import("std");
const atomic_mod = @import("atomic_primitives.zig");
const control_kernel_mod = @import("control_kernel.zig");
const domain_registry_mod = @import("domain_registry.zig");
const value = @import("value.zig");
const event_sink_mod = @import("event_sink.zig");
const fiber_scheduler_mod = @import("fiber_scheduler.zig");
const heap_store = @import("heap_store.zig");
const collector_mod = @import("collector.zig");
const language_mod = @import("language.zig");
const liveness_mod = @import("liveness.zig");
const memprof_mod = @import("memprof.zig");
const mutator = @import("mutator.zig");
const platform_caps_mod = @import("platform_caps.zig");
const remembered_set_mod = @import("remembered_set.zig");
const root_provider_mod = @import("root_provider.zig");
const root_registry = @import("root_registry.zig");
const runtime_services_mod = @import("runtime_services.zig");
const stw_coordinator_mod = @import("stw_coordinator.zig");

pub const Value = value.Value;
pub const Tag = value.Tag;
pub const HeapRef = value.HeapRef;
pub const AtomicCounter = atomic_mod.AtomicCounter;
pub const AtomicFlag = atomic_mod.AtomicFlag;
pub const Event = event_sink_mod.Event;
pub const EventCounters = event_sink_mod.Counters;
pub const EventRecorder = event_sink_mod.Recorder;
pub const EventSink = event_sink_mod.EventSink;
pub const GcSnapshotEvent = event_sink_mod.GcSnapshotEvent;
pub const ObjectExplain = struct {
    handle: HeapRef,
    kind: ObjectKind,
    space: heap_store.Space,
    payload_bytes: usize,
    storage_bytes: usize,
    scan_words: usize,
    allocation_cost_units: usize,
    explicit_roots: usize,
    control_roots: usize,
    service_roots: usize,
    liveness_roots: usize,
    remembered_targets: usize,
    memprof_sample: ?memprof_mod.SampleView = null,
    last_event: ?event_sink_mod.ObjectLastEvent = null,
};
pub const TraceRecorder = event_sink_mod.TraceRecorder;
pub const TraceEntry = event_sink_mod.TraceEntry;
pub const RootProviderEvent = event_sink_mod.RootProviderEvent;
pub const ControlKernel = control_kernel_mod.ControlKernel;
pub const PerformResult = control_kernel_mod.ControlKernel.PerformResult;
pub const ResumeResult = control_kernel_mod.ControlKernel.ResumeResult;
pub const ContinuationHandle = control_kernel_mod.ContinuationHandle;
pub const EffectId = control_kernel_mod.EffectId;
pub const FiberHandle = control_kernel_mod.FiberHandle;
pub const HandlerFrame = control_kernel_mod.HandlerFrame;
pub const FrameInfo = control_kernel_mod.ManagedStack.FrameInfo;
pub const StackLimits = control_kernel_mod.StackLimits;
pub const SuspendedStack = control_kernel_mod.SuspendedStack;
pub const DomainHandle = domain_registry_mod.DomainHandle;
pub const DomainRegistry = domain_registry_mod.DomainRegistry;
pub const DomainWorker = domain_registry_mod.DomainWorker;
pub const DomainWorkerState = domain_registry_mod.DomainWorkerState;
pub const DomainStatus = domain_registry_mod.DomainStatus;
pub const FiberScheduler = fiber_scheduler_mod.FiberScheduler;
pub const SchedulerLaneSnapshot = fiber_scheduler_mod.LaneCoordinationSnapshot;
pub const HeapBackendKind = heap_store.BackendKind;
pub const HeapStorageOwner = heap_store.StorageOwner;
pub const HeapStore = heap_store.HeapStore;
pub const HeapStoreConfig = heap_store.Config;
pub const Object = heap_store.Object;
pub const ObjectKind = heap_store.ObjectKind;
pub const Space = heap_store.Space;
pub const Collector = collector_mod.Collector;
pub const Language = language_mod.Language;
pub const Mutator = mutator.Mutator;
pub const ManagedLiveness = liveness_mod.ManagedLiveness;
pub const MemprofConfig = memprof_mod.Config;
pub const MemprofState = memprof_mod.MemprofState;
pub const MemprofSampling = memprof_mod.SamplingMode;
pub const MemprofSample = memprof_mod.SampleView;
pub const PlatformCaps = platform_caps_mod.PlatformCaps;
pub const BuildCaps = platform_caps_mod.BuildCaps;
pub const RuntimePermissions = platform_caps_mod.RuntimePermissions;
pub const HostAccess = platform_caps_mod.HostAccess;
pub const WeakRefHandle = liveness_mod.WeakRefHandle;
pub const EphemeronHandle = liveness_mod.EphemeronHandle;
pub const FinalizerHandle = liveness_mod.FinalizerHandle;
pub const FinalizerMode = liveness_mod.FinalizerMode;
pub const ReadyFinalizer = liveness_mod.ReadyFinalizer;
pub const RememberedSet = remembered_set_mod.RememberedSet;
pub const RootProvider = root_provider_mod.RootProvider;
pub const RootVisitor = root_provider_mod.RootVisitor;
pub const RootBinding = root_registry.RootBinding;
pub const RootFrame = root_registry.RootFrame;
pub const RootRegistry = root_registry.RootRegistry;
pub const RootHandle = root_registry.RootHandle;
pub const RuntimeServices = runtime_services_mod.RuntimeServices;
pub const SignalIngressSnapshot = runtime_services_mod.SignalIngressSnapshot;
pub const StopTheWorldCoordinator = stw_coordinator_mod.StopTheWorldCoordinator;
pub const StopTheWorldSnapshot = stw_coordinator_mod.CoordinationSnapshot;
pub const Error = language_mod.Error;

pub const PendingActionCheckpoint = enum {
    explicit,
    scheduler_safepoint,
    blocking_enter,
    blocking_exit,
    stw_pause,
};

pub const PendingSignal = struct {
    signo: u8,
    handler: Value,
};

pub const PendingAction = union(enum) {
    signal: PendingSignal,
    finalizer: ReadyFinalizer,
};

pub const PendingActionDelivery = struct {
    ctx: ?*anyopaque = null,
    deliver_fn: ?*const fn (ctx: ?*anyopaque, checkpoint: PendingActionCheckpoint, action: PendingAction) anyerror!void = null,

    pub fn configured(self: PendingActionDelivery) bool {
        return self.deliver_fn != null;
    }
};

pub const Runtime = struct {
    pub const GcStrategy = collector_mod.GcStrategy;

    pub const Config = struct {
        debugRootChecks: bool = false,
        debugChecks: DebugChecks = .{},
        fixedArena: ?[]u8 = null,
        eventSink: EventSink = EventSink.noop(),
        /// Strategy selection for collection:
        /// - .mark_sweep: root-based mark-and-sweep (default, baseline behavior)
        /// - .generational: nursery/minor baseline with explicit promotion
        /// - .bump: experimental full reset path
        gcStrategy: GcStrategy = .mark_sweep,
        nurseryObjectUnits: usize = 32,
        nurseryLiveUnits: usize = 1024,
        nurseryLiveObjects: usize = 256,
        memprof: MemprofConfig = .{},
        stackLimits: StackLimits = .{},
        pendingActionDelivery: PendingActionDelivery = .{},
        permissions: RuntimePermissions = .{},
    };

    pub const DebugChecks = struct {
        verify_roots: bool = false,
        verify_heap_store: bool = false,
        verify_control_kernel: bool = false,
        verify_after_collect: bool = false,

        pub fn any(self: DebugChecks) bool {
            return self.verify_roots or self.verify_heap_store or self.verify_control_kernel or self.verify_after_collect;
        }
    };

    pub const Stats = struct {
        root_generation: usize,
        root_registrations: usize,
        root_unregistrations: usize,
        collect_generations: usize,
        minor_collect_generations: usize,
        nursery_objects: usize,
        nursery_allocation_units: usize,
        major_objects: usize,
        major_allocation_units: usize,
    };

    allocator: std.mem.Allocator,
    event_sink: EventSink,
    compiled_caps: PlatformCaps,
    runtime_permissions: RuntimePermissions,
    host_access: HostAccess,
    domains: DomainRegistry,
    control_kernel: ControlKernel,
    fiber_scheduler: FiberScheduler,
    stw: StopTheWorldCoordinator,
    heap_store: HeapStore,
    remembered_set: RememberedSet,
    root_registry: RootRegistry,
    services: RuntimeServices,
    liveness: ManagedLiveness,
    memprof: MemprofState,
    debug_root_checks: bool = false,
    debug_checks: DebugChecks = .{},
    fixed_arena: ?std.heap.FixedBufferAllocator = null,
    gc_strategy: GcStrategy = .mark_sweep,
    fixed_arena_buffer: ?[]u8 = null,
    collect_generations: usize = 0,
    minor_collect_generations: usize = 0,
    main_worker_token: u64 = 1,
    pending_action_delivery: PendingActionDelivery = .{},
    draining_pending_actions: bool = false,

    pub fn init(allocator: std.mem.Allocator) Runtime {
        return initWithConfig(allocator, .{});
    }

    pub fn initWithConfig(allocator: std.mem.Allocator, config: Config) Runtime {
        const compiled_caps = PlatformCaps.detect();
        const runtime_permissions = config.permissions.normalized();
        var runtime = Runtime{
            .allocator = allocator,
            .event_sink = config.eventSink,
            .compiled_caps = compiled_caps,
            .runtime_permissions = runtime_permissions,
            .host_access = HostAccess.from(compiled_caps, runtime_permissions),
            .domains = DomainRegistry.init(allocator),
            .control_kernel = undefined,
            .fiber_scheduler = undefined,
            .stw = StopTheWorldCoordinator.init(allocator, config.eventSink),
            .heap_store = HeapStore.init(allocator),
            .remembered_set = RememberedSet.init(allocator),
            .root_registry = RootRegistry.init(allocator, config.eventSink),
            .services = RuntimeServices.init(allocator, compiled_caps, runtime_permissions),
            .liveness = ManagedLiveness.init(allocator),
            .memprof = MemprofState.init(allocator, config.eventSink, config.memprof),
            .debug_root_checks = config.debugRootChecks,
            .debug_checks = config.debugChecks,
            .pending_action_delivery = config.pendingActionDelivery,
        };
        runtime.control_kernel = ControlKernel.initWithConfig(allocator, .{
            .event_sink = config.eventSink,
            .initial_domain = runtime.domains.mainDomain(),
            .stack_limits = config.stackLimits,
        });
        runtime.fiber_scheduler = FiberScheduler.init(
            allocator,
            config.eventSink,
            runtime.domains.mainDomain(),
            runtime.control_kernel.currentFiber(),
        );
        runtime.stw.registerDomain(runtime.domains.mainDomain()) catch @panic("zort: out of memory while creating main stw slot");
        _ = runtime.fiber_scheduler.claimLaneOwnership(runtime.domains.mainDomain(), runtime.main_worker_token) catch
            @panic("zort: failed to claim main scheduler lane");
        _ = runtime.domains.startWorker(runtime.domains.mainDomain(), runtime.main_worker_token) catch
            @panic("zort: failed to bootstrap main domain worker");
        if (config.fixedArena) |buffer| {
            runtime.fixed_arena = std.heap.FixedBufferAllocator.init(buffer);
            runtime.fixed_arena_buffer = buffer;
        }
        runtime.gc_strategy = config.gcStrategy;
        runtime.heap_store.configureNursery(.{
            .enabled = config.gcStrategy == .generational,
            .max_object_units = config.nurseryObjectUnits,
            .max_live_units = config.nurseryLiveUnits,
            .max_live_objects = config.nurseryLiveObjects,
        });
        runtime.services.startup() catch unreachable;
        return runtime;
    }

    pub fn initWithFixedArena(allocator: std.mem.Allocator, buffer: []u8) Runtime {
        return initWithConfig(allocator, .{
            .fixedArena = buffer,
        });
    }

    pub fn rootStats(self: *Runtime) Stats {
        const stats = self.root_registry.stats();
        return .{
            .root_generation = stats.root_generation,
            .root_registrations = stats.root_registrations,
            .root_unregistrations = stats.root_unregistrations,
            .collect_generations = self.collect_generations,
            .minor_collect_generations = self.minor_collect_generations,
            .nursery_objects = self.heap_store.spaceStats(.nursery).objects,
            .nursery_allocation_units = self.heap_store.spaceStats(.nursery).allocation_units,
            .major_objects = self.heap_store.spaceStats(.major).objects,
            .major_allocation_units = self.heap_store.spaceStats(.major).allocation_units,
        };
    }

    pub fn platformCaps(self: *const Runtime) PlatformCaps {
        return self.compiled_caps;
    }

    pub fn permissions(self: *const Runtime) RuntimePermissions {
        return self.runtime_permissions;
    }

    pub fn hostAccess(self: *const Runtime) HostAccess {
        return self.host_access;
    }

    pub fn deinit(self: *Runtime) void {
        self.services.shutdown() catch {};
        self.services.deinit();
        self.liveness.deinit();
        self.memprof.deinit();
        self.heap_store.deinit(self.fixed_arena_buffer != null);
        self.remembered_set.deinit();
        self.root_registry.deinit();
        self.control_kernel.deinit();
        self.fiber_scheduler.deinit();
        self.stw.deinit();
        self.domains.deinit();
    }

    pub fn objectCount(self: *Runtime) usize {
        return self.heap_store.count();
    }

    pub fn objectSpace(self: *Runtime, block_value: Value) ?Space {
        const handle = block_value.asHeapRef() orelse return null;
        return self.heap_store.spaceOf(handle);
    }

    pub fn objectFromDebug(self: *Runtime, block_value: Value) ?*Object {
        return self.objectFrom(block_value);
    }

    fn collectorWithProviders(self: *Runtime, root_providers: []const RootProvider) Collector {
        return Collector.init(
            &self.heap_store,
            &self.remembered_set,
            &self.memprof,
            root_providers,
            &self.fixed_arena,
            self.fixed_arena_buffer,
            self.gc_strategy,
            self.event_sink,
            .{
                .ctx = &self.liveness,
                .process_weak_fn = processWeakHook,
                .process_finalizers_fn = processFinalizersHook,
            },
        );
    }

    pub fn mutator(self: *Runtime) Mutator {
        return Mutator.init(self.currentAllocator(), &self.heap_store, self.event_sink, &self.remembered_set);
    }

    pub fn language(self: *Runtime) Language {
        return Language.init(self.allocator, self.currentAllocator(), &self.heap_store, self.event_sink, &self.remembered_set);
    }

    pub fn controlKernel(self: *Runtime) *const ControlKernel {
        return &self.control_kernel;
    }

    pub fn domainRegistry(self: *Runtime) *DomainRegistry {
        return &self.domains;
    }

    pub fn fiberScheduler(self: *Runtime) *FiberScheduler {
        return &self.fiber_scheduler;
    }

    pub fn schedulerLaneSnapshot(self: *Runtime, domain: DomainHandle) !SchedulerLaneSnapshot {
        return self.fiber_scheduler.coordinationSnapshot(domain);
    }

    pub fn requestSchedulerWake(self: *Runtime, domain: DomainHandle) !void {
        try self.fiber_scheduler.requestWake(domain);
    }

    pub fn claimSchedulerLane(self: *Runtime, domain: DomainHandle, token: u64) !bool {
        return self.fiber_scheduler.claimLaneOwnership(domain, token);
    }

    pub fn releaseSchedulerLane(self: *Runtime, domain: DomainHandle, token: u64) !bool {
        return self.fiber_scheduler.releaseLaneOwnership(domain, token);
    }

    pub fn takeSchedulerWake(self: *Runtime, domain: DomainHandle) !bool {
        return self.fiber_scheduler.takeWakeRequest(domain);
    }

    pub fn stwCoordinator(self: *Runtime) *StopTheWorldCoordinator {
        return &self.stw;
    }

    pub fn stwSnapshot(self: *Runtime) StopTheWorldSnapshot {
        return self.stw.coordinationSnapshot();
    }

    pub fn currentDomain(self: *Runtime) DomainHandle {
        return self.control_kernel.currentDomain();
    }

    pub fn mainWorkerToken(self: *const Runtime) u64 {
        return self.main_worker_token;
    }

    pub fn currentFiber(self: *Runtime) FiberHandle {
        return self.control_kernel.currentFiber();
    }

    pub fn stackLimits(self: *Runtime) StackLimits {
        return self.control_kernel.stackLimits();
    }

    pub fn runtimeServices(self: *Runtime) *RuntimeServices {
        return &self.services;
    }

    pub fn managedLiveness(self: *Runtime) *ManagedLiveness {
        return &self.liveness;
    }

    pub fn memprofState(self: *Runtime) *MemprofState {
        return &self.memprof;
    }

    pub fn rememberedSet(self: *Runtime) *RememberedSet {
        return &self.remembered_set;
    }

    pub fn domainWorker(self: *Runtime, handle: DomainHandle) ?DomainWorker {
        return self.domains.worker(handle);
    }

    pub fn createDomain(self: *Runtime) !DomainHandle {
        const domain = try self.domains.createDomain();
        try self.fiber_scheduler.registerDomain(domain);
        try self.stw.registerDomain(domain);
        return domain;
    }

    pub fn attachDomain(self: *Runtime, handle: DomainHandle) !void {
        try self.domains.attach(handle);
    }

    pub fn detachDomain(self: *Runtime, handle: DomainHandle) !void {
        try self.domains.detach(handle);
    }

    pub fn startDomainWorker(self: *Runtime, handle: DomainHandle, owner_token: u64) !bool {
        const lane = try self.schedulerLaneSnapshot(handle);
        if (!try self.claimSchedulerLane(handle, owner_token)) return error.WorkerAlreadyOwned;
        errdefer if (lane.owner_token == null) {
            _ = self.releaseSchedulerLane(handle, owner_token) catch {};
        };

        const started = try self.domains.startWorker(handle, owner_token);
        if (started) try self.requestSchedulerWake(handle);
        return started;
    }

    pub fn requestDomainWorkerShutdown(self: *Runtime, handle: DomainHandle, owner_token: u64) !bool {
        const requested = try self.domains.requestWorkerShutdown(handle, owner_token);
        if (requested) try self.requestSchedulerWake(handle);
        return requested;
    }

    pub fn finishDomainWorkerShutdown(self: *Runtime, handle: DomainHandle, owner_token: u64) !bool {
        const lane = try self.schedulerLaneSnapshot(handle);
        if (lane.owner_token != owner_token) return error.WorkerNotOwned;
        if (lane.current != null or lane.runnable_count != 0 or lane.parked_count != 0 or lane.suspended_count != 0) {
            return error.WorkerNotQuiescent;
        }
        if (!try self.releaseSchedulerLane(handle, owner_token)) return error.WorkerNotOwned;
        return self.domains.finishWorkerShutdown(handle, owner_token);
    }

    fn requiredLaneOwnerToken(self: *Runtime, domain: DomainHandle) !u64 {
        const worker = self.domains.worker(domain) orelse return error.InvalidDomain;
        if (worker.state == .stopped or worker.owner_token == null) return error.WorkerNotRunning;
        const token = worker.owner_token.?;
        const lane = try self.schedulerLaneSnapshot(domain);
        if (lane.owner_token != token) return error.WorkerNotOwned;
        return token;
    }

    pub fn createFiberInDomain(self: *Runtime, parent: ?FiberHandle, domain: DomainHandle) !FiberHandle {
        const state = self.domains.domain(domain) orelse return error.InvalidDomain;
        if (state.status == .detached) return error.DomainDetached;
        const owner_token = try self.requiredLaneOwnerToken(domain);
        const fiber = try self.control_kernel.createFiberInDomain(parent, domain);
        try self.fiber_scheduler.enqueue(domain, fiber, owner_token);
        return fiber;
    }

    pub fn spawnFiberInDomain(self: *Runtime, parent: ?FiberHandle, domain: DomainHandle) !FiberHandle {
        return self.createFiberInDomain(parent, domain);
    }

    pub fn activateFiberInDomain(self: *Runtime, domain: DomainHandle, fiber: FiberHandle) !void {
        const owner_token = try self.requiredLaneOwnerToken(domain);
        try self.fiber_scheduler.activate(domain, fiber, owner_token);
        try self.control_kernel.activateFiber(fiber);
        _ = try self.drainConfiguredPendingActions(.scheduler_safepoint);
    }

    pub fn scheduleNextFiber(self: *Runtime, domain: DomainHandle) !?FiberHandle {
        const owner_token = try self.requiredLaneOwnerToken(domain);
        const next = try self.fiber_scheduler.switchToNext(domain, owner_token) orelse return null;
        try self.control_kernel.activateFiber(next);
        _ = try self.drainConfiguredPendingActions(.scheduler_safepoint);
        return next;
    }

    pub fn yieldCurrentFiber(self: *Runtime) !?FiberHandle {
        const current_domain = self.currentDomain();
        const owner_token = try self.requiredLaneOwnerToken(current_domain);
        const next = try self.fiber_scheduler.yieldCurrent(current_domain, owner_token) orelse return null;
        try self.control_kernel.activateFiber(next);
        _ = try self.drainConfiguredPendingActions(.scheduler_safepoint);
        return next;
    }

    pub fn parkCurrentFiber(self: *Runtime) !?FiberHandle {
        const current_domain = self.currentDomain();
        const owner_token = try self.requiredLaneOwnerToken(current_domain);
        const next = try self.fiber_scheduler.parkCurrent(current_domain, owner_token) orelse return null;
        try self.control_kernel.activateFiber(next);
        _ = try self.drainConfiguredPendingActions(.scheduler_safepoint);
        return next;
    }

    pub fn unparkFiber(self: *Runtime, domain: DomainHandle, fiber: FiberHandle) !void {
        const owner_token = try self.requiredLaneOwnerToken(domain);
        try self.fiber_scheduler.unpark(domain, fiber, owner_token);
    }

    pub fn transferRunnableFiber(self: *Runtime, source_domain: DomainHandle, target_domain: DomainHandle, fiber: FiberHandle) !bool {
        const fiber_state = self.control_kernel.fiber(fiber) orelse return error.InvalidFiber;
        if (fiber_state.domain.index != source_domain.index or fiber_state.domain.generation != source_domain.generation) {
            return error.FiberDomainMismatch;
        }
        if (target_domain.index == source_domain.index and target_domain.generation == source_domain.generation) return false;

        const source_owner = try self.requiredLaneOwnerToken(source_domain);
        const target_owner = try self.requiredLaneOwnerToken(target_domain);
        const transferred = try self.fiber_scheduler.transferRunnable(
            source_domain,
            target_domain,
            fiber,
            source_owner,
            target_owner,
        );
        _ = try self.control_kernel.assignFiberDomain(fiber, target_domain);
        return transferred;
    }

    pub fn pushEffectHandler(self: *Runtime, fiber: FiberHandle, handler: HandlerFrame) !void {
        try self.control_kernel.pushHandler(fiber, handler);
    }

    pub fn popEffectHandler(self: *Runtime, fiber: FiberHandle) !HandlerFrame {
        return self.control_kernel.popHandler(fiber);
    }

    pub fn handlerCount(self: *Runtime, fiber: FiberHandle) !usize {
        return self.control_kernel.handlerCount(fiber);
    }

    pub fn pushFiberFrame(self: *Runtime, fiber: FiberHandle, site_id: u32) !void {
        try self.control_kernel.pushFrame(fiber, site_id);
    }

    pub fn popFiberFrame(self: *Runtime, fiber: FiberHandle) !FrameInfo {
        return self.control_kernel.popFrame(fiber);
    }

    pub fn pushFiberFrameRoot(self: *Runtime, fiber: FiberHandle, rooted: Value) !void {
        try self.control_kernel.pushFrameRoot(fiber, rooted);
    }

    pub fn frameCount(self: *Runtime, fiber: FiberHandle) !usize {
        return self.control_kernel.frameCount(fiber);
    }

    pub fn enterCallbackBoundary(self: *Runtime, fiber: FiberHandle) !void {
        try self.control_kernel.enterCallbackBoundary(fiber);
    }

    pub fn exitCallbackBoundary(self: *Runtime, fiber: FiberHandle) !void {
        try self.control_kernel.exitCallbackBoundary(fiber);
    }

    pub fn performEffect(
        self: *Runtime,
        effect: EffectId,
        payload: Value,
        captured_roots: []const Value,
    ) !PerformResult {
        return self.performEffectAt(0, effect, payload, captured_roots);
    }

    pub fn performEffectAt(
        self: *Runtime,
        site_id: u32,
        effect: EffectId,
        payload: Value,
        captured_roots: []const Value,
    ) !PerformResult {
        const current_fiber = self.control_kernel.currentFiber();
        const current_domain = self.currentDomain();
        const performed = try self.control_kernel.performAt(site_id, effect, payload, captured_roots);
        if (performed.handler_fiber.index != current_fiber.index or performed.handler_fiber.generation != current_fiber.generation) {
            const current_owner = try self.requiredLaneOwnerToken(current_domain);
            _ = try self.fiber_scheduler.suspendCurrent(current_domain, current_owner);
            const handler_domain = self.control_kernel.fiber(performed.handler_fiber).?.domain;
            try self.activateFiberInDomain(handler_domain, performed.handler_fiber);
        } else {
            try self.control_kernel.activateFiber(current_fiber);
        }
        return performed;
    }

    pub fn reperformEffect(
        self: *Runtime,
        effect: EffectId,
        payload: Value,
        captured_roots: []const Value,
    ) !PerformResult {
        return self.reperformEffectAt(0, effect, payload, captured_roots);
    }

    pub fn reperformEffectAt(
        self: *Runtime,
        site_id: u32,
        effect: EffectId,
        payload: Value,
        captured_roots: []const Value,
    ) !PerformResult {
        const current_fiber = self.control_kernel.currentFiber();
        const current_domain = self.currentDomain();
        const performed = try self.control_kernel.reperformAt(site_id, effect, payload, captured_roots);
        if (performed.handler_fiber.index != current_fiber.index or performed.handler_fiber.generation != current_fiber.generation) {
            const current_owner = try self.requiredLaneOwnerToken(current_domain);
            _ = try self.fiber_scheduler.suspendCurrent(current_domain, current_owner);
            const handler_domain = self.control_kernel.fiber(performed.handler_fiber).?.domain;
            try self.activateFiberInDomain(handler_domain, performed.handler_fiber);
        } else {
            try self.control_kernel.activateFiber(current_fiber);
        }
        return performed;
    }

    pub fn resumeContinuation(self: *Runtime, handle: ContinuationHandle, value_to_resume: Value) !ResumeResult {
        const continuation = self.control_kernel.continuation(handle) orelse return error.InvalidContinuation;
        const target_domain = self.currentDomain();
        const resumed = try self.control_kernel.resumeContinuation(handle, value_to_resume);
        if (continuation.handler_fiber.index != continuation.fiber.index or continuation.handler_fiber.generation != continuation.fiber.generation) {
            const source_owner = try self.requiredLaneOwnerToken(continuation.domain);
            _ = try self.fiber_scheduler.discardSuspended(continuation.domain, continuation.fiber, source_owner);
        }
        try self.activateFiberInDomain(target_domain, resumed.fiber);
        return resumed;
    }

    pub fn dropContinuation(self: *Runtime, handle: ContinuationHandle) bool {
        const continuation = self.control_kernel.continuation(handle) orelse return false;
        const fiber = continuation.fiber;
        const fiber_domain = continuation.domain;
        const self_handled = continuation.handler_fiber.index == fiber.index and continuation.handler_fiber.generation == fiber.generation;
        const was_suspended = continuation.status == .suspended;
        const dropped = self.control_kernel.dropContinuation(handle);
        if (!dropped) return false;
        if (was_suspended and !self_handled) {
            const source_owner = self.requiredLaneOwnerToken(fiber_domain) catch return false;
            _ = self.fiber_scheduler.discardSuspended(fiber_domain, fiber, source_owner) catch return false;
            self.control_kernel.discardFiber(fiber) catch return false;
        }
        return true;
    }

    pub fn requestStopTheWorld(self: *Runtime) !usize {
        const generation = try self.stw.request(self.currentDomain(), self.domainRegistry().attachedCount());
        _ = try self.enterSafepoint(self.currentDomain());
        return generation;
    }

    pub fn enterSafepoint(self: *Runtime, domain: DomainHandle) !bool {
        const generation = self.stw.currentGeneration();
        const acknowledged = try self.stw.acknowledgePause(domain, generation);
        if (acknowledged) _ = try self.drainConfiguredPendingActions(.stw_pause);
        return acknowledged;
    }

    pub fn resumeTheWorld(self: *Runtime) void {
        self.stw.resumeWorld();
    }

    pub fn alloc(self: *Runtime, arity: usize, tag: Tag) !Value {
        self.prepareCompatAllocation(arity, tag);
        var writer = self.mutator();
        const allocated = try writer.allocCompat(arity, tag);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocTuple(self: *Runtime, len: usize) !Value {
        self.prepareAllocation(.tuple, tupleAllocationUnits(len));
        var surface = self.language();
        const allocated = try surface.allocTuple(len);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    /// Allocate a tuple and initialize all fields from `fields`.
    pub fn tuple(self: *Runtime, fields: []const Value) !Value {
        self.prepareAllocation(.tuple, tupleAllocationUnits(fields.len));
        var surface = self.language();
        const allocated = try surface.tuple(fields);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn tupleLength(self: *Runtime, block_value: Value) !usize {
        var surface = self.language();
        return surface.tupleLength(block_value);
    }

    pub fn allocString(self: *Runtime, bytes: []const u8) !Value {
        self.prepareAllocation(.string, compatStringUnits(bytes.len));
        var surface = self.language();
        const allocated = try surface.allocString(bytes);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocStringWithFill(self: *Runtime, len: usize, fill: u8) !Value {
        self.prepareAllocation(.string, compatStringUnits(len));
        var surface = self.language();
        const allocated = try surface.allocStringWithFill(len, fill);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocStringWithInit(self: *Runtime, len: usize, initial_bytes: []const u8) !Value {
        self.prepareAllocation(.string, compatStringUnits(len));
        var surface = self.language();
        const allocated = try surface.allocStringWithInit(len, initial_bytes);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocBytes(self: *Runtime, bytes: []const u8) !Value {
        self.prepareAllocation(.string, compatStringUnits(bytes.len));
        var surface = self.language();
        const allocated = try surface.allocBytes(bytes);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocBytesWithFill(self: *Runtime, len: usize, fill: u8) !Value {
        self.prepareAllocation(.string, compatStringUnits(len));
        var surface = self.language();
        const allocated = try surface.allocBytesWithFill(len, fill);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocBytesWithInit(self: *Runtime, len: usize, initial_bytes: []const u8) !Value {
        self.prepareAllocation(.string, compatStringUnits(len));
        var surface = self.language();
        const allocated = try surface.allocBytesWithInit(len, initial_bytes);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocI64(self: *Runtime, n: i64) !Value {
        self.prepareAllocation(.boxed_i64, 1);
        var surface = self.language();
        const allocated = try surface.allocI64(n);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocInt64(self: *Runtime, n: i64) !Value {
        return self.allocI64(n);
    }

    pub fn allocInt32(self: *Runtime, n: i32) !Value {
        return self.allocI32(n);
    }

    pub fn allocI32(self: *Runtime, n: i32) !Value {
        self.prepareAllocation(.boxed_i64, 1);
        var surface = self.language();
        const allocated = try surface.allocI32(n);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocF64(self: *Runtime, number: f64) !Value {
        self.prepareAllocation(.boxed_f64, 1);
        var surface = self.language();
        const allocated = try surface.allocF64(number);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocDouble(self: *Runtime, number: f64) !Value {
        return self.allocF64(number);
    }

    pub fn field(self: *Runtime, block_value: Value, idx: usize) !Value {
        var surface = self.language();
        return surface.field(block_value, idx);
    }

    pub fn setField(self: *Runtime, block_value: Value, idx: usize, next: Value) !void {
        var surface = self.language();
        try surface.setField(block_value, idx, next);
    }

    pub fn setStringBytes(self: *Runtime, block_value: Value, bytes: []const u8) !void {
        var surface = self.language();
        try surface.setStringBytes(block_value, bytes);
    }

    pub fn setBytes(self: *Runtime, block_value: Value, bytes: []const u8) !void {
        var surface = self.language();
        try surface.setBytes(block_value, bytes);
    }

    pub fn stringLength(self: *Runtime, block_value: Value) !usize {
        var surface = self.language();
        return surface.stringLength(block_value);
    }

    pub fn bytesLength(self: *Runtime, block_value: Value) !usize {
        var surface = self.language();
        return surface.bytesLength(block_value);
    }

    pub fn stringSlice(self: *Runtime, block_value: Value) ![]const u8 {
        var surface = self.language();
        return surface.stringSlice(block_value);
    }

    pub fn bytesSlice(self: *Runtime, block_value: Value) ![]const u8 {
        var surface = self.language();
        return surface.bytesSlice(block_value);
    }

    pub fn isString(self: *Runtime, block_value: Value) bool {
        var surface = self.language();
        return surface.isString(block_value);
    }

    pub fn isBytes(self: *Runtime, block_value: Value) bool {
        var surface = self.language();
        return surface.isBytes(block_value);
    }

    pub fn unboxI64(self: *Runtime, boxed_value: Value) !i64 {
        var surface = self.language();
        return surface.unboxI64(boxed_value);
    }

    pub fn unboxF64(self: *Runtime, boxed_value: Value) !f64 {
        var surface = self.language();
        return surface.unboxF64(boxed_value);
    }

    pub fn parseF64(self: *Runtime, literal: []const u8) !Value {
        self.prepareAllocation(.boxed_f64, 1);
        var surface = self.language();
        const allocated = try surface.parseF64(literal);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn formatF64(self: *Runtime, boxed_value: Value, buffer: []u8) ![]const u8 {
        var surface = self.language();
        return surface.formatF64(boxed_value, buffer);
    }

    /// Escape hatch for interop code that already owns a stable `Value` slot.
    /// Prefer `beginRootFrame()` for ordinary runtime code.
    pub fn registerInteropRoot(self: *Runtime, slot: *const Value) !void {
        try self.root_registry.register(slot);
    }

    pub fn registerRoot(self: *Runtime, slot: *const Value) !void {
        try self.registerInteropRoot(slot);
    }

    /// Escape hatch for interop code that already owns a stable `Value` slot.
    /// Prefer `beginRootFrame()` for ordinary runtime code.
    pub fn scopedInteropRoot(self: *Runtime, slot: *const Value) !RootHandle {
        return self.root_registry.scoped(slot);
    }

    pub fn scopedRoot(self: *Runtime, slot: *const Value) !RootHandle {
        return self.scopedInteropRoot(slot);
    }

    pub fn beginRootFrame(self: *Runtime) RootFrame {
        return self.root_registry.beginFrame();
    }

    /// Escape hatch for interop code that already owns a stable `Value` slot.
    /// Prefer `RootFrame.end()` for ordinary runtime code.
    pub fn unregisterInteropRoot(self: *Runtime, slot: *const Value) void {
        self.root_registry.unregister(slot);
    }

    pub fn unregisterRoot(self: *Runtime, slot: *const Value) void {
        self.unregisterInteropRoot(slot);
    }

    pub fn registerNamedValue(self: *Runtime, name: []const u8, rooted: Value) !void {
        try self.services.registerNamedValue(name, rooted);
    }

    pub fn lookupNamedValue(self: *Runtime, name: []const u8) ?Value {
        return self.services.lookupNamedValue(name);
    }

    pub fn enterBlockingSection(self: *Runtime) !void {
        _ = try self.drainConfiguredPendingActions(.blocking_enter);
        self.services.enterBlockingSection();
        try self.domains.enterBlocking(self.currentDomain());
    }

    pub fn exitBlockingSection(self: *Runtime) !void {
        try self.services.exitBlockingSection();
        try self.domains.exitBlocking(self.currentDomain());
        _ = try self.drainConfiguredPendingActions(.blocking_exit);
    }

    pub fn recordSignal(self: *Runtime, signo: u8) !void {
        try self.services.recordSignal(signo);
    }

    pub fn pendingSignalBits(self: *Runtime) u64 {
        return self.services.pendingSignalBits();
    }

    pub fn takePendingSignals(self: *Runtime) u64 {
        return self.services.takePendingSignals();
    }

    pub fn registerSignalHandler(self: *Runtime, signo: u8, handler: Value) !void {
        try self.services.registerSignalHandler(signo, handler);
    }

    pub fn unregisterSignalHandler(self: *Runtime, signo: u8) !void {
        try self.services.unregisterSignalHandler(signo);
    }

    pub fn lookupSignalHandler(self: *Runtime, signo: u8) ?Value {
        return self.services.lookupSignalHandler(signo);
    }

    pub fn installSignalIngress(self: *Runtime, signo: u8) !void {
        try self.services.installSignalIngress(signo);
    }

    pub fn uninstallSignalIngress(self: *Runtime, signo: u8) !bool {
        return self.services.uninstallSignalIngress(signo);
    }

    pub fn enableAlternateSignalStack(self: *Runtime, requested_size: ?usize) !void {
        try self.services.enableAlternateSignalStack(requested_size);
    }

    pub fn disableAlternateSignalStack(self: *Runtime) !void {
        try self.services.disableAlternateSignalStack();
    }

    pub fn signalIngressSnapshot(self: *Runtime) SignalIngressSnapshot {
        return self.services.signalIngressSnapshot();
    }

    pub fn raiseSignal(self: *Runtime, signo: u8) !void {
        try self.services.raiseSignal(signo);
    }

    pub fn createWeakRef(self: *Runtime, target: ?Value) !WeakRefHandle {
        return self.liveness.createWeakRef(target);
    }

    pub fn weakGet(self: *Runtime, handle: WeakRefHandle) !?Value {
        return self.liveness.weakGet(handle);
    }

    pub fn weakSet(self: *Runtime, handle: WeakRefHandle, target: ?Value) !void {
        try self.liveness.weakSet(handle, target);
    }

    pub fn createEphemeron(self: *Runtime, keys: []const Value, data: ?Value) !EphemeronHandle {
        return self.liveness.createEphemeron(keys, data);
    }

    pub fn ephemeronData(self: *Runtime, handle: EphemeronHandle) !?Value {
        return self.liveness.ephemeronData(handle);
    }

    pub fn ephemeronSetData(self: *Runtime, handle: EphemeronHandle, data: ?Value) !void {
        try self.liveness.ephemeronSetData(handle, data);
    }

    pub fn registerFinalizer(self: *Runtime, target: Value, callback: Value, mode: FinalizerMode) !FinalizerHandle {
        return self.liveness.registerFinalizer(target, callback, mode);
    }

    pub fn drainReadyFinalizers(self: *Runtime, allocator: std.mem.Allocator) ![]ReadyFinalizer {
        return self.liveness.drainReadyFinalizers(allocator);
    }

    pub fn pendingActionCount(self: *Runtime) usize {
        return @popCount(self.services.pendingSignalBits()) + self.liveness.readyFinalizerCount();
    }

    pub fn hasPendingActions(self: *Runtime) bool {
        return self.pendingActionCount() != 0;
    }

    pub fn captureCurrentBacktrace(self: *Runtime, allocator: std.mem.Allocator) ![]control_kernel_mod.BacktraceFrame {
        return self.control_kernel.captureBacktrace(allocator, null);
    }

    pub fn captureContinuationBacktrace(
        self: *Runtime,
        allocator: std.mem.Allocator,
        handle: ContinuationHandle,
    ) ![]control_kernel_mod.BacktraceFrame {
        return self.control_kernel.captureContinuationBacktrace(allocator, handle);
    }

    pub fn snapshotContinuationStack(
        self: *Runtime,
        allocator: std.mem.Allocator,
        handle: ContinuationHandle,
    ) !SuspendedStack {
        return self.control_kernel.snapshotContinuationStack(allocator, handle);
    }

    pub fn explainValue(self: *Runtime, block_value: Value, trace: ?*const TraceRecorder) Error!ObjectExplain {
        const obj = self.objectFrom(block_value) orelse return Error.InvalidValue;
        const handle = block_value.asHeapRef() orelse return Error.InvalidValue;
        const metrics = obj.sizeMetrics();
        return .{
            .handle = handle,
            .kind = obj.kind().?,
            .space = self.heap_store.spaceOf(handle).?,
            .payload_bytes = metrics.payload_bytes,
            .storage_bytes = metrics.storage_bytes,
            .scan_words = metrics.scan_words,
            .allocation_cost_units = metrics.allocation_cost_units,
            .explicit_roots = self.root_registry.ownerCount(block_value),
            .control_roots = self.control_kernel.ownedRootCount(block_value),
            .service_roots = self.services.ownerCount(block_value),
            .liveness_roots = self.liveness.ownerCount(block_value),
            .remembered_targets = self.remembered_set.ownerCount(handle),
            .memprof_sample = self.memprof.sampleFor(handle),
            .last_event = if (trace) |recorder| recorder.lastObjectEvent(handle) else null,
        };
    }

    pub const VerifyError = heap_store.HeapStore.VerifyError || control_kernel_mod.ControlKernel.VerifyError || domain_registry_mod.DomainRegistry.VerifyError || fiber_scheduler_mod.FiberScheduler.VerifyError || stw_coordinator_mod.StopTheWorldCoordinator.VerifyError || error{
        OrphanFiber,
        InvalidScheduledFiber,
        ScheduledFiberDomainMismatch,
    };

    pub fn verifyDebugState(self: *Runtime) VerifyError!void {
        if (self.debug_root_checks or self.debug_checks.verify_roots) self.verifyRoots();
        try self.domains.verify();
        try self.fiber_scheduler.verify();
        try self.stw.verify();
        try self.verifyScheduledFiberOwnership();
        if (self.debug_checks.verify_heap_store) try self.heap_store.verifyInvariants();
        if (self.debug_checks.verify_control_kernel) try self.control_kernel.verify(self, isValidRootedValue);
    }

    pub fn collect(self: *Runtime) void {
        if (self.gc_strategy == .generational) {
            self.collectMinor();
            return;
        }
        self.collectMajor();
    }

    pub fn collectMinor(self: *Runtime) void {
        self.enforceFiberOwnership();
        if (self.debug_root_checks or self.debug_checks.verify_roots) self.verifyRoots();
        _ = self.requestStopTheWorld() catch unreachable;
        defer self.resumeTheWorld();
        self.quiesceAttachedDomainsForCollection();
        self.collect_generations +%= 1;
        self.minor_collect_generations +%= 1;
        var providers_buffer: [5]RootProvider = undefined;
        const providers = self.fillRootProviders(&providers_buffer);
        var gc = self.collectorWithProviders(providers);
        if (self.gc_strategy == .generational) {
            gc.collectMinor();
        } else {
            gc.collect();
        }
        if (self.debug_checks.verify_after_collect) {
            self.verifyDebugState() catch |err| std.debug.panic("zort: debug verification failed: {s}", .{@errorName(err)});
        }
    }

    pub fn collectMajor(self: *Runtime) void {
        self.enforceFiberOwnership();
        if (self.debug_root_checks or self.debug_checks.verify_roots) self.verifyRoots();
        _ = self.requestStopTheWorld() catch unreachable;
        defer self.resumeTheWorld();
        self.quiesceAttachedDomainsForCollection();
        self.collect_generations +%= 1;
        var providers_buffer: [5]RootProvider = undefined;
        const providers = self.fillRootProviders(&providers_buffer);
        var gc = self.collectorWithProviders(providers);
        gc.collectMajor();
        if (self.debug_checks.verify_after_collect) {
            self.verifyDebugState() catch |err| std.debug.panic("zort: debug verification failed: {s}", .{@errorName(err)});
        }
    }

    pub fn deliverPendingActions(
        self: *Runtime,
        context: anytype,
        comptime deliver: fn (@TypeOf(context), PendingAction) anyerror!void,
    ) !usize {
        return self.deliverPendingActionsAtCheckpoint(context, deliver, .explicit);
    }

    fn deliverPendingActionsAtCheckpoint(
        self: *Runtime,
        context: anytype,
        comptime deliver: fn (@TypeOf(context), PendingAction) anyerror!void,
        checkpoint: PendingActionCheckpoint,
    ) !usize {
        const current = self.control_kernel.currentFiber();
        var delivered: usize = 0;

        while (self.services.nextPendingSignal()) |signo| {
            const handler = self.services.lookupSignalHandler(signo) orelse {
                _ = try self.services.clearPendingSignal(signo);
                continue;
            };
            {
                try self.control_kernel.enterCallbackBoundary(current);
                defer self.control_kernel.exitCallbackBoundary(current) catch unreachable;
                try deliver(context, .{ .signal = .{
                    .signo = signo,
                    .handler = handler,
                } });
            }
            _ = try self.services.clearPendingSignal(signo);
            delivered += 1;
        }

        while (self.liveness.peekReadyFinalizer()) |ready| {
            {
                try self.control_kernel.enterCallbackBoundary(current);
                defer self.control_kernel.exitCallbackBoundary(current) catch unreachable;
                try deliver(context, .{ .finalizer = ready });
            }
            std.debug.assert(self.liveness.acknowledgeReadyFinalizer(ready.handle));
            delivered += 1;
        }

        _ = checkpoint;
        return delivered;
    }

    fn drainConfiguredPendingActions(self: *Runtime, checkpoint: PendingActionCheckpoint) !usize {
        if (!self.pending_action_delivery.configured()) return 0;
        if (!self.hasPendingActions()) return 0;
        if (self.draining_pending_actions) return 0;

        self.draining_pending_actions = true;
        defer self.draining_pending_actions = false;

        const Delivery = struct {
            delivery: PendingActionDelivery,
            checkpoint: PendingActionCheckpoint,

            fn visit(delivery_ctx: *@This(), action: PendingAction) !void {
                const deliver_fn = delivery_ctx.delivery.deliver_fn.?;
                try deliver_fn(delivery_ctx.delivery.ctx, delivery_ctx.checkpoint, action);
            }
        };

        var delivery = Delivery{
            .delivery = self.pending_action_delivery,
            .checkpoint = checkpoint,
        };
        return self.deliverPendingActionsAtCheckpoint(&delivery, Delivery.visit, checkpoint);
    }

    fn trackAllocationSample(self: *Runtime, block_value: Value) void {
        if (!self.memprof.enabled()) return;
        const handle = block_value.asHeapRef() orelse return;
        const obj = self.objectFrom(block_value) orelse return;
        const metrics = obj.sizeMetrics();
        const sample_ordinal = self.memprof.beginAllocation(metrics.allocation_cost_units) orelse return;
        const kind = obj.kind() orelse return;
        const space = self.heap_store.spaceOf(handle) orelse return;

        if (!self.memprof.capturesBacktraces()) {
            self.memprof.recordAllocation(
                sample_ordinal,
                handle,
                kind,
                metrics.payload_bytes,
                metrics.storage_bytes,
                metrics.scan_words,
                metrics.allocation_cost_units,
                space,
                &.{},
            );
            return;
        }

        const frames = self.control_kernel.captureBacktrace(self.allocator, null) catch {
            self.memprof.recordAllocation(
                sample_ordinal,
                handle,
                kind,
                metrics.payload_bytes,
                metrics.storage_bytes,
                metrics.scan_words,
                metrics.allocation_cost_units,
                space,
                &.{},
            );
            return;
        };
        defer self.allocator.free(frames);

        const sites = self.allocator.alloc(u32, frames.len) catch {
            self.memprof.recordAllocation(
                sample_ordinal,
                handle,
                kind,
                metrics.payload_bytes,
                metrics.storage_bytes,
                metrics.scan_words,
                metrics.allocation_cost_units,
                space,
                &.{},
            );
            return;
        };
        defer self.allocator.free(sites);

        for (frames, 0..) |frame, index| {
            sites[index] = frame.site_id;
        }
        self.memprof.recordAllocation(
            sample_ordinal,
            handle,
            kind,
            metrics.payload_bytes,
            metrics.storage_bytes,
            metrics.scan_words,
            metrics.allocation_cost_units,
            space,
            sites,
        );
    }

    fn prepareCompatAllocation(self: *Runtime, arity: usize, tag: Tag) void {
        const compat_allocation = switch (tag) {
            .tuple => CompatAllocation{ .kind = .tuple, .allocation_units = @max(arity, 1) },
            .string => CompatAllocation{ .kind = .string, .allocation_units = compatStringUnits(arity) },
            .int64 => CompatAllocation{ .kind = .boxed_i64, .allocation_units = 1 },
            .double => CompatAllocation{ .kind = .boxed_f64, .allocation_units = 1 },
            .custom => CompatAllocation{ .kind = .custom, .allocation_units = bytesToAllocationUnits(arity) },
        };
        self.prepareAllocation(compat_allocation.kind, compat_allocation.allocation_units);
    }

    fn prepareAllocation(self: *Runtime, kind: ObjectKind, allocation_units: usize) void {
        if (self.gc_strategy != .generational) return;
        if (kind == .custom) return;
        if (allocation_units > self.heap_store.nursery_config.max_object_units) return;
        if (!self.heap_store.shouldCollectBeforeNurseryAlloc(allocation_units)) return;
        self.collectMinor();
    }

    const CompatAllocation = struct {
        kind: ObjectKind,
        allocation_units: usize,
    };

    fn tupleAllocationUnits(len: usize) usize {
        return @max(len, 1);
    }

    fn compatStringUnits(len: usize) usize {
        return bytesToAllocationUnits(len + 1);
    }

    fn bytesToAllocationUnits(byte_count: usize) usize {
        if (byte_count == 0) return 1;
        return std.math.divCeil(usize, byte_count, @sizeOf(usize)) catch unreachable;
    }

    fn quiesceAttachedDomainsForCollection(self: *Runtime) void {
        const Pause = struct {
            runtime: *Runtime,

            fn visit(ctx: *@This(), domain: DomainHandle) void {
                _ = ctx.runtime.enterSafepoint(domain) catch unreachable;
            }
        };

        var pause_ctx = Pause{ .runtime = self };
        self.domains.visitAttached(&pause_ctx, Pause.visit);
        std.debug.assert(self.stw.allPaused());
    }

    fn objectFrom(self: *Runtime, block_value: Value) ?*Object {
        const handle = block_value.asHeapRef() orelse return null;
        return self.heap_store.get(handle);
    }

    fn verifyRoots(self: *Runtime) void {
        self.root_registry.verify(self, isValidRootedValue);
    }

    fn fillRootProviders(self: *Runtime, buffer: []RootProvider) []const RootProvider {
        std.debug.assert(buffer.len >= 5);
        buffer[0] = self.root_registry.provider();
        buffer[1] = self.schedulerFiberProvider();
        buffer[2] = self.control_kernel.continuationProvider();
        buffer[3] = self.services.provider();
        buffer[4] = self.liveness.provider();
        return buffer[0..5];
    }

    fn currentAllocator(self: *Runtime) std.mem.Allocator {
        // Derive the allocator from the live fixed-arena state so the runtime
        // never holds an allocator interface tied to a pre-move stack address.
        if (self.fixed_arena) |*arena| return arena.allocator();
        return self.allocator;
    }

    fn schedulerFiberProvider(self: *Runtime) RootProvider {
        return .{
            .name = "fiber_scheduler",
            .ctx = self,
            .count_fn = countSchedulerFiberRoots,
            .visit_fn = visitSchedulerFiberRoots,
        };
    }

    fn isValidRootedValue(self: *Runtime, rooted: Value) bool {
        return self.objectFrom(rooted) != null;
    }

    fn countSchedulerFiberRoots(ctx: ?*anyopaque) usize {
        const self: *Runtime = @ptrCast(@alignCast(ctx.?));
        const Count = struct {
            runtime: *Runtime,
            total: usize = 0,

            fn visit(context: *@This(), _: DomainHandle, fiber: FiberHandle) void {
                context.total += context.runtime.control_kernel.fiberRootCount(fiber);
            }
        };

        var count = Count{ .runtime = self };
        self.fiber_scheduler.visitOwnedFibers(&count, Count.visit);
        return count.total;
    }

    fn visitSchedulerFiberRoots(ctx: ?*anyopaque, visitor: RootVisitor) void {
        const self: *Runtime = @ptrCast(@alignCast(ctx.?));
        const Visit = struct {
            runtime: *Runtime,
            visitor: RootVisitor,

            fn visit(context: *@This(), _: DomainHandle, fiber: FiberHandle) void {
                context.runtime.control_kernel.visitFiberRoots(fiber, context.visitor);
            }
        };

        var visit_ctx = Visit{
            .runtime = self,
            .visitor = visitor,
        };
        self.fiber_scheduler.visitOwnedFibers(&visit_ctx, Visit.visit);
    }

    fn verifyScheduledFiberOwnership(self: *Runtime) VerifyError!void {
        const Verify = struct {
            runtime: *Runtime,
            error_state: ?VerifyError = null,

            fn visit(context: *@This(), domain: DomainHandle, fiber: FiberHandle) void {
                if (context.error_state != null) return;
                const fiber_state = context.runtime.control_kernel.fiber(fiber) orelse {
                    context.error_state = error.InvalidScheduledFiber;
                    return;
                };
                if (fiber_state.domain.index != domain.index or fiber_state.domain.generation != domain.generation) {
                    context.error_state = error.ScheduledFiberDomainMismatch;
                }
            }
        };

        var verify = Verify{ .runtime = self };
        self.fiber_scheduler.visitOwnedFibers(&verify, Verify.visit);
        if (verify.error_state) |err| return err;

        const Orphans = struct {
            runtime: *Runtime,
            error_state: ?VerifyError = null,

            fn visit(context: *@This(), fiber: FiberHandle, fiber_state: *const control_kernel_mod.FiberState) void {
                if (context.error_state != null) return;
                if (fiber_state.status == .completed) return;
                if (context.runtime.fiber_scheduler.ownsFiber(fiber)) return;
                context.error_state = error.OrphanFiber;
            }
        };

        var orphans = Orphans{ .runtime = self };
        self.control_kernel.visitFibers(&orphans, Orphans.visit);
        if (orphans.error_state) |err| return err;
    }

    fn enforceFiberOwnership(self: *Runtime) void {
        self.verifyScheduledFiberOwnership() catch |err| {
            std.debug.panic("zort: strict fiber ownership failed: {s}", .{@errorName(err)});
        };
    }

    fn processWeakHook(ctx: ?*anyopaque, collector: *Collector) usize {
        const liveness: *ManagedLiveness = @ptrCast(@alignCast(ctx.?));
        return liveness.processWeak(collector);
    }

    fn processFinalizersHook(ctx: ?*anyopaque, collector: *Collector) usize {
        const liveness: *ManagedLiveness = @ptrCast(@alignCast(ctx.?));
        return liveness.processFinalizers(collector);
    }
};

test "runtime: tuple allocation and fields" {
    var runtime = Runtime.init(std.testing.allocator);
    const tuple = try runtime.allocTuple(3);
    try runtime.setField(tuple, 0, Value.fromInt(1));
    try runtime.setField(tuple, 1, Value.fromInt(2));
    try runtime.setField(tuple, 2, Value.fromInt(3));

    try std.testing.expectEqual(Value.fromInt(1), try runtime.field(tuple, 0));
    try std.testing.expectEqual(Value.fromInt(2), try runtime.field(tuple, 1));
    try std.testing.expectEqual(Value.fromInt(3), try runtime.field(tuple, 2));
    try std.testing.expectEqual(@as(usize, 1), runtime.objectCount());

    runtime.deinit();
}

test "runtime: tuple storage initializes to zero immediates" {
    var rt = Runtime.init(std.testing.allocator);
    const tuple = try rt.allocTuple(4);
    var i: usize = 0;
    while (i < 4) : (i += 1) {
        try std.testing.expectEqual(Value.fromInt(0), try rt.field(tuple, i));
    }
    rt.deinit();
}

test "runtime: tuple helper initializes from values" {
    var rt = Runtime.init(std.testing.allocator);
    const tuple = try rt.tuple(&.{ Value.fromInt(1), Value.fromInt(2), Value.fromInt(3) });

    try std.testing.expectEqual(Value.fromInt(1), try rt.field(tuple, 0));
    try std.testing.expectEqual(Value.fromInt(2), try rt.field(tuple, 1));
    try std.testing.expectEqual(Value.fromInt(3), try rt.field(tuple, 2));
    rt.deinit();
}

test "runtime: strings keep byte length and sentinel behavior" {
    var rt = Runtime.init(std.testing.allocator);
    const string = try rt.allocString("abc");
    try std.testing.expectEqual(@as(usize, 3), try rt.stringLength(string));
    try std.testing.expect(std.mem.eql(u8, try rt.stringSlice(string), "abc"));
    const obj = rt.objectFrom(string).?;
    const with_null = obj.stringBuffer().?;
    try std.testing.expectEqual(@as(u8, 0), with_null[with_null.len - 1]);
    rt.deinit();
}

test "runtime: boxed immediates are stored as heap values" {
    var rt = Runtime.init(std.testing.allocator);
    const int64_value = try rt.allocInt64(1234);
    const double_value = try rt.allocDouble(12.5);
    try std.testing.expect(!int64_value.isImmediate());
    try std.testing.expect(!double_value.isImmediate());
    try std.testing.expectError(Error.InvalidValue, rt.stringLength(int64_value));
    try std.testing.expectError(Error.InvalidValue, rt.stringLength(double_value));
    rt.deinit();
}

test "runtime: boxed constructors have canonical names" {
    var rt = Runtime.init(std.testing.allocator);
    const i32_via_new = try rt.allocI32(-17);
    const i64_via_new = try rt.allocI64(1234);
    const f64_via_new = try rt.allocF64(12.375);

    try std.testing.expect(!i32_via_new.isImmediate());
    try std.testing.expect(!i64_via_new.isImmediate());
    try std.testing.expect(!f64_via_new.isImmediate());
    try std.testing.expectEqual(@as(?ObjectKind, .boxed_i64), rt.objectFrom(i32_via_new).?.kind());
    try std.testing.expectEqual(@as(?ObjectKind, .boxed_i64), rt.objectFrom(i64_via_new).?.kind());
    try std.testing.expectEqual(@as(?ObjectKind, .boxed_f64), rt.objectFrom(f64_via_new).?.kind());
    try std.testing.expectEqual(@as(i64, -17), rt.objectFrom(i32_via_new).?.boxedI64().?);
    try std.testing.expectEqual(@as(i64, 1234), rt.objectFrom(i64_via_new).?.boxedI64().?);
    try std.testing.expectEqual(@as(f64, 12.375), rt.objectFrom(f64_via_new).?.boxedF64().?);

    const i32_compat = try rt.allocInt32(-17);
    const f64_compat = try rt.allocDouble(12.375);
    try std.testing.expectEqual(@as(?ObjectKind, .boxed_i64), rt.objectFrom(i32_compat).?.kind());
    try std.testing.expectEqual(@as(?ObjectKind, .boxed_f64), rt.objectFrom(f64_compat).?.kind());
    rt.deinit();
}

test "runtime: bytes helpers share string representation" {
    var rt = Runtime.init(std.testing.allocator);
    const bytes = try rt.allocBytes("abc");

    try std.testing.expect(rt.isBytes(bytes));
    try std.testing.expect(rt.isString(bytes));
    try std.testing.expectEqual(@as(usize, 3), try rt.bytesLength(bytes));
    try std.testing.expectEqualSlices(u8, "abc", try rt.bytesSlice(bytes));

    try rt.setBytes(bytes, "xy");
    try std.testing.expectEqualSlices(u8, "xy\x00", try rt.bytesSlice(bytes));
    rt.deinit();
}

test "runtime: float parse and format use semantic API" {
    var rt = Runtime.init(std.testing.allocator);
    const parsed = try rt.parseF64("1_23.5");
    try std.testing.expectApproxEqRel(@as(f64, 123.5), try rt.unboxF64(parsed), 1e-12);

    var buffer: [64]u8 = undefined;
    try std.testing.expectEqualSlices(u8, "123.5", try rt.formatF64(parsed, &buffer));
    try std.testing.expectError(Error.InvalidFloatLiteral, rt.parseF64("12.5ms"));
    rt.deinit();
}

test "runtime: string checks for isString and stringSlice failures" {
    var rt = Runtime.init(std.testing.allocator);
    const string = try rt.allocString("x");
    const tuple = try rt.allocTuple(0);
    try std.testing.expect(rt.isString(string));
    try std.testing.expect(!rt.isString(tuple));
    try std.testing.expectError(Error.InvalidValue, rt.stringSlice(tuple));
    rt.deinit();
}

test "runtime: allocStringWithInit rejects oversize input" {
    var rt = Runtime.init(std.testing.allocator);
    const result = rt.allocStringWithInit(3, "abcdef");
    try std.testing.expectError(Error.OutOfMemory, result);
    rt.deinit();
}

test "runtime: allocStringWithFill writes explicit pattern" {
    var rt = Runtime.init(std.testing.allocator);
    const string = try rt.allocStringWithFill(5, 'x');
    const content = try rt.stringSlice(string);
    try std.testing.expect(std.mem.eql(u8, content, "xxxxx"));
    rt.deinit();
}

test "runtime: setStringBytes preserves null-termination" {
    var rt = Runtime.init(std.testing.allocator);
    const string = try rt.allocStringWithFill(5, 0);
    try rt.setStringBytes(string, "hi");
    const bytes = rt.objectFrom(string).?.stringBuffer().?;
    try std.testing.expect(std.mem.eql(u8, bytes[0..2], "hi"));
    try std.testing.expectEqual(@as(u8, 0), bytes[2]);
    try std.testing.expectEqual(@as(u8, 0), bytes[bytes.len - 1]);
    rt.deinit();
}

test "runtime: deep tuple chain is retained and reclaimed" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const depth = 1_024;
    var frame = rt.beginRootFrame();
    var head = try frame.bind(try rt.allocTuple(1));
    var current = head.get();
    var i: usize = 1;

    while (i < depth) : (i += 1) {
        const next = try rt.allocTuple(1);
        try rt.setField(current, 0, next);
        current = next;
    }

    try rt.setField(current, 0, Value.fromInt(1234));

    rt.collect();
    try std.testing.expectEqual(@as(usize, depth), rt.objectCount());

    frame.end();
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
}

test "runtime: shared object graph keeps object alive across multiple parents" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const shared = try rt.allocTuple(1);
    try rt.setField(shared, 0, Value.fromInt(0));

    const left = try rt.allocTuple(2);
    try rt.setField(left, 0, shared);
    try rt.setField(left, 1, Value.fromInt(1));

    const right = try rt.allocTuple(2);
    try rt.setField(right, 0, shared);
    try rt.setField(right, 1, Value.fromInt(2));

    var frame = rt.beginRootFrame();
    var left_root = try frame.bind(left);
    var right_root = try frame.bind(right);

    rt.collect();
    try std.testing.expectEqual(@as(usize, 3), rt.objectCount());

    try rt.setField(shared, 0, Value.fromInt(77));

    const shared_from_left = try rt.field(left_root.get(), 0);
    const shared_from_right = try rt.field(right_root.get(), 0);
    try std.testing.expectEqual(shared, shared_from_left);
    try std.testing.expectEqual(shared, shared_from_right);
    try std.testing.expectEqual(Value.fromInt(77), try rt.field(shared_from_left, 0));
    try std.testing.expectEqual(Value.fromInt(77), try rt.field(shared_from_right, 0));

    left_root.set(Value.fromInt(0));
    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    right_root.set(Value.fromInt(0));
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    frame.end();
}

test "runtime: cyclic graph is marked without recursion blowup" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const first = try rt.allocTuple(1);
    const second = try rt.allocTuple(1);

    try rt.setField(first, 0, second);
    try rt.setField(second, 0, first);

    var frame = rt.beginRootFrame();
    _ = try frame.bind(first);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    frame.end();
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
}

test "runtime: root generation counters track registration activity" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{ .debugRootChecks = true });
    const initial = rt.rootStats();
    try std.testing.expectEqual(@as(usize, 0), initial.root_generation);
    try std.testing.expectEqual(@as(usize, 0), initial.root_registrations);
    try std.testing.expectEqual(@as(usize, 0), initial.root_unregistrations);
    try std.testing.expectEqual(@as(usize, 0), initial.collect_generations);

    const tuple = try rt.allocTuple(0);
    try rt.registerRoot(&tuple);
    rt.unregisterRoot(&tuple);

    const tuple_reg = try rt.allocTuple(1);

    try rt.registerRoot(&tuple_reg);
    try rt.registerRoot(&tuple_reg);
    rt.unregisterRoot(&tuple_reg);
    const mid = rt.rootStats();
    try std.testing.expectEqual(@as(usize, 5), mid.root_generation);
    try std.testing.expectEqual(@as(usize, 3), mid.root_registrations);
    try std.testing.expectEqual(@as(usize, 2), mid.root_unregistrations);

    rt.collect();
    const after_collect = rt.rootStats();
    try std.testing.expectEqual(@as(usize, 1), after_collect.collect_generations);

    rt.unregisterRoot(&tuple_reg);
    const final = rt.rootStats();
    try std.testing.expectEqual(@as(usize, 6), final.root_generation);
    try std.testing.expectEqual(@as(usize, 3), final.root_unregistrations);
    rt.deinit();
}

test "runtime: lexical root frames keep values alive without raw slot registration" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    var rooted = try frame.bind(try rt.allocTuple(1));
    const child = try rt.allocTuple(0);
    try rt.setField(rooted.get(), 0, child);

    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    frame.end();
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
}

test "runtime: fixed arena allocation failure leaves runtime consistent" {
    var arena = [_]u8{0} ** 256;
    var rt = Runtime.initWithFixedArena(std.testing.allocator, arena[0..]);

    _ = try rt.allocTuple(1);
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    try std.testing.expectError(Error.OutOfMemory, rt.allocTuple(1_024));
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: default collection strategy is mark-sweep" {
    var rt = Runtime.init(std.testing.allocator);
    try std.testing.expectEqual(Runtime.GcStrategy.mark_sweep, rt.gc_strategy);
    rt.deinit();
}

test "runtime: mark-sweep strategy preserves reachable graph" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();
    var frame = rt.beginRootFrame();
    var root = try frame.bind(try rt.allocTuple(1));
    const child = try rt.allocTuple(1);
    _ = try rt.allocTuple(1);
    try rt.setField(root.get(), 0, child);

    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
    frame.end();
}

test "runtime: bump GC strategy discards rooted objects" {
    var arena = [_]u8{0} ** 256;
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .fixedArena = arena[0..],
        .gcStrategy = .bump,
    });
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    var root = try frame.bind(try rt.allocTuple(1));
    const child = try rt.allocTuple(1);
    _ = try rt.allocTuple(1);
    try rt.setField(root.get(), 0, child);

    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    frame.end();
}

test "runtime: bump GC strategy drops all objects and reuses fixed arena buffer" {
    var arena = [_]u8{0} ** 256;
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .fixedArena = arena[0..],
        .gcStrategy = .bump,
    });

    _ = try rt.allocTuple(2);
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());

    _ = try rt.allocTuple(2);
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: alloc tuple zero arity still has valid header" {
    var rt = Runtime.init(std.testing.allocator);
    const tuple = try rt.allocTuple(0);
    try std.testing.expectError(Error.InvalidValue, rt.field(tuple, 0));
    try std.testing.expectError(Error.InvalidValue, rt.setField(tuple, 0, Value.fromInt(1)));
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: string allocation with zero length includes sentinel" {
    var rt = Runtime.init(std.testing.allocator);
    const string = try rt.allocStringWithFill(0, 'x');
    const obj = rt.objectFrom(string).?;
    const bytes = obj.stringBuffer().?;
    try std.testing.expectEqual(@as(usize, 1), bytes.len);
    try std.testing.expectEqual(@as(u8, 0), bytes[0]);
    try std.testing.expectEqual(@as(usize, 0), try rt.stringLength(string));
    try std.testing.expectEqualSlices(u8, "", try rt.stringSlice(string));
    rt.deinit();
}

test "runtime: setStringBytes accepts exact-size updates without error" {
    var rt = Runtime.init(std.testing.allocator);
    const string = try rt.allocString("test");
    try rt.setStringBytes(string, "zzzz");
    try std.testing.expect(std.mem.eql(u8, try rt.stringSlice(string), "zzzz"));
    rt.deinit();
}

test "runtime: setStringBytes shorter fills with zeros" {
    var rt = Runtime.init(std.testing.allocator);
    const string = try rt.allocStringWithFill(6, 'q');
    try rt.setStringBytes(string, "ok");
    const bytes = rt.objectFrom(string).?.stringBuffer().?;
    try std.testing.expect(std.mem.eql(u8, bytes[0..2], "ok"));
    try std.testing.expectEqual(@as(u8, 0), bytes[2]);
    try std.testing.expectEqual(@as(u8, 0), bytes[3]);
    try std.testing.expectEqual(@as(u8, 0), bytes[bytes.len - 1]);
    rt.deinit();
}

test "runtime: register/unregister roots control liveness" {
    var rt = Runtime.init(std.testing.allocator);
    const shared = try rt.allocTuple(1);
    var first_root = shared;
    var second_root = shared;

    try rt.registerRoot(&first_root);
    try rt.registerRoot(&second_root);
    rt.unregisterRoot(&first_root);

    rt.collect();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    try rt.setField(second_root, 0, Value.fromInt(42));
    try std.testing.expectEqual(Value.fromInt(42), try rt.field(second_root, 0));

    rt.unregisterRoot(&second_root);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: scoped interop root handle keeps and releases liveness" {
    var rt = Runtime.init(std.testing.allocator);
    var rooted = try rt.allocTuple(1);
    var handle = try rt.scopedInteropRoot(&rooted);

    rt.collect();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    handle.deinit();
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: unregistering non-root slot is safe" {
    var rt = Runtime.init(std.testing.allocator);
    var unattached = Value.fromInt(7);
    rt.unregisterRoot(&unattached);
    const tuple = try rt.allocTuple(1);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    _ = tuple;
    rt.deinit();
}

test "runtime: transitive and cyclic graphs are marked" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const left = try rt.allocTuple(2);
    const right = try rt.allocTuple(2);
    try rt.setField(left, 0, right);
    try rt.setField(right, 0, left);
    try rt.setField(left, 1, Value.fromInt(1));
    try rt.setField(right, 1, Value.fromInt(2));

    var frame = rt.beginRootFrame();
    _ = try frame.bind(left);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    frame.end();
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
}

test "runtime: immediate roots are ignored by GC and do not retain blocks" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();
    _ = try rt.allocTuple(1);
    var frame = rt.beginRootFrame();
    _ = try frame.bind(Value.fromInt(1234));
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    frame.end();
}

test "runtime: root slot mutation can drop formerly rooted object" {
    var rt = Runtime.init(std.testing.allocator);
    var slot = try rt.allocTuple(1);
    try rt.registerRoot(&slot);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    slot = Value.fromInt(99);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: operations on non-blocks reject field access" {
    var rt = Runtime.init(std.testing.allocator);
    const imm = Value.fromInt(7);
    const dbl = try rt.allocDouble(3.5);
    const int64_v = try rt.allocInt64(17);

    try std.testing.expectError(Error.InvalidValue, rt.field(imm, 0));
    try std.testing.expectError(Error.InvalidValue, rt.setField(imm, 0, imm));
    try std.testing.expectError(Error.InvalidValue, rt.field(dbl, 1));
    try std.testing.expectError(Error.InvalidValue, rt.setField(dbl, 0, imm));
    try std.testing.expectError(Error.InvalidValue, rt.field(int64_v, 0));

    rt.deinit();
}

test "runtime: allocInt32 and allocDouble produce typed boxed values" {
    var rt = Runtime.init(std.testing.allocator);
    const int32 = try rt.allocI32(-17);
    const int32_alias = try rt.allocInt32(-17);
    const dbl = try rt.allocDouble(12.375);

    const int_obj = rt.objectFrom(int32).?;
    const int_alias_obj = rt.objectFrom(int32_alias).?;
    const dbl_obj = rt.objectFrom(dbl).?;

    try std.testing.expectEqual(@as(?ObjectKind, .boxed_i64), int_obj.kind());
    try std.testing.expectEqual(@as(?ObjectKind, .boxed_i64), int_alias_obj.kind());
    try std.testing.expectEqual(@as(?ObjectKind, .boxed_f64), dbl_obj.kind());
    try std.testing.expectEqual(@as(i64, -17), int_obj.boxedI64().?);
    try std.testing.expectEqual(@as(i64, -17), int_alias_obj.boxedI64().?);
    try std.testing.expectEqual(@as(f64, 12.375), dbl_obj.boxedF64().?);
    rt.deinit();
}

test "runtime: register many roots and unregister all" {
    var rt = Runtime.init(std.testing.allocator);
    var kept: [64]Value = undefined;
    var kept_count: usize = 0;

    for (0..64) |_| {
        const node = try rt.allocTuple(0);
        kept[kept_count] = node;
        kept_count += 1;
    }

    for (kept[0..kept_count]) |*slot| {
        try rt.registerRoot(slot);
    }
    rt.collect();
    try std.testing.expectEqual(@as(usize, 64), rt.objectCount());

    for (kept[0..kept_count]) |*slot| {
        rt.unregisterRoot(slot);
    }
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: invalid field access returns structured errors" {
    var rt = Runtime.init(std.testing.allocator);
    const tuple = try rt.allocTuple(1);
    try std.testing.expectError(Error.InvalidValue, rt.field(tuple, 1));
    try std.testing.expectError(Error.InvalidValue, rt.setField(tuple, 1, Value.fromInt(1)));
    try std.testing.expectError(Error.InvalidValue, rt.setStringBytes(tuple, "x"));
    rt.deinit();
}

test "runtime: setField on immediate fails" {
    var rt = Runtime.init(std.testing.allocator);
    const immediate = Value.fromInt(42);
    try std.testing.expectError(Error.InvalidValue, rt.setField(immediate, 0, Value.fromInt(1)));
    try std.testing.expectError(Error.InvalidValue, rt.field(immediate, 0));
    rt.deinit();
}

test "runtime: transitive marking keeps graph" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();
    var frame = rt.beginRootFrame();
    var root = try frame.bind(try rt.allocTuple(2));
    const child = try rt.allocTuple(1);
    try rt.setField(root.get(), 0, child);
    try rt.setField(root.get(), 1, Value.fromInt(7));
    try rt.setField(child, 0, Value.fromInt(9));

    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
    frame.end();
}

test "runtime: self-referential tuple survives gc" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();
    var frame = rt.beginRootFrame();
    var cyclic = try frame.bind(try rt.allocTuple(1));
    try rt.setField(cyclic.get(), 0, cyclic.get());
    rt.collect();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());
    frame.end();
}

test "runtime: unregisterRoot removes every duplicate at first match" {
    var rt = Runtime.init(std.testing.allocator);
    var tuple = try rt.allocTuple(1);
    try rt.registerRoot(&tuple);
    try rt.registerRoot(&tuple);
    rt.unregisterRoot(&tuple);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());
    rt.deinit();
}

test "runtime: gc collects unreachable values" {
    var runtime = Runtime.init(std.testing.allocator);
    defer runtime.deinit();
    var frame = runtime.beginRootFrame();
    const keep_me = try runtime.allocTuple(1);
    try runtime.setField(keep_me, 0, Value.fromInt(42));
    _ = try frame.bind(keep_me);

    _ = try runtime.allocTuple(1);
    runtime.collect();
    try std.testing.expectEqual(@as(usize, 1), runtime.objectCount());
    frame.end();
}

test "runtime: performed continuations keep heap values alive until resumed" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const handler_value = try rt.allocTuple(0);
    const captured_value = try rt.allocTuple(0);

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = 1,
        .handle_effect = handler_value,
    });

    const child = try rt.spawnFiberInDomain(main, rt.currentDomain());
    try rt.activateFiberInDomain(rt.currentDomain(), child);
    const performed = try rt.performEffect(1, Value.fromInt(0), &.{captured_value});

    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    _ = try rt.popEffectHandler(main);
    _ = try rt.resumeContinuation(performed.continuation, Value.fromInt(123));

    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
}

test "runtime: continuations can migrate across attached domains" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const main_domain = rt.currentDomain();
    const other_domain = try rt.createDomain();
    try std.testing.expectError(error.DomainDetached, rt.createFiberInDomain(null, other_domain));
    try rt.attachDomain(other_domain);
    try std.testing.expectError(error.WorkerNotRunning, rt.createFiberInDomain(null, other_domain));
    try std.testing.expect(try rt.startDomainWorker(other_domain, 6));

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = 6,
        .handle_effect = Value.fromInt(1),
    });

    const child = try rt.createFiberInDomain(main, main_domain);
    try rt.activateFiberInDomain(main_domain, child);
    try rt.pushFiberFrame(child, 606);
    try rt.pushFiberFrameRoot(child, try rt.allocTuple(0));

    const performed = try rt.performEffectAt(606, 6, Value.fromInt(2), &.{});
    const worker = try rt.createFiberInDomain(null, other_domain);
    try rt.activateFiberInDomain(other_domain, worker);

    _ = try rt.resumeContinuation(performed.continuation, Value.fromInt(11));

    try std.testing.expectEqual(other_domain, rt.currentDomain());
    try std.testing.expectEqual(other_domain, rt.controlKernel().fiber(child).?.domain);
}

test "runtime: stack limits configure managed continuation snapshots" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .stackLimits = .{
            .initial_frame_capacity = 1,
            .initial_frame_root_capacity = 1,
            .max_frames = 4,
            .max_frame_roots = 4,
        },
    });
    defer rt.deinit();

    try std.testing.expectEqual(@as(usize, 4), rt.stackLimits().max_frames);

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = 16,
        .handle_effect = Value.fromInt(1),
    });

    const child = try rt.spawnFiberInDomain(main, rt.currentDomain());
    try rt.activateFiberInDomain(rt.currentDomain(), child);
    try rt.pushFiberFrame(child, 1601);
    try rt.pushFiberFrameRoot(child, try rt.allocTuple(0));
    try rt.pushFiberFrame(child, 1602);
    try rt.pushFiberFrameRoot(child, try rt.allocTuple(0));

    const performed = try rt.performEffectAt(1602, 16, Value.fromInt(7), &.{});
    var snapshot = try rt.snapshotContinuationStack(std.testing.allocator, performed.continuation);
    defer snapshot.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(u32, 1602), snapshot.capture_site_id);
    try std.testing.expectEqual(@as(usize, 2), snapshot.frame_count);
    try std.testing.expectEqual(@as(usize, 2), snapshot.root_count);

    _ = try rt.resumeContinuation(performed.continuation, Value.fromInt(8));
    try std.testing.expectError(error.AlreadyResumed, rt.snapshotContinuationStack(std.testing.allocator, performed.continuation));
}

test "runtime: dropping a resumed continuation does not discard the resumed fiber" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const effect: EffectId = 19;
    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = effect,
        .handle_effect = Value.fromInt(1),
    });
    defer _ = rt.popEffectHandler(main) catch {};

    const child = try rt.spawnFiberInDomain(main, rt.currentDomain());
    try rt.activateFiberInDomain(rt.currentDomain(), child);
    try rt.pushFiberFrame(child, 1901);

    const performed = try rt.performEffectAt(1901, effect, Value.fromInt(3), &.{});
    _ = try rt.resumeContinuation(performed.continuation, Value.fromInt(4));
    try std.testing.expect(rt.dropContinuation(performed.continuation));
    try std.testing.expectEqual(child, rt.currentFiber());
    try std.testing.expect(rt.controlKernel().fiber(child) != null);
    _ = try rt.popFiberFrame(child);
    try rt.verifyDebugState();
}

test "runtime: per-domain fiber scheduler queues, parks, and unparks fibers" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const main_domain = rt.currentDomain();
    const first = try rt.spawnFiberInDomain(null, main_domain);
    const second = try rt.spawnFiberInDomain(null, main_domain);

    try std.testing.expectEqual(@as(usize, 2), rt.fiberScheduler().runnableCount(main_domain));
    try std.testing.expectEqual(first, (try rt.scheduleNextFiber(main_domain)).?);
    try std.testing.expectEqual(first, rt.currentFiber());
    try std.testing.expectEqual(@as(usize, 2), rt.fiberScheduler().runnableCount(main_domain));

    try std.testing.expectEqual(second, (try rt.yieldCurrentFiber()).?);
    try std.testing.expectEqual(second, rt.currentFiber());
    try std.testing.expectEqual(@as(usize, 2), rt.fiberScheduler().runnableCount(main_domain));

    try std.testing.expectEqual(FiberHandle{ .index = 0, .generation = 1 }, (try rt.parkCurrentFiber()).?);
    try std.testing.expectEqual(@as(usize, 1), rt.fiberScheduler().parkedCount(main_domain));
    try std.testing.expectEqual(FiberHandle{ .index = 0, .generation = 1 }, rt.currentFiber());

    try rt.unparkFiber(main_domain, second);
    try std.testing.expectEqual(@as(usize, 2), rt.fiberScheduler().runnableCount(main_domain));
    try rt.verifyDebugState();
}

test "runtime: runnable fibers can transfer across running domains" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const source_domain = rt.currentDomain();
    const target_domain = try rt.createDomain();
    try rt.attachDomain(target_domain);
    try std.testing.expect(try rt.startDomainWorker(target_domain, 33));

    const worker = try rt.spawnFiberInDomain(null, source_domain);
    try std.testing.expectEqual(source_domain, rt.controlKernel().fiber(worker).?.domain);
    try std.testing.expect(try rt.transferRunnableFiber(source_domain, target_domain, worker));
    try std.testing.expectEqual(target_domain, rt.controlKernel().fiber(worker).?.domain);
    try std.testing.expectEqual(@as(usize, 0), rt.fiberScheduler().runnableCount(source_domain));
    try std.testing.expectEqual(@as(usize, 1), rt.fiberScheduler().runnableCount(target_domain));
    try std.testing.expect((try rt.schedulerLaneSnapshot(target_domain)).wake_requested);
    try std.testing.expectEqual(worker, (try rt.scheduleNextFiber(target_domain)).?);
    try rt.verifyDebugState();
}

test "runtime: continuations remain migratable across attached domains" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const effect: EffectId = 21;
    const main_domain = rt.currentDomain();
    const other_domain = try rt.createDomain();
    try rt.attachDomain(other_domain);
    try std.testing.expect(try rt.startDomainWorker(other_domain, 55));

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = effect,
        .handle_effect = Value.fromInt(1),
    });
    defer _ = rt.popEffectHandler(main) catch {};

    const child = try rt.spawnFiberInDomain(main, main_domain);
    try rt.activateFiberInDomain(main_domain, child);
    try rt.pushFiberFrame(child, 2101);

    const performed = try rt.performEffectAt(2101, effect, Value.fromInt(9), &.{});
    const worker = try rt.createFiberInDomain(null, other_domain);
    try rt.activateFiberInDomain(other_domain, worker);

    _ = try rt.resumeContinuation(performed.continuation, Value.fromInt(10));
    try std.testing.expectEqual(other_domain, rt.controlKernel().fiber(child).?.domain);
    try std.testing.expect(rt.dropContinuation(performed.continuation));
    try rt.verifyDebugState();
}

test "runtime: scheduler and stw coordination snapshots are explicit" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const main_domain = rt.currentDomain();
    const child = try rt.spawnFiberInDomain(null, main_domain);

    var lane = try rt.schedulerLaneSnapshot(main_domain);
    try std.testing.expectEqual(@as(usize, 1), lane.runnable_count);
    try std.testing.expect(lane.wake_requested);
    try std.testing.expectEqual(@as(?u64, rt.mainWorkerToken()), lane.owner_token);
    try std.testing.expect(try rt.claimSchedulerLane(main_domain, rt.mainWorkerToken()));
    lane = try rt.schedulerLaneSnapshot(main_domain);
    try std.testing.expectEqual(@as(?u64, rt.mainWorkerToken()), lane.owner_token);
    try std.testing.expect(try rt.takeSchedulerWake(main_domain));
    lane = try rt.schedulerLaneSnapshot(main_domain);
    try std.testing.expect(!lane.wake_requested);

    try std.testing.expectEqual(child, (try rt.scheduleNextFiber(main_domain)).?);
    lane = try rt.schedulerLaneSnapshot(main_domain);
    try std.testing.expectEqual(child, lane.current.?);
    try std.testing.expectEqual(@as(usize, 1), lane.runnable_count);

    var stw = rt.stwSnapshot();
    try std.testing.expect(!stw.active);
    const generation = try rt.requestStopTheWorld();
    stw = rt.stwSnapshot();
    try std.testing.expect(stw.active);
    try std.testing.expectEqual(@as(usize, 1), stw.paused_count);
    try std.testing.expectEqual(rt.domainRegistry().attachedCount(), stw.target_pause_count);
    try std.testing.expectEqual(generation, stw.generation);
    rt.resumeTheWorld();
    stw = rt.stwSnapshot();
    try std.testing.expect(!stw.active);
}

test "runtime: domain workers bootstrap and shut down explicitly" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const main_domain = rt.currentDomain();
    try std.testing.expectEqual(@as(?DomainWorkerState, .running), rt.domainWorker(main_domain).?.state);
    try std.testing.expectEqual(@as(?u64, rt.mainWorkerToken()), rt.domainWorker(main_domain).?.owner_token);

    const worker_domain = try rt.createDomain();
    try rt.attachDomain(worker_domain);
    try std.testing.expect(try rt.startDomainWorker(worker_domain, 77));
    try std.testing.expectEqual(@as(?DomainWorkerState, .running), rt.domainWorker(worker_domain).?.state);
    try std.testing.expectEqual(@as(?u64, 77), (try rt.schedulerLaneSnapshot(worker_domain)).owner_token);

    try std.testing.expect(try rt.requestDomainWorkerShutdown(worker_domain, 77));
    try std.testing.expectEqual(@as(?DomainWorkerState, .stopping), rt.domainWorker(worker_domain).?.state);
    try std.testing.expect(try rt.finishDomainWorkerShutdown(worker_domain, 77));
    try std.testing.expectEqual(@as(?DomainWorkerState, .stopped), rt.domainWorker(worker_domain).?.state);
    try std.testing.expectEqual(@as(?u64, null), (try rt.schedulerLaneSnapshot(worker_domain)).owner_token);

    try rt.detachDomain(worker_domain);
}

test "runtime: worker shutdown requires a quiescent scheduler lane" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const worker_domain = try rt.createDomain();
    try rt.attachDomain(worker_domain);
    try std.testing.expect(try rt.startDomainWorker(worker_domain, 88));

    _ = try rt.spawnFiberInDomain(null, worker_domain);
    try std.testing.expect(try rt.requestDomainWorkerShutdown(worker_domain, 88));
    try std.testing.expectError(error.WorkerNotQuiescent, rt.finishDomainWorkerShutdown(worker_domain, 88));
    try std.testing.expectEqual(@as(?DomainWorkerState, .stopping), rt.domainWorker(worker_domain).?.state);
    try std.testing.expectEqual(@as(?u64, 88), (try rt.schedulerLaneSnapshot(worker_domain)).owner_token);
}

test "runtime: parked fibers stay live through scheduler-owned root providers" {
    var trace = TraceRecorder.init(std.testing.allocator, .{});
    defer trace.deinit();

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .eventSink = trace.sink(),
    });
    defer rt.deinit();

    const main_domain = rt.currentDomain();
    const parked = try rt.spawnFiberInDomain(null, main_domain);
    const replacement = try rt.spawnFiberInDomain(null, main_domain);

    try std.testing.expectEqual(parked, (try rt.scheduleNextFiber(main_domain)).?);
    try rt.pushFiberFrame(parked, 701);
    try rt.pushFiberFrameRoot(parked, try rt.allocTuple(0));

    try std.testing.expectEqual(replacement, (try rt.parkCurrentFiber()).?);

    rt.collect();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    const providers = trace.rootProviderEntries();
    const Find = struct {
        fn count(entries: []const RootProviderEvent, name: []const u8) usize {
            for (entries) |entry| {
                if (std.mem.eql(u8, entry.name, name)) return entry.count;
            }
            return 0;
        }
    };
    try std.testing.expect(Find.count(providers, "fiber_scheduler") > 0);
    try std.testing.expectEqual(@as(usize, 0), Find.count(providers, "orphan_fibers"));
}

test "runtime: suspended continuations use dedicated root providers" {
    var trace = TraceRecorder.init(std.testing.allocator, .{});
    defer trace.deinit();

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .eventSink = trace.sink(),
    });
    defer rt.deinit();

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = 9,
        .handle_effect = Value.fromInt(1),
    });

    const child = try rt.spawnFiberInDomain(main, rt.currentDomain());
    try rt.activateFiberInDomain(rt.currentDomain(), child);
    try rt.pushFiberFrame(child, 909);
    try rt.pushFiberFrameRoot(child, try rt.allocTuple(0));
    const payload = try rt.allocTuple(0);

    _ = try rt.performEffectAt(909, 9, payload, &.{});

    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    const providers = trace.rootProviderEntries();
    const Find = struct {
        fn count(entries: []const RootProviderEvent, name: []const u8) usize {
            for (entries) |entry| {
                if (std.mem.eql(u8, entry.name, name)) return entry.count;
            }
            return 0;
        }
    };
    try std.testing.expect(Find.count(providers, "suspended_continuations") > 0);
}

test "runtime: stop-the-world hooks pause attached domains in the single-threaded model" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const other = try rt.createDomain();
    try rt.attachDomain(other);

    const generation = try rt.requestStopTheWorld();
    try std.testing.expectEqual(@as(usize, 1), generation);
    try std.testing.expect(rt.stwCoordinator().isActive());
    try std.testing.expect(rt.stwCoordinator().isPaused(rt.currentDomain()));
    try std.testing.expect(!rt.stwCoordinator().isPaused(other));

    try std.testing.expect(try rt.enterSafepoint(other));
    try std.testing.expectEqual(rt.domainRegistry().attachedCount(), rt.stwCoordinator().pausedCount());
    try std.testing.expect(rt.stwCoordinator().isPaused(other));
    try std.testing.expect(rt.stwCoordinator().allPaused());

    rt.resumeTheWorld();
    try std.testing.expect(!rt.stwCoordinator().isActive());
    try std.testing.expectEqual(@as(usize, 0), rt.stwCoordinator().pausedCount());
}

test "runtime: explainValue reports root ownership and last object event" {
    var trace = TraceRecorder.init(std.testing.allocator, .{
        .track_object_events = true,
    });
    defer trace.deinit();

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .eventSink = trace.sink(),
    });
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    defer frame.end();
    var rooted = try frame.bind(try rt.allocTuple(1));
    const child = try rt.allocTuple(0);
    try rt.setField(rooted.get(), 0, child);
    try rt.registerNamedValue("child", child);

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = 1,
        .handle_effect = child,
    });

    const explained_root = try rt.explainValue(rooted.get(), &trace);
    try std.testing.expectEqual(@as(usize, 1), explained_root.explicit_roots);
    try std.testing.expectEqual(@as(usize, 8), explained_root.payload_bytes);
    try std.testing.expectEqual(@as(usize, 8), explained_root.storage_bytes);
    try std.testing.expectEqual(@as(usize, 1), explained_root.scan_words);
    try std.testing.expectEqual(@as(usize, 1), explained_root.allocation_cost_units);
    try std.testing.expectEqual(@as(usize, 0), explained_root.remembered_targets);
    try std.testing.expectEqual(@as(?event_sink_mod.ObjectLastEvent, .{ .field_write = .{
        .target = explained_root.handle,
        .index = 0,
        .phase = .mutate,
    } }), explained_root.last_event);

    const explained_child = try rt.explainValue(child, &trace);
    try std.testing.expectEqual(@as(usize, 1), explained_child.control_roots);
    try std.testing.expectEqual(@as(usize, 1), explained_child.service_roots);
}

test "runtime: verifyDebugState accepts healthy runtime" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .debugChecks = .{
            .verify_roots = true,
            .verify_heap_store = true,
            .verify_control_kernel = true,
        },
    });
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    defer frame.end();
    _ = try frame.bind(try rt.allocTuple(0));
    try rt.verifyDebugState();
}

test "runtime: direct unscheduled fibers fail strict ownership checks" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const main = rt.currentFiber();
    _ = try rt.control_kernel.createFiber(main);
    try std.testing.expectError(error.OrphanFiber, rt.verifyDebugState());
}

test "runtime: runtime services named values keep blocks alive across collection" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const named = try rt.allocTuple(0);
    try rt.registerNamedValue("persist", named);
    rt.collect();

    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());
    try std.testing.expectEqual(named, rt.lookupNamedValue("persist").?);

    const explained = try rt.explainValue(named, null);
    try std.testing.expectEqual(@as(usize, 1), explained.service_roots);
}

test "runtime: runtime services track pending signals and blocking sections" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    try rt.enterBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .blocked), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try rt.recordSignal(2);
    try rt.recordSignal(7);
    try std.testing.expectEqual((@as(u64, 1) << 2) | (@as(u64, 1) << 7), rt.takePendingSignals());
    try std.testing.expectEqual(@as(u64, 0), rt.takePendingSignals());
    try rt.exitBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .attached), rt.domainRegistry().domain(rt.currentDomain()).?.status);
}

test "runtime: compiled platform caps and runtime permissions are explicit" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .permissions = .{
            .allow_read = true,
            .allow_write = true,
            .allow_hrtime = true,
        },
    });
    defer rt.deinit();

    const caps = rt.platformCaps();
    const permissions = rt.permissions();
    const access = rt.hostAccess();

    try std.testing.expectEqual(permissions.allow_read, true);
    try std.testing.expectEqual(permissions.allow_write, true);
    try std.testing.expectEqual(permissions.allow_hrtime, true);
    try std.testing.expectEqual(access.read, caps.filesystem and permissions.allow_read);
    try std.testing.expectEqual(access.write, caps.filesystem and permissions.allow_write);
    try std.testing.expectEqual(access.net, caps.network and permissions.allow_net);
    try std.testing.expectEqual(access.ffi, caps.native_plugin_loading and permissions.allow_ffi);
}

test "runtime: signal ingress wrappers expose runtime service ownership" {
    if (!runtime_services_mod.supports_native_signal_ingress) return error.SkipZigTest;

    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    const signo: u8 = @intCast(std.posix.SIG.USR1);
    try rt.enableAlternateSignalStack(null);
    try rt.installSignalIngress(signo);

    const snapshot = rt.signalIngressSnapshot();
    try std.testing.expect(snapshot.installed);
    try std.testing.expect(snapshot.owns_alternate_stack);
    try std.testing.expect(snapshot.alternate_stack_size > 0);
    try std.testing.expectEqual((@as(u64, 1) << @intCast(signo)), snapshot.installed_signals);

    try rt.raiseSignal(signo);

    var spins: usize = 0;
    while (rt.pendingSignalBits() == 0 and spins < 1000) : (spins += 1) {
        std.Thread.yield() catch {};
    }
    try std.testing.expectEqual((@as(u64, 1) << @intCast(signo)), rt.pendingSignalBits());

    try std.testing.expect(try rt.uninstallSignalIngress(signo));
    try rt.disableAlternateSignalStack();
    const after = rt.signalIngressSnapshot();
    try std.testing.expect(!after.installed);
    try std.testing.expect(!after.owns_alternate_stack);
}

test "runtime: generational minor collection promotes live nursery objects" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
    });
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    defer frame.end();
    var root = try frame.bind(try rt.allocTuple(1));
    const child = try rt.allocTuple(0);
    _ = try rt.allocTuple(0);
    try rt.setField(root.get(), 0, child);

    try std.testing.expectEqual(@as(?Space, .nursery), rt.objectSpace(root.get()));
    try std.testing.expectEqual(@as(?Space, .nursery), rt.objectSpace(child));
    rt.collectMinor();

    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(root.get()));
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(child));
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
}

test "runtime: nursery tuple payloads stay pinned across promotion" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
    });
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    defer frame.end();
    var tuple = try frame.bind(try rt.allocTuple(2));

    const before_obj = rt.objectFromDebug(tuple.get()).?;
    const before_fields = before_obj.tupleFields().?;
    const before_ptr = @intFromPtr(before_fields.ptr);
    try std.testing.expectEqual(HeapStorageOwner.nursery_page, before_obj.storageOwner().?);

    try rt.setField(tuple.get(), 0, Value.fromInt(1));
    try rt.setField(tuple.get(), 1, Value.fromInt(2));
    rt.collectMinor();

    const after_obj = rt.objectFromDebug(tuple.get()).?;
    const after_fields = after_obj.tupleFields().?;
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(tuple.get()));
    try std.testing.expectEqual(HeapStorageOwner.major_page, after_obj.storageOwner().?);
    try std.testing.expectEqual(before_ptr, @intFromPtr(after_fields.ptr));
    try std.testing.expectEqual(Value.fromInt(1), after_fields[0]);
    try std.testing.expectEqual(Value.fromInt(2), after_fields[1]);
}

test "runtime: nursery pressure triggers minor collection before allocation" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
        .nurseryLiveUnits = 1,
        .nurseryLiveObjects = 1,
    });
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    defer frame.end();
    var root = try frame.bind(try rt.allocTuple(1));
    try std.testing.expectEqual(@as(?Space, .nursery), rt.objectSpace(root.get()));
    try std.testing.expectEqual(@as(u64, 0), rt.rootStats().minor_collect_generations);

    const next = try rt.allocTuple(1);

    try std.testing.expectEqual(@as(u64, 1), rt.rootStats().minor_collect_generations);
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(root.get()));
    try std.testing.expectEqual(@as(?Space, .nursery), rt.objectSpace(next));

    const stats = rt.rootStats();
    try std.testing.expectEqual(@as(usize, 1), stats.nursery_objects);
    try std.testing.expectEqual(@as(usize, 1), stats.nursery_allocation_units);
    try std.testing.expectEqual(@as(usize, 1), stats.major_objects);
    try std.testing.expectEqual(@as(usize, 1), stats.major_allocation_units);
}

test "runtime: remembered targets keep nursery children alive from major parents" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
    });
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    defer frame.end();
    var parent = try frame.bind(try rt.allocTuple(1));
    rt.collectMinor();
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(parent.get()));

    const child = try rt.allocTuple(0);
    try rt.setField(parent.get(), 0, child);
    try std.testing.expectEqual(@as(usize, 1), rt.rememberedSet().count());

    rt.collectMinor();
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(child));
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
}

test "runtime: remembered targets rescan current major fields instead of stale nursery edges" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
    });
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    defer frame.end();
    var parent = try frame.bind(try rt.allocTuple(1));
    rt.collectMinor();
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(parent.get()));

    const first = try rt.allocTuple(0);
    try rt.setField(parent.get(), 0, first);
    const second = try rt.allocTuple(0);
    try rt.setField(parent.get(), 0, second);
    try std.testing.expectEqual(@as(usize, 1), rt.rememberedSet().count());

    rt.collectMinor();
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(second));
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
}

test "runtime: named-value major parents rely on remembered targets during minor gc" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
    });
    defer rt.deinit();

    const parent = try rt.allocTuple(1);
    try rt.registerNamedValue("remembered-parent", parent);
    rt.collectMinor();
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(parent));

    const child = try rt.allocTuple(0);
    try rt.setField(parent, 0, child);
    try std.testing.expectEqual(@as(usize, 1), rt.rememberedSet().count());

    rt.collectMinor();
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(child));
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
}

test "runtime: weak refs and ephemerons follow collector liveness" {
    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    var frame = rt.beginRootFrame();
    var key = try frame.bind(try rt.allocTuple(0));
    const data = try rt.allocTuple(0);
    const weak = try rt.createWeakRef(data);
    const ephemeron = try rt.createEphemeron(&.{key.get()}, data);

    rt.collectMajor();
    try std.testing.expectEqual(data, (try rt.weakGet(weak)).?);
    try std.testing.expectEqual(data, (try rt.ephemeronData(ephemeron)).?);

    frame.end();
    rt.collectMajor();
    try std.testing.expectEqual(@as(?Value, null), try rt.weakGet(weak));
    try std.testing.expectEqual(@as(?Value, null), try rt.ephemeronData(ephemeron));
}

test "runtime: configured pending actions drain at scheduler safepoints" {
    var trace = TraceRecorder.init(std.testing.allocator, .{
        .capture_events = true,
    });
    defer trace.deinit();

    const Delivery = struct {
        runtime: ?*Runtime = null,
        actions: std.ArrayListUnmanaged(PendingAction) = .{},
        checkpoints: std.ArrayListUnmanaged(PendingActionCheckpoint) = .{},

        fn deliver(ctx: ?*anyopaque, checkpoint: PendingActionCheckpoint, action: PendingAction) !void {
            const self: *@This() = @ptrCast(@alignCast(ctx.?));
            try self.actions.append(std.testing.allocator, action);
            try self.checkpoints.append(std.testing.allocator, checkpoint);
            if (self.runtime) |rt| {
                try std.testing.expectError(error.UnhandledEffect, rt.performEffectAt(8, 55, Value.fromInt(1), &.{}));
            }
        }
    };

    var delivery = Delivery{};
    defer delivery.actions.deinit(std.testing.allocator);
    defer delivery.checkpoints.deinit(std.testing.allocator);

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .eventSink = trace.sink(),
    });
    defer rt.deinit();
    delivery.runtime = &rt;

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = 55,
        .handle_effect = Value.fromInt(1),
    });

    try rt.registerSignalHandler(2, try rt.allocTuple(0));
    try rt.recordSignal(2);

    const finalizer_target = try rt.allocTuple(0);
    const finalizer_callback = try rt.allocTuple(0);
    _ = try rt.registerFinalizer(finalizer_target, finalizer_callback, .first);
    rt.collectMajor();

    rt.pending_action_delivery = .{
        .ctx = &delivery,
        .deliver_fn = Delivery.deliver,
    };
    try std.testing.expectEqual(@as(usize, 2), rt.pendingActionCount());

    const child = try rt.spawnFiberInDomain(main, rt.currentDomain());
    try rt.activateFiberInDomain(rt.currentDomain(), child);

    try std.testing.expectEqual(@as(usize, 2), delivery.actions.items.len);
    try std.testing.expectEqual(@as(usize, 2), delivery.checkpoints.items.len);
    try std.testing.expectEqual(PendingActionCheckpoint.scheduler_safepoint, delivery.checkpoints.items[0]);
    try std.testing.expectEqual(PendingActionCheckpoint.scheduler_safepoint, delivery.checkpoints.items[1]);
    try std.testing.expect(delivery.actions.items[0] == .signal);
    try std.testing.expect(delivery.actions.items[1] == .finalizer);
    try std.testing.expectEqual(@as(usize, 0), rt.pendingActionCount());

    var callback_entries: usize = 0;
    for (trace.traceEntries()) |entry| {
        if (entry.event == .control) {
            if (entry.event.control.action == .callback_enter or entry.event.control.action == .callback_exit) {
                callback_entries += 1;
            }
        }
    }
    try std.testing.expect(callback_entries >= 4);
}

test "runtime: blocking transitions drain configured pending actions deterministically" {
    const Delivery = struct {
        actions: std.ArrayListUnmanaged(PendingAction) = .{},
        checkpoints: std.ArrayListUnmanaged(PendingActionCheckpoint) = .{},

        fn deliver(ctx: ?*anyopaque, checkpoint: PendingActionCheckpoint, action: PendingAction) !void {
            const self: *@This() = @ptrCast(@alignCast(ctx.?));
            try self.actions.append(std.testing.allocator, action);
            try self.checkpoints.append(std.testing.allocator, checkpoint);
        }
    };

    var delivery = Delivery{};
    defer delivery.actions.deinit(std.testing.allocator);
    defer delivery.checkpoints.deinit(std.testing.allocator);

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .pendingActionDelivery = .{
            .ctx = &delivery,
            .deliver_fn = Delivery.deliver,
        },
    });
    defer rt.deinit();

    try rt.registerSignalHandler(2, try rt.allocTuple(0));
    try rt.registerSignalHandler(7, try rt.allocTuple(0));

    try rt.recordSignal(2);
    try rt.enterBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .blocked), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try std.testing.expectEqual(@as(usize, 1), delivery.actions.items.len);
    try std.testing.expectEqual(PendingActionCheckpoint.blocking_enter, delivery.checkpoints.items[0]);
    try std.testing.expectEqual(@as(usize, 0), rt.pendingActionCount());

    try rt.recordSignal(7);
    try rt.exitBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .attached), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try std.testing.expectEqual(@as(usize, 2), delivery.actions.items.len);
    try std.testing.expectEqual(PendingActionCheckpoint.blocking_exit, delivery.checkpoints.items[1]);
    try std.testing.expectEqual(@as(usize, 0), rt.pendingActionCount());
}

test "runtime: blocking enter drains mixed signal and finalizer actions once each" {
    const Delivery = struct {
        actions: std.ArrayListUnmanaged(PendingAction) = .{},
        checkpoints: std.ArrayListUnmanaged(PendingActionCheckpoint) = .{},

        fn deliver(ctx: ?*anyopaque, checkpoint: PendingActionCheckpoint, action: PendingAction) !void {
            const self: *@This() = @ptrCast(@alignCast(ctx.?));
            try self.actions.append(std.testing.allocator, action);
            try self.checkpoints.append(std.testing.allocator, checkpoint);
        }
    };

    var delivery = Delivery{};
    defer delivery.actions.deinit(std.testing.allocator);
    defer delivery.checkpoints.deinit(std.testing.allocator);

    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    try rt.registerSignalHandler(2, try rt.allocTuple(0));
    try rt.recordSignal(2);

    const finalizer_target = try rt.allocTuple(0);
    const finalizer_callback = try rt.allocTuple(0);
    _ = try rt.registerFinalizer(finalizer_target, finalizer_callback, .first);
    rt.collectMajor();

    rt.pending_action_delivery = .{
        .ctx = &delivery,
        .deliver_fn = Delivery.deliver,
    };
    try std.testing.expectEqual(@as(usize, 2), rt.pendingActionCount());
    try rt.enterBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .blocked), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try std.testing.expectEqual(@as(usize, 2), delivery.actions.items.len);
    try std.testing.expectEqual(@as(usize, 2), delivery.checkpoints.items.len);
    try std.testing.expectEqual(PendingActionCheckpoint.blocking_enter, delivery.checkpoints.items[0]);
    try std.testing.expectEqual(PendingActionCheckpoint.blocking_enter, delivery.checkpoints.items[1]);
    try std.testing.expect(delivery.actions.items[0] == .signal);
    try std.testing.expect(delivery.actions.items[1] == .finalizer);
    try std.testing.expectEqual(finalizer_callback, delivery.actions.items[1].finalizer.callback);
    try std.testing.expectEqual(@as(usize, 0), rt.pendingActionCount());

    try rt.exitBlockingSection();
    try std.testing.expectEqual(@as(usize, 2), delivery.actions.items.len);
}

test "runtime: failed mixed blocking delivery keeps signal and finalizer pending" {
    const Delivery = struct {
        fail_once: bool = true,
        actions: std.ArrayListUnmanaged(PendingAction) = .{},
        checkpoints: std.ArrayListUnmanaged(PendingActionCheckpoint) = .{},

        fn deliver(ctx: ?*anyopaque, checkpoint: PendingActionCheckpoint, action: PendingAction) !void {
            const self: *@This() = @ptrCast(@alignCast(ctx.?));
            if (self.fail_once) {
                self.fail_once = false;
                return error.DeliveryFailed;
            }
            try self.actions.append(std.testing.allocator, action);
            try self.checkpoints.append(std.testing.allocator, checkpoint);
        }
    };

    var delivery = Delivery{};
    defer delivery.actions.deinit(std.testing.allocator);
    defer delivery.checkpoints.deinit(std.testing.allocator);

    var rt = Runtime.init(std.testing.allocator);
    defer rt.deinit();

    try rt.registerSignalHandler(2, try rt.allocTuple(0));
    try rt.recordSignal(2);

    const finalizer_target = try rt.allocTuple(0);
    const finalizer_callback = try rt.allocTuple(0);
    _ = try rt.registerFinalizer(finalizer_target, finalizer_callback, .first);
    rt.collectMajor();

    rt.pending_action_delivery = .{
        .ctx = &delivery,
        .deliver_fn = Delivery.deliver,
    };
    try std.testing.expectEqual(@as(usize, 2), rt.pendingActionCount());
    try std.testing.expectError(error.DeliveryFailed, rt.enterBlockingSection());
    try std.testing.expectEqual(@as(?DomainStatus, .attached), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try std.testing.expectEqual((@as(u64, 1) << 2), rt.pendingSignalBits());
    try std.testing.expect(rt.liveness.peekReadyFinalizer() != null);
    try std.testing.expectEqual(@as(usize, 2), rt.pendingActionCount());

    try rt.enterBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .blocked), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try std.testing.expectEqual(@as(usize, 2), delivery.actions.items.len);
    try std.testing.expectEqual(PendingActionCheckpoint.blocking_enter, delivery.checkpoints.items[0]);
    try std.testing.expectEqual(PendingActionCheckpoint.blocking_enter, delivery.checkpoints.items[1]);
    try std.testing.expect(delivery.actions.items[0] == .signal);
    try std.testing.expect(delivery.actions.items[1] == .finalizer);
    try std.testing.expectEqual(@as(usize, 0), rt.pendingActionCount());
}

test "runtime: failed configured pending-action delivery does not clear work" {
    const Delivery = struct {
        fail_once: bool = true,
        actions: std.ArrayListUnmanaged(PendingAction) = .{},
        checkpoints: std.ArrayListUnmanaged(PendingActionCheckpoint) = .{},

        fn deliver(ctx: ?*anyopaque, checkpoint: PendingActionCheckpoint, action: PendingAction) !void {
            const self: *@This() = @ptrCast(@alignCast(ctx.?));
            if (self.fail_once) {
                self.fail_once = false;
                return error.DeliveryFailed;
            }
            try self.actions.append(std.testing.allocator, action);
            try self.checkpoints.append(std.testing.allocator, checkpoint);
        }
    };

    var delivery = Delivery{};
    defer delivery.actions.deinit(std.testing.allocator);
    defer delivery.checkpoints.deinit(std.testing.allocator);

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .pendingActionDelivery = .{
            .ctx = &delivery,
            .deliver_fn = Delivery.deliver,
        },
    });
    defer rt.deinit();

    try rt.registerSignalHandler(2, try rt.allocTuple(0));
    try rt.recordSignal(2);
    try std.testing.expectEqual(@as(usize, 1), rt.pendingActionCount());

    try std.testing.expectError(error.DeliveryFailed, rt.enterBlockingSection());
    try std.testing.expectEqual(@as(usize, 1), rt.pendingActionCount());
    try std.testing.expectEqual(@as(?DomainStatus, .attached), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try std.testing.expectEqual((@as(u64, 1) << 2), rt.pendingSignalBits());

    try rt.enterBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .blocked), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try std.testing.expectEqual(@as(usize, 0), rt.pendingActionCount());
    try std.testing.expectEqual(@as(usize, 1), delivery.actions.items.len);
    try std.testing.expectEqual(PendingActionCheckpoint.blocking_enter, delivery.checkpoints.items[0]);
}

test "runtime: stop-the-world pause drains configured pending actions" {
    const Delivery = struct {
        actions: std.ArrayListUnmanaged(PendingAction) = .{},
        checkpoints: std.ArrayListUnmanaged(PendingActionCheckpoint) = .{},

        fn deliver(ctx: ?*anyopaque, checkpoint: PendingActionCheckpoint, action: PendingAction) !void {
            const self: *@This() = @ptrCast(@alignCast(ctx.?));
            try self.actions.append(std.testing.allocator, action);
            try self.checkpoints.append(std.testing.allocator, checkpoint);
        }
    };

    var delivery = Delivery{};
    defer delivery.actions.deinit(std.testing.allocator);
    defer delivery.checkpoints.deinit(std.testing.allocator);

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .pendingActionDelivery = .{
            .ctx = &delivery,
            .deliver_fn = Delivery.deliver,
        },
    });
    defer rt.deinit();

    try rt.registerSignalHandler(2, try rt.allocTuple(0));
    try rt.recordSignal(2);

    _ = try rt.requestStopTheWorld();
    defer rt.resumeTheWorld();

    try std.testing.expectEqual(@as(usize, 1), delivery.actions.items.len);
    try std.testing.expectEqual(PendingActionCheckpoint.stw_pause, delivery.checkpoints.items[0]);
    try std.testing.expectEqual(@as(usize, 0), rt.pendingActionCount());
}

test "runtime: pending signal and finalizer delivery runs inside callback boundaries" {
    var trace = TraceRecorder.init(std.testing.allocator, .{
        .capture_events = true,
    });
    defer trace.deinit();

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .eventSink = trace.sink(),
    });
    defer rt.deinit();

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = 55,
        .handle_effect = Value.fromInt(1),
    });

    const child = try rt.spawnFiberInDomain(main, rt.currentDomain());
    try rt.activateFiberInDomain(rt.currentDomain(), child);
    try rt.registerSignalHandler(2, try rt.allocTuple(0));
    try rt.recordSignal(2);

    const finalizer_target = try rt.allocTuple(0);
    const finalizer_callback = try rt.allocTuple(0);
    _ = try rt.registerFinalizer(finalizer_target, finalizer_callback, .first);
    rt.collectMajor();

    const Delivery = struct {
        rt: *Runtime,
        actions: std.ArrayListUnmanaged(PendingAction) = .{},

        fn visit(self: *@This(), action: PendingAction) !void {
            try self.actions.append(std.testing.allocator, action);
            try std.testing.expectError(error.UnhandledEffect, self.rt.performEffectAt(8, 55, Value.fromInt(1), &.{}));
        }
    };

    var delivery = Delivery{ .rt = &rt };
    defer delivery.actions.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), try rt.deliverPendingActions(&delivery, Delivery.visit));
    try std.testing.expectEqual(@as(usize, 2), delivery.actions.items.len);
    try std.testing.expect(delivery.actions.items[0] == .signal);
    try std.testing.expect(delivery.actions.items[1] == .finalizer);
    try std.testing.expectEqual(finalizer_callback, delivery.actions.items[1].finalizer.callback);
    try std.testing.expectEqual(finalizer_target, delivery.actions.items[1].finalizer.argument.?);

    rt.collectMajor();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());

    var callback_entries: usize = 0;
    for (trace.traceEntries()) |entry| {
        if (entry.event == .control) {
            if (entry.event.control.action == .callback_enter or entry.event.control.action == .callback_exit) {
                callback_entries += 1;
            }
        }
    }
    try std.testing.expect(callback_entries >= 4);
}

test "runtime: memprof samples allocation backtraces and lifecycle transitions" {
    var trace = TraceRecorder.init(std.testing.allocator, .{
        .capture_events = true,
    });
    defer trace.deinit();

    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .eventSink = trace.sink(),
        .gcStrategy = .generational,
        .memprof = .{
            .enabled = true,
            .sample_interval_units = 1,
            .capture_backtraces = true,
        },
    });
    defer rt.deinit();

    const main = rt.currentFiber();
    try rt.pushFiberFrame(main, 777);
    defer _ = rt.popFiberFrame(main) catch unreachable;

    const tuple = try rt.allocTuple(1);
    var frame = rt.beginRootFrame();
    _ = try frame.bind(tuple);

    const explained = try rt.explainValue(tuple, null);
    try std.testing.expect(explained.memprof_sample != null);
    try std.testing.expectEqual(@as(u32, 777), explained.memprof_sample.?.backtrace_sites[0]);
    try std.testing.expectEqual(@as(usize, 8), explained.memprof_sample.?.payload_bytes);
    try std.testing.expectEqual(@as(usize, 8), explained.memprof_sample.?.storage_bytes);
    try std.testing.expectEqual(@as(usize, 1), explained.memprof_sample.?.scan_words);

    rt.collectMinor();
    try std.testing.expectEqual(Space.major, rt.objectSpace(tuple).?);
    try std.testing.expectEqual(@as(usize, 1), trace.snapshot().memprof_promotions);

    frame.end();
    rt.collectMajor();
    try std.testing.expect(rt.objectFromDebug(tuple) == null);

    const counters = trace.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_samples);
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_promotions);
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_reclaims);
}

test "runtime: debug object layout sizes" {
    if (false) {
        std.debug.print("value-size={d} object-size={d}\n", .{ @sizeOf(Value), @sizeOf(Object) });
    }
}
