const std = @import("std");
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
const remembered_set_mod = @import("remembered_set.zig");
const root_provider_mod = @import("root_provider.zig");
const root_registry = @import("root_registry.zig");
const runtime_services_mod = @import("runtime_services.zig");
const stw_coordinator_mod = @import("stw_coordinator.zig");

pub const Value = value.Value;
pub const Tag = value.Tag;
pub const HeapRef = value.HeapRef;
pub const Event = event_sink_mod.Event;
pub const EventCounters = event_sink_mod.Counters;
pub const EventRecorder = event_sink_mod.Recorder;
pub const EventSink = event_sink_mod.EventSink;
pub const GcSnapshotEvent = event_sink_mod.GcSnapshotEvent;
pub const ObjectExplain = struct {
    handle: HeapRef,
    kind: ObjectKind,
    space: heap_store.Space,
    size: usize,
    explicit_roots: usize,
    control_roots: usize,
    service_roots: usize,
    liveness_roots: usize,
    remembered_edges: usize,
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
pub const DomainStatus = domain_registry_mod.DomainStatus;
pub const FiberScheduler = fiber_scheduler_mod.FiberScheduler;
pub const HeapStore = heap_store.HeapStore;
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
pub const WeakRefHandle = liveness_mod.WeakRefHandle;
pub const EphemeronHandle = liveness_mod.EphemeronHandle;
pub const FinalizerHandle = liveness_mod.FinalizerHandle;
pub const FinalizerMode = liveness_mod.FinalizerMode;
pub const ReadyFinalizer = liveness_mod.ReadyFinalizer;
pub const RememberedSet = remembered_set_mod.RememberedSet;
pub const RootProvider = root_provider_mod.RootProvider;
pub const RootVisitor = root_provider_mod.RootVisitor;
pub const RootRegistry = root_registry.RootRegistry;
pub const RootHandle = root_registry.RootHandle;
pub const RuntimeServices = runtime_services_mod.RuntimeServices;
pub const StopTheWorldCoordinator = stw_coordinator_mod.StopTheWorldCoordinator;
pub const Error = language_mod.Error;

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
        nurseryObjectWords: usize = 32,
        nurseryLiveWords: usize = 1024,
        nurseryLiveObjects: usize = 256,
        memprof: MemprofConfig = .{},
        stackLimits: StackLimits = .{},
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
        nursery_words: usize,
        major_objects: usize,
        major_words: usize,
    };

    pub const PendingSignal = struct {
        signo: u8,
        handler: Value,
    };

    pub const PendingAction = union(enum) {
        signal: PendingSignal,
        finalizer: ReadyFinalizer,
    };

    allocator: std.mem.Allocator,
    event_sink: EventSink,
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

    pub fn init(allocator: std.mem.Allocator) Runtime {
        return initWithConfig(allocator, .{});
    }

    pub fn initWithConfig(allocator: std.mem.Allocator, config: Config) Runtime {
        var runtime = Runtime{
            .allocator = allocator,
            .event_sink = config.eventSink,
            .domains = DomainRegistry.init(allocator),
            .control_kernel = undefined,
            .fiber_scheduler = undefined,
            .stw = StopTheWorldCoordinator.init(allocator, config.eventSink),
            .heap_store = HeapStore.init(allocator),
            .remembered_set = RememberedSet.init(allocator),
            .root_registry = RootRegistry.init(allocator, config.eventSink),
            .services = RuntimeServices.init(allocator),
            .liveness = ManagedLiveness.init(allocator),
            .memprof = MemprofState.init(allocator, config.eventSink, config.memprof),
            .debug_root_checks = config.debugRootChecks,
            .debug_checks = config.debugChecks,
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
        if (config.fixedArena) |buffer| {
            runtime.fixed_arena = std.heap.FixedBufferAllocator.init(buffer);
            runtime.fixed_arena_buffer = buffer;
        }
        runtime.gc_strategy = config.gcStrategy;
        runtime.heap_store.configureNursery(.{
            .enabled = config.gcStrategy == .generational,
            .max_object_words = config.nurseryObjectWords,
            .max_live_words = config.nurseryLiveWords,
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
            .nursery_words = self.heap_store.spaceStats(.nursery).words,
            .major_objects = self.heap_store.spaceStats(.major).objects,
            .major_words = self.heap_store.spaceStats(.major).words,
        };
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

    pub fn stwCoordinator(self: *Runtime) *StopTheWorldCoordinator {
        return &self.stw;
    }

    pub fn currentDomain(self: *Runtime) DomainHandle {
        return self.control_kernel.currentDomain();
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

    pub fn createDomain(self: *Runtime) !DomainHandle {
        const domain = try self.domains.createDomain();
        try self.fiber_scheduler.registerDomain(domain);
        return domain;
    }

    pub fn attachDomain(self: *Runtime, handle: DomainHandle) !void {
        try self.domains.attach(handle);
    }

    pub fn detachDomain(self: *Runtime, handle: DomainHandle) !void {
        try self.domains.detach(handle);
    }

    pub fn createFiberInDomain(self: *Runtime, parent: ?FiberHandle, domain: DomainHandle) !FiberHandle {
        const state = self.domains.domain(domain) orelse return error.InvalidDomain;
        if (state.status == .detached) return error.DomainDetached;
        const fiber = try self.control_kernel.createFiberInDomain(parent, domain);
        try self.fiber_scheduler.enqueue(domain, fiber);
        return fiber;
    }

    pub fn spawnFiberInDomain(self: *Runtime, parent: ?FiberHandle, domain: DomainHandle) !FiberHandle {
        return self.createFiberInDomain(parent, domain);
    }

    pub fn activateFiberInDomain(self: *Runtime, domain: DomainHandle, fiber: FiberHandle) !void {
        try self.fiber_scheduler.activate(domain, fiber);
        try self.control_kernel.activateFiber(fiber);
    }

    pub fn scheduleNextFiber(self: *Runtime, domain: DomainHandle) !?FiberHandle {
        const next = try self.fiber_scheduler.switchToNext(domain) orelse return null;
        try self.control_kernel.activateFiber(next);
        return next;
    }

    pub fn yieldCurrentFiber(self: *Runtime) !?FiberHandle {
        const next = try self.fiber_scheduler.yieldCurrent(self.currentDomain()) orelse return null;
        try self.control_kernel.activateFiber(next);
        return next;
    }

    pub fn parkCurrentFiber(self: *Runtime) !?FiberHandle {
        const next = try self.fiber_scheduler.parkCurrent(self.currentDomain()) orelse return null;
        try self.control_kernel.activateFiber(next);
        return next;
    }

    pub fn unparkFiber(self: *Runtime, domain: DomainHandle, fiber: FiberHandle) !void {
        try self.fiber_scheduler.unpark(domain, fiber);
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
            _ = try self.fiber_scheduler.suspendCurrent(current_domain);
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
            _ = try self.fiber_scheduler.suspendCurrent(current_domain);
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
            _ = try self.fiber_scheduler.discardSuspended(continuation.domain, continuation.fiber);
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
            _ = self.fiber_scheduler.discardSuspended(fiber_domain, fiber) catch return false;
            self.control_kernel.discardFiber(fiber) catch return false;
        }
        return true;
    }

    pub fn requestStopTheWorld(self: *Runtime) !usize {
        const generation = try self.stw.request(self.currentDomain());
        const Pause = struct {
            stw: *StopTheWorldCoordinator,

            fn visit(ctx: *@This(), domain: DomainHandle) void {
                ctx.stw.markPaused(domain) catch unreachable;
            }
        };
        var pause_ctx = Pause{ .stw = &self.stw };
        self.domains.visitAttached(&pause_ctx, Pause.visit);
        return generation;
    }

    pub fn enterSafepoint(self: *Runtime, domain: DomainHandle) !void {
        try self.stw.markPaused(domain);
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
        self.prepareAllocation(.tuple, len);
        var surface = self.language();
        const allocated = try surface.allocTuple(len);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    /// Allocate a tuple and initialize all fields from `fields`.
    pub fn tuple(self: *Runtime, fields: []const Value) !Value {
        self.prepareAllocation(.tuple, fields.len);
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
        self.prepareAllocation(.string, bytes.len);
        var surface = self.language();
        const allocated = try surface.allocString(bytes);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocStringWithFill(self: *Runtime, len: usize, fill: u8) !Value {
        self.prepareAllocation(.string, len);
        var surface = self.language();
        const allocated = try surface.allocStringWithFill(len, fill);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocStringWithInit(self: *Runtime, len: usize, initial_bytes: []const u8) !Value {
        self.prepareAllocation(.string, len);
        var surface = self.language();
        const allocated = try surface.allocStringWithInit(len, initial_bytes);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocBytes(self: *Runtime, bytes: []const u8) !Value {
        self.prepareAllocation(.string, bytes.len);
        var surface = self.language();
        const allocated = try surface.allocBytes(bytes);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocBytesWithFill(self: *Runtime, len: usize, fill: u8) !Value {
        self.prepareAllocation(.string, len);
        var surface = self.language();
        const allocated = try surface.allocBytesWithFill(len, fill);
        self.trackAllocationSample(allocated);
        return allocated;
    }

    pub fn allocBytesWithInit(self: *Runtime, len: usize, initial_bytes: []const u8) !Value {
        self.prepareAllocation(.string, len);
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

    pub fn registerRoot(self: *Runtime, slot: *const Value) !void {
        try self.root_registry.register(slot);
    }

    pub fn scopedRoot(self: *Runtime, slot: *const Value) !RootHandle {
        return self.root_registry.scoped(slot);
    }

    pub fn unregisterRoot(self: *Runtime, slot: *const Value) void {
        self.root_registry.unregister(slot);
    }

    pub fn registerNamedValue(self: *Runtime, name: []const u8, rooted: Value) !void {
        try self.services.registerNamedValue(name, rooted);
    }

    pub fn lookupNamedValue(self: *Runtime, name: []const u8) ?Value {
        return self.services.lookupNamedValue(name);
    }

    pub fn enterBlockingSection(self: *Runtime) void {
        self.services.enterBlockingSection();
        self.domains.enterBlocking(self.currentDomain()) catch unreachable;
    }

    pub fn exitBlockingSection(self: *Runtime) !void {
        try self.services.exitBlockingSection();
        try self.domains.exitBlocking(self.currentDomain());
    }

    pub fn recordSignal(self: *Runtime, signo: u8) !void {
        try self.services.recordSignal(signo);
    }

    pub fn takePendingSignals(self: *Runtime) u64 {
        return self.services.takePendingSignals();
    }

    pub fn registerSignalHandler(self: *Runtime, signo: u8, handler: Value) !void {
        try self.services.registerSignalHandler(signo, handler);
    }

    pub fn lookupSignalHandler(self: *Runtime, signo: u8) ?Value {
        return self.services.lookupSignalHandler(signo);
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
        return .{
            .handle = handle,
            .kind = obj.kind().?,
            .space = self.heap_store.spaceOf(handle).?,
            .size = obj.wosize(),
            .explicit_roots = self.root_registry.ownerCount(block_value),
            .control_roots = self.control_kernel.ownedRootCount(block_value),
            .service_roots = self.services.ownerCount(block_value),
            .liveness_roots = self.liveness.ownerCount(block_value),
            .remembered_edges = self.remembered_set.ownerCount(handle),
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
        const current = self.control_kernel.currentFiber();
        var delivered: usize = 0;

        var pending_signals = self.services.takePendingSignals();
        while (pending_signals != 0) {
            const bit_index = @ctz(pending_signals);
            pending_signals &= ~(@as(u64, 1) << @intCast(bit_index));
            const signo: u8 = @intCast(bit_index);
            const handler = self.services.lookupSignalHandler(signo) orelse continue;
            {
                try self.control_kernel.enterCallbackBoundary(current);
                defer self.control_kernel.exitCallbackBoundary(current) catch unreachable;
                try deliver(context, .{ .signal = .{
                    .signo = signo,
                    .handler = handler,
                } });
            }
            delivered += 1;
        }

        const ready_finalizers = try self.liveness.drainReadyFinalizers(self.allocator);
        defer self.allocator.free(ready_finalizers);
        for (ready_finalizers) |ready| {
            {
                try self.control_kernel.enterCallbackBoundary(current);
                defer self.control_kernel.exitCallbackBoundary(current) catch unreachable;
                try deliver(context, .{ .finalizer = ready });
            }
            delivered += 1;
        }

        return delivered;
    }

    fn trackAllocationSample(self: *Runtime, block_value: Value) void {
        if (!self.memprof.enabled()) return;
        const handle = block_value.asHeapRef() orelse return;
        const obj = self.objectFrom(block_value) orelse return;
        const sample_ordinal = self.memprof.beginAllocation(obj.wosize()) orelse return;
        const kind = obj.kind() orelse return;
        const space = self.heap_store.spaceOf(handle) orelse return;

        if (!self.memprof.capturesBacktraces()) {
            self.memprof.recordAllocation(sample_ordinal, handle, kind, obj.wosize(), space, &.{});
            return;
        }

        const frames = self.control_kernel.captureBacktrace(self.allocator, null) catch {
            self.memprof.recordAllocation(sample_ordinal, handle, kind, obj.wosize(), space, &.{});
            return;
        };
        defer self.allocator.free(frames);

        const sites = self.allocator.alloc(u32, frames.len) catch {
            self.memprof.recordAllocation(sample_ordinal, handle, kind, obj.wosize(), space, &.{});
            return;
        };
        defer self.allocator.free(sites);

        for (frames, 0..) |frame, index| {
            sites[index] = frame.site_id;
        }
        self.memprof.recordAllocation(sample_ordinal, handle, kind, obj.wosize(), space, sites);
    }

    fn prepareCompatAllocation(self: *Runtime, arity: usize, tag: Tag) void {
        const kind, const words = switch (tag) {
            .tuple => .{ .tuple, arity },
            .string => .{ .string, arity },
            .int64 => .{ .boxed_i64, 1 },
            .double => .{ .boxed_f64, 1 },
            .custom => .{ .custom, arity },
        };
        self.prepareAllocation(kind, words);
    }

    fn prepareAllocation(self: *Runtime, kind: ObjectKind, words: usize) void {
        if (self.gc_strategy != .generational) return;
        if (kind == .custom) return;
        if (words > self.heap_store.nursery_config.max_object_words) return;
        if (!self.heap_store.shouldCollectBeforeNurseryAlloc(words)) return;
        self.collectMinor();
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

    const depth = 1_024;
    var head = try rt.allocTuple(1);
    var current = head;
    var i: usize = 1;

    while (i < depth) : (i += 1) {
        const next = try rt.allocTuple(1);
        try rt.setField(current, 0, next);
        current = next;
    }

    try rt.setField(current, 0, Value.fromInt(1234));

    try rt.registerRoot(&head);
    rt.collect();
    try std.testing.expectEqual(@as(usize, depth), rt.objectCount());

    rt.unregisterRoot(&head);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: shared object graph keeps object alive across multiple parents" {
    var rt = Runtime.init(std.testing.allocator);

    const shared = try rt.allocTuple(1);
    try rt.setField(shared, 0, Value.fromInt(0));

    const left = try rt.allocTuple(2);
    try rt.setField(left, 0, shared);
    try rt.setField(left, 1, Value.fromInt(1));

    const right = try rt.allocTuple(2);
    try rt.setField(right, 0, shared);
    try rt.setField(right, 1, Value.fromInt(2));

    try rt.registerRoot(&left);
    try rt.registerRoot(&right);

    rt.collect();
    try std.testing.expectEqual(@as(usize, 3), rt.objectCount());

    try rt.setField(shared, 0, Value.fromInt(77));

    const shared_from_left = try rt.field(left, 0);
    const shared_from_right = try rt.field(right, 0);
    try std.testing.expectEqual(shared, shared_from_left);
    try std.testing.expectEqual(shared, shared_from_right);
    try std.testing.expectEqual(Value.fromInt(77), try rt.field(shared_from_left, 0));
    try std.testing.expectEqual(Value.fromInt(77), try rt.field(shared_from_right, 0));

    rt.unregisterRoot(&left);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    rt.unregisterRoot(&right);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: cyclic graph is marked without recursion blowup" {
    var rt = Runtime.init(std.testing.allocator);

    const first = try rt.allocTuple(1);
    const second = try rt.allocTuple(1);

    try rt.setField(first, 0, second);
    try rt.setField(second, 0, first);

    try rt.registerRoot(&first);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    rt.unregisterRoot(&first);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
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
    const root = try rt.allocTuple(1);
    const child = try rt.allocTuple(1);
    _ = try rt.allocTuple(1);
    try rt.setField(root, 0, child);

    try rt.registerRoot(&root);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
    rt.unregisterRoot(&root);
    rt.deinit();
}

test "runtime: bump GC strategy discards rooted objects" {
    var arena = [_]u8{0} ** 256;
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .fixedArena = arena[0..],
        .gcStrategy = .bump,
    });

    const root = try rt.allocTuple(1);
    const child = try rt.allocTuple(1);
    _ = try rt.allocTuple(1);
    try rt.setField(root, 0, child);

    try rt.registerRoot(&root);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());

    rt.unregisterRoot(&root);
    rt.deinit();
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

test "runtime: scoped root handle keeps and releases liveness" {
    var rt = Runtime.init(std.testing.allocator);
    var rooted = try rt.allocTuple(1);
    var handle = try rt.scopedRoot(&rooted);

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

    const left = try rt.allocTuple(2);
    const right = try rt.allocTuple(2);
    try rt.setField(left, 0, right);
    try rt.setField(right, 0, left);
    try rt.setField(left, 1, Value.fromInt(1));
    try rt.setField(right, 1, Value.fromInt(2));

    var root = left;
    try rt.registerRoot(&root);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());

    rt.unregisterRoot(&root);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.deinit();
}

test "runtime: immediate roots are ignored by GC and do not retain blocks" {
    var rt = Runtime.init(std.testing.allocator);
    _ = try rt.allocTuple(1);
    var root = Value.fromInt(1234);
    try rt.registerRoot(&root);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 0), rt.objectCount());
    rt.unregisterRoot(&root);
    rt.deinit();
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
    var root = try rt.allocTuple(2);
    const child = try rt.allocTuple(1);
    try rt.setField(root, 0, child);
    try rt.setField(root, 1, Value.fromInt(7));
    try rt.setField(child, 0, Value.fromInt(9));

    try rt.registerRoot(&root);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
    rt.unregisterRoot(&root);
    rt.deinit();
}

test "runtime: self-referential tuple survives gc" {
    var rt = Runtime.init(std.testing.allocator);
    var cyclic = try rt.allocTuple(1);
    try rt.setField(cyclic, 0, cyclic);
    try rt.registerRoot(&cyclic);
    rt.collect();
    try std.testing.expectEqual(@as(usize, 1), rt.objectCount());
    rt.unregisterRoot(&cyclic);
    rt.deinit();
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
    var kept: Value = Value.fromInt(0);
    const keep_me = try runtime.allocTuple(1);
    try runtime.setField(keep_me, 0, Value.fromInt(42));
    kept = keep_me;

    _ = try runtime.allocTuple(1);
    try runtime.registerRoot(&kept);
    runtime.collect();
    try std.testing.expectEqual(@as(usize, 1), runtime.objectCount());
    runtime.unregisterRoot(&kept);

    runtime.deinit();
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
    try std.testing.expectEqual(rt.domainRegistry().attachedCount(), rt.stwCoordinator().pausedCount());
    try std.testing.expect(rt.stwCoordinator().isPaused(rt.currentDomain()));
    try std.testing.expect(rt.stwCoordinator().isPaused(other));

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

    var rooted = try rt.allocTuple(1);
    try rt.registerRoot(&rooted);
    const child = try rt.allocTuple(0);
    try rt.setField(rooted, 0, child);
    try rt.registerNamedValue("child", child);

    const main = rt.currentFiber();
    try rt.pushEffectHandler(main, .{
        .effect = 1,
        .handle_effect = child,
    });

    const explained_root = try rt.explainValue(rooted, &trace);
    try std.testing.expectEqual(@as(usize, 1), explained_root.explicit_roots);
    try std.testing.expectEqual(@as(usize, 0), explained_root.remembered_edges);
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

    var rooted = try rt.allocTuple(0);
    try rt.registerRoot(&rooted);
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

    rt.enterBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .blocked), rt.domainRegistry().domain(rt.currentDomain()).?.status);
    try rt.recordSignal(2);
    try rt.recordSignal(7);
    try std.testing.expectEqual((@as(u64, 1) << 2) | (@as(u64, 1) << 7), rt.takePendingSignals());
    try std.testing.expectEqual(@as(u64, 0), rt.takePendingSignals());
    try rt.exitBlockingSection();
    try std.testing.expectEqual(@as(?DomainStatus, .attached), rt.domainRegistry().domain(rt.currentDomain()).?.status);
}

test "runtime: generational minor collection promotes live nursery objects" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
    });
    defer rt.deinit();

    var root = try rt.allocTuple(1);
    try rt.registerRoot(&root);
    const child = try rt.allocTuple(0);
    _ = try rt.allocTuple(0);
    try rt.setField(root, 0, child);

    try std.testing.expectEqual(@as(?Space, .nursery), rt.objectSpace(root));
    try std.testing.expectEqual(@as(?Space, .nursery), rt.objectSpace(child));
    rt.collectMinor();

    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(root));
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(child));
    try std.testing.expectEqual(@as(usize, 2), rt.objectCount());
}

test "runtime: nursery pressure triggers minor collection before allocation" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
        .nurseryLiveWords = 1,
        .nurseryLiveObjects = 1,
    });
    defer rt.deinit();

    var root = try rt.allocTuple(1);
    try rt.registerRoot(&root);
    try std.testing.expectEqual(@as(?Space, .nursery), rt.objectSpace(root));
    try std.testing.expectEqual(@as(u64, 0), rt.rootStats().minor_collect_generations);

    const next = try rt.allocTuple(1);

    try std.testing.expectEqual(@as(u64, 1), rt.rootStats().minor_collect_generations);
    try std.testing.expectEqual(@as(?Space, .major), rt.objectSpace(root));
    try std.testing.expectEqual(@as(?Space, .nursery), rt.objectSpace(next));

    const stats = rt.rootStats();
    try std.testing.expectEqual(@as(usize, 1), stats.nursery_objects);
    try std.testing.expectEqual(@as(usize, 1), stats.nursery_words);
    try std.testing.expectEqual(@as(usize, 1), stats.major_objects);
    try std.testing.expectEqual(@as(usize, 1), stats.major_words);
}

test "runtime: remembered edges keep nursery children alive from major parents" {
    var rt = Runtime.initWithConfig(std.testing.allocator, .{
        .gcStrategy = .generational,
    });
    defer rt.deinit();

    var parent = try rt.allocTuple(1);
    try rt.registerRoot(&parent);
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

    var key = try rt.allocTuple(0);
    try rt.registerRoot(&key);
    const data = try rt.allocTuple(0);
    const weak = try rt.createWeakRef(data);
    const ephemeron = try rt.createEphemeron(&.{key}, data);

    rt.collectMajor();
    try std.testing.expectEqual(data, (try rt.weakGet(weak)).?);
    try std.testing.expectEqual(data, (try rt.ephemeronData(ephemeron)).?);

    rt.unregisterRoot(&key);
    rt.collectMajor();
    try std.testing.expectEqual(@as(?Value, null), try rt.weakGet(weak));
    try std.testing.expectEqual(@as(?Value, null), try rt.ephemeronData(ephemeron));
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
        actions: std.ArrayListUnmanaged(Runtime.PendingAction) = .{},

        fn visit(self: *@This(), action: Runtime.PendingAction) !void {
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
            .sample_interval_words = 1,
            .capture_backtraces = true,
        },
    });
    defer rt.deinit();

    const main = rt.currentFiber();
    try rt.pushFiberFrame(main, 777);
    defer _ = rt.popFiberFrame(main) catch unreachable;

    const tuple = try rt.allocTuple(1);
    var rooted = tuple;
    var root = try rt.scopedRoot(&rooted);

    const explained = try rt.explainValue(tuple, null);
    try std.testing.expect(explained.memprof_sample != null);
    try std.testing.expectEqual(@as(u32, 777), explained.memprof_sample.?.backtrace_sites[0]);

    rt.collectMinor();
    try std.testing.expectEqual(Space.major, rt.objectSpace(tuple).?);
    try std.testing.expectEqual(@as(usize, 1), trace.snapshot().memprof_promotions);

    rooted = Value.fromInt(0);
    root.deinit();
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
