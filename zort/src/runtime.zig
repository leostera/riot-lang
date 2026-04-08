const std = @import("std");
const value = @import("value.zig");
const heap_store = @import("heap_store.zig");
const collector_mod = @import("collector.zig");
const mutator = @import("mutator.zig");
const root_registry = @import("root_registry.zig");

pub const Value = value.Value;
pub const Tag = value.Tag;
pub const HeapRef = value.HeapRef;
pub const HeapStore = heap_store.HeapStore;
pub const Object = heap_store.Object;
pub const ObjectKind = heap_store.ObjectKind;
pub const Collector = collector_mod.Collector;
pub const Mutator = mutator.Mutator;
pub const RootRegistry = root_registry.RootRegistry;
pub const RootHandle = root_registry.RootHandle;
pub const Error = mutator.Error;

pub const Runtime = struct {
    pub const GcStrategy = collector_mod.GcStrategy;

    pub const Config = struct {
        debugRootChecks: bool = false,
        fixedArena: ?[]u8 = null,
        /// Strategy selection for collection:
        /// - .mark_sweep: root-based mark-and-sweep (default, baseline behavior)
        /// - .bump: experimental full reset path
        gcStrategy: GcStrategy = .mark_sweep,
    };

    pub const Stats = struct {
        root_generation: usize,
        root_registrations: usize,
        root_unregistrations: usize,
        collect_generations: usize,
    };

    allocator: std.mem.Allocator,
    heap_store: HeapStore,
    root_registry: RootRegistry,
    debug_root_checks: bool = false,
    fixed_arena: ?std.heap.FixedBufferAllocator = null,
    gc_strategy: GcStrategy = .mark_sweep,
    fixed_arena_buffer: ?[]u8 = null,
    collect_generations: usize = 0,

    pub fn init(allocator: std.mem.Allocator) Runtime {
        return initWithConfig(allocator, .{});
    }

    pub fn initWithConfig(allocator: std.mem.Allocator, config: Config) Runtime {
        var runtime = Runtime{
            .allocator = allocator,
            .heap_store = HeapStore.init(allocator),
            .root_registry = RootRegistry.init(allocator),
            .debug_root_checks = config.debugRootChecks,
        };
        if (config.fixedArena) |buffer| {
            runtime.fixed_arena = std.heap.FixedBufferAllocator.init(buffer);
            runtime.fixed_arena_buffer = buffer;
        }
        runtime.gc_strategy = config.gcStrategy;
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
        };
    }

    pub fn deinit(self: *Runtime) void {
        self.heap_store.deinit(self.fixed_arena_buffer != null);
        self.root_registry.deinit();
    }

    pub fn objectCount(self: *Runtime) usize {
        return self.heap_store.count();
    }

    pub fn collector(self: *Runtime) Collector {
        return Collector.init(
            &self.heap_store,
            self.root_registry.items(),
            &self.fixed_arena,
            self.fixed_arena_buffer,
            self.gc_strategy,
        );
    }

    pub fn mutator(self: *Runtime) Mutator {
        return Mutator.init(self.currentAllocator(), &self.heap_store);
    }

    pub fn alloc(self: *Runtime, arity: usize, tag: Tag) !Value {
        var writer = self.mutator();
        return writer.allocCompat(arity, tag);
    }

    pub fn allocTuple(self: *Runtime, len: usize) !Value {
        return self.alloc(len, .tuple);
    }

    /// Allocate a tuple and initialize all fields from `fields`.
    pub fn tuple(self: *Runtime, fields: []const Value) !Value {
        var writer = self.mutator();
        const result = try writer.allocTuple(fields.len);
        if (fields.len == 0) return result;
        try writer.initTupleFromSlice(result, fields);
        return result;
    }

    pub fn allocString(self: *Runtime, bytes: []const u8) !Value {
        return self.allocStringWithInit(bytes.len, bytes);
    }

    pub fn allocStringWithFill(self: *Runtime, len: usize, fill: u8) !Value {
        var writer = self.mutator();
        const string = try writer.allocStringLen(len);
        try writer.fillString(string, fill);
        return string;
    }

    pub fn allocStringWithInit(self: *Runtime, len: usize, initial_bytes: []const u8) !Value {
        if (initial_bytes.len > len) return Error.OutOfMemory;
        var writer = self.mutator();
        const string = try writer.allocStringLen(len);
        try writer.initStringBytes(string, initial_bytes);
        return string;
    }

    pub fn allocI64(self: *Runtime, n: i64) !Value {
        var writer = self.mutator();
        return writer.allocBoxedI64(n);
    }

    pub fn allocInt64(self: *Runtime, n: i64) !Value {
        return self.allocI64(n);
    }

    pub fn allocInt32(self: *Runtime, n: i32) !Value {
        return self.allocI32(n);
    }

    pub fn allocI32(self: *Runtime, n: i32) !Value {
        return self.allocI64(@as(i64, n));
    }

    pub fn allocF64(self: *Runtime, number: f64) !Value {
        var writer = self.mutator();
        return writer.allocBoxedF64(number);
    }

    pub fn allocDouble(self: *Runtime, number: f64) !Value {
        return self.allocF64(number);
    }

    pub fn field(self: *Runtime, block_value: Value, idx: usize) !Value {
        const obj = self.objectFrom(block_value) orelse return Error.InvalidValue;
        const fields = obj.tupleFields() orelse return Error.InvalidValue;
        if (idx >= fields.len) return Error.InvalidValue;
        return fields[idx];
    }

    pub fn setField(self: *Runtime, block_value: Value, idx: usize, next: Value) !void {
        var writer = self.mutator();
        try writer.writeField(block_value, idx, next);
    }

    pub fn setStringBytes(self: *Runtime, block_value: Value, bytes: []const u8) !void {
        var writer = self.mutator();
        try writer.writeStringBytes(block_value, bytes);
    }

    pub fn stringLength(self: *Runtime, block_value: Value) !usize {
        const obj = self.objectFrom(block_value) orelse return Error.InvalidValue;
        if (obj.kind() != .string) return Error.InvalidValue;
        return obj.wosize();
    }

    pub fn stringSlice(self: *Runtime, block_value: Value) ![]const u8 {
        const obj = self.objectFrom(block_value) orelse return Error.InvalidValue;
        return obj.stringSlice() orelse Error.InvalidValue;
    }

    pub fn isString(self: *Runtime, block_value: Value) bool {
        const obj = self.objectFrom(block_value) orelse return false;
        return obj.kind() == .string;
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

    pub fn collect(self: *Runtime) void {
        if (self.debug_root_checks) self.verifyRoots();
        self.collect_generations +%= 1;
        var gc = self.collector();
        gc.collect();
    }

    fn objectFrom(self: *Runtime, block_value: Value) ?*Object {
        const handle = block_value.asHeapRef() orelse return null;
        return self.heap_store.get(handle);
    }

    fn verifyRoots(self: *Runtime) void {
        self.root_registry.verify(self, isValidRootedValue);
    }

    fn currentAllocator(self: *Runtime) std.mem.Allocator {
        // Derive the allocator from the live fixed-arena state so the runtime
        // never holds an allocator interface tied to a pre-move stack address.
        if (self.fixed_arena) |*arena| return arena.allocator();
        return self.allocator;
    }

    fn isValidRootedValue(self: *Runtime, rooted: Value) bool {
        return self.objectFrom(rooted) != null;
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

    test "runtime: debug object layout sizes" {
    if (false) {
        std.debug.print("value-size={d} object-size={d}\n", .{ @sizeOf(Value), @sizeOf(Object) });
    }
}
