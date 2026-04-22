const std = @import("std");
const value = @import("value.zig");

pub const HeapRef = value.HeapRef;
pub const ObjectKind = enum {
    tuple,
    string,
    boxed_i64,
    boxed_f64,
    custom,
};

pub const Space = enum {
    nursery,
    major,
};

pub const BackendKind = enum {
    slot_registry,
};

pub const Config = struct {
    backend: BackendKind = .slot_registry,
    tuple_page_capacity_words: usize = 256,
};

pub const NurseryConfig = struct {
    enabled: bool = false,
    max_object_units: usize = 32,
    max_live_units: usize = 1024,
    max_live_objects: usize = 256,
};

pub const SpaceStats = struct {
    objects: usize = 0,
    allocation_units: usize = 0,
};

pub const ObjectSizeMetrics = struct {
    payload_bytes: usize = 0,
    storage_bytes: usize = 0,
    scan_words: usize = 0,
    allocation_cost_units: usize = 0,
};

pub const StorageOwner = enum {
    host_allocator,
    static,
    nursery_page,
    major_page,
};

pub const TupleStorage = struct {
    fields: []value.Value,
    owner: StorageOwner = .host_allocator,
};

pub const StringStorage = struct {
    len: usize,
    buffer: []u8,
    owner: StorageOwner = .host_allocator,
};

pub const CustomStorage = struct {
    bytes: []u8,
    owner: StorageOwner = .host_allocator,
};

const Payload = union(enum) {
    none,
    tuple: TupleStorage,
    string: StringStorage,
    boxed_i64: i64,
    boxed_f64: f64,
    custom: CustomStorage,
};

pub const Object = struct {
    marked: bool,
    payload: Payload,

    pub fn empty() Object {
        return .{
            .marked = false,
            .payload = .none,
        };
    }

    pub fn initTuple(fields: []value.Value) Object {
        return initTupleOwned(fields, .host_allocator);
    }

    pub fn initTupleOwned(fields: []value.Value, owner: StorageOwner) Object {
        return .{
            .marked = false,
            .payload = .{ .tuple = .{
                .fields = fields,
                .owner = owner,
            } },
        };
    }

    pub fn initString(len: usize, buffer: []u8) Object {
        return initStringOwned(len, buffer, .host_allocator);
    }

    pub fn initStringOwned(len: usize, buffer: []u8, owner: StorageOwner) Object {
        return .{
            .marked = false,
            .payload = .{ .string = .{
                .len = len,
                .buffer = buffer,
                .owner = owner,
            } },
        };
    }

    pub fn initBoxedI64(number: i64) Object {
        return .{
            .marked = false,
            .payload = .{ .boxed_i64 = number },
        };
    }

    pub fn initBoxedF64(number: f64) Object {
        return .{
            .marked = false,
            .payload = .{ .boxed_f64 = number },
        };
    }

    pub fn initCustom(bytes: []u8) Object {
        return initCustomOwned(bytes, .host_allocator);
    }

    pub fn initCustomOwned(bytes: []u8, owner: StorageOwner) Object {
        return .{
            .marked = false,
            .payload = .{ .custom = .{
                .bytes = bytes,
                .owner = owner,
            } },
        };
    }

    pub fn kind(self: *const Object) ?ObjectKind {
        return switch (self.payload) {
            .none => null,
            .tuple => .tuple,
            .string => .string,
            .boxed_i64 => .boxed_i64,
            .boxed_f64 => .boxed_f64,
            .custom => .custom,
        };
    }

    pub fn compatTag(self: *const Object) value.Tag {
        return switch (self.payload) {
            .none => unreachable,
            .tuple => .tuple,
            .string => .string,
            .boxed_i64 => .int64,
            .boxed_f64 => .double,
            .custom => .custom,
        };
    }

    pub fn sizeMetrics(self: *const Object) ObjectSizeMetrics {
        return switch (self.payload) {
            .none => .{},
            .tuple => |storage| .{
                .payload_bytes = storage.fields.len * @sizeOf(value.Value),
                .storage_bytes = storage.fields.len * @sizeOf(value.Value),
                .scan_words = storage.fields.len,
                .allocation_cost_units = @max(storage.fields.len, 1),
            },
            .string => |storage| .{
                .payload_bytes = storage.len,
                .storage_bytes = storage.buffer.len,
                .scan_words = 0,
                .allocation_cost_units = bytesToAllocationUnits(storage.buffer.len),
            },
            .boxed_i64 => .{
                .payload_bytes = @sizeOf(i64),
                .storage_bytes = 0,
                .scan_words = 0,
                .allocation_cost_units = 1,
            },
            .boxed_f64 => .{
                .payload_bytes = @sizeOf(f64),
                .storage_bytes = 0,
                .scan_words = 0,
                .allocation_cost_units = 1,
            },
            .custom => |storage| .{
                .payload_bytes = storage.bytes.len,
                .storage_bytes = storage.bytes.len,
                .scan_words = 0,
                .allocation_cost_units = bytesToAllocationUnits(storage.bytes.len),
            },
        };
    }

    pub fn payloadBytes(self: *const Object) usize {
        return self.sizeMetrics().payload_bytes;
    }

    pub fn storageBytes(self: *const Object) usize {
        return self.sizeMetrics().storage_bytes;
    }

    pub fn scanWords(self: *const Object) usize {
        return self.sizeMetrics().scan_words;
    }

    pub fn allocationCostUnits(self: *const Object) usize {
        return self.sizeMetrics().allocation_cost_units;
    }

    pub fn tupleFields(self: *Object) ?[]value.Value {
        return switch (self.payload) {
            .tuple => |storage| storage.fields,
            else => null,
        };
    }

    pub fn tupleFieldsConst(self: *const Object) ?[]const value.Value {
        return switch (self.payload) {
            .tuple => |storage| storage.fields,
            else => null,
        };
    }

    pub fn stringSlice(self: *const Object) ?[]const u8 {
        return switch (self.payload) {
            .string => |storage| storage.buffer[0..storage.len],
            else => null,
        };
    }

    pub fn stringBuffer(self: *const Object) ?[]const u8 {
        return switch (self.payload) {
            .string => |storage| storage.buffer,
            else => null,
        };
    }

    pub fn stringBufferMut(self: *Object) ?[]u8 {
        return switch (self.payload) {
            .string => |*storage| storage.buffer,
            else => null,
        };
    }

    pub fn boxedI64(self: *const Object) ?i64 {
        return switch (self.payload) {
            .boxed_i64 => |number| number,
            else => null,
        };
    }

    pub fn boxedF64(self: *const Object) ?f64 {
        return switch (self.payload) {
            .boxed_f64 => |number| number,
            else => null,
        };
    }

    pub fn customBytes(self: *const Object) ?[]const u8 {
        return switch (self.payload) {
            .custom => |storage| storage.bytes,
            else => null,
        };
    }

    pub fn storageOwner(self: *const Object) ?StorageOwner {
        return switch (self.payload) {
            .none, .boxed_i64, .boxed_f64 => null,
            .tuple => |storage| storage.owner,
            .string => |storage| storage.owner,
            .custom => |storage| storage.owner,
        };
    }

    pub fn promoteStorageOwner(self: *Object) void {
        switch (self.payload) {
            .tuple => |*storage| {
                if (storage.owner == .nursery_page) storage.owner = .major_page;
            },
            .string => |*storage| {
                if (storage.owner == .nursery_page) storage.owner = .major_page;
            },
            .custom => |*storage| {
                if (storage.owner == .nursery_page) storage.owner = .major_page;
            },
            .none, .boxed_i64, .boxed_f64 => {},
        }
    }

    pub fn deinit(self: *Object, allocator: std.mem.Allocator, fixed_arena: bool) void {
        if (!fixed_arena) {
            switch (self.payload) {
                .tuple => |storage| switch (storage.owner) {
                    .host_allocator => if (storage.fields.len > 0) allocator.free(storage.fields),
                    .static, .nursery_page, .major_page => {},
                },
                .string => |storage| switch (storage.owner) {
                    .host_allocator => allocator.free(storage.buffer),
                    .static, .nursery_page, .major_page => {},
                },
                .custom => |storage| switch (storage.owner) {
                    .host_allocator => if (storage.bytes.len > 0) allocator.free(storage.bytes),
                    .static, .nursery_page, .major_page => {},
                },
                .none, .boxed_i64, .boxed_f64 => {},
            }
        }
        self.* = Object.empty();
    }
};

const HeapSlot = struct {
    generation: u32,
    alive: bool,
    space: Space,
    object: Object,
};

const TuplePage = struct {
    allocator: std.mem.Allocator,
    storage: []value.Value,
    used_words: usize = 0,
    live_words: usize = 0,
    live_allocations: usize = 0,

    fn capacity(self: *const TuplePage) usize {
        return self.storage.len;
    }

    fn remaining(self: *const TuplePage) usize {
        return self.capacity() - self.used_words;
    }

    fn reset(self: *TuplePage) void {
        self.used_words = 0;
        self.live_words = 0;
        self.live_allocations = 0;
    }

    fn containsFields(self: *const TuplePage, fields: []const value.Value) bool {
        if (fields.len == 0) return false;
        const start = @intFromPtr(fields.ptr);
        const end = start + (fields.len * @sizeOf(value.Value));
        const page_start = @intFromPtr(self.storage.ptr);
        const page_end = page_start + (self.storage.len * @sizeOf(value.Value));
        return start >= page_start and end <= page_end;
    }
};

pub const HeapStore = struct {
    allocator: std.mem.Allocator,
    backend_kind: BackendKind = .slot_registry,
    tuple_page_capacity_words: usize = 256,
    tuple_pages: std.ArrayListUnmanaged(TuplePage) = .{},
    slots: std.ArrayListUnmanaged(HeapSlot) = .{},
    free_indices: std.ArrayListUnmanaged(u32) = .{},
    nursery_handles: std.ArrayListUnmanaged(value.HeapRef) = .{},
    object_count: usize = 0,
    nursery_config: NurseryConfig = .{},
    nursery_stats: SpaceStats = .{},
    major_stats: SpaceStats = .{},

    pub fn init(allocator: std.mem.Allocator) HeapStore {
        return initWithConfig(allocator, .{});
    }

    pub fn initWithConfig(allocator: std.mem.Allocator, config: Config) HeapStore {
        return .{
            .allocator = allocator,
            .backend_kind = config.backend,
            .tuple_page_capacity_words = config.tuple_page_capacity_words,
        };
    }

    pub fn backendKind(self: *const HeapStore) BackendKind {
        return self.backend_kind;
    }

    pub fn hostAllocator(self: *const HeapStore) std.mem.Allocator {
        return self.allocator;
    }

    pub fn deinit(self: *HeapStore, fixed_arena: bool) void {
        self.clear(fixed_arena);
        for (self.tuple_pages.items) |page| {
            page.allocator.free(page.storage);
        }
        self.tuple_pages.deinit(self.allocator);
        self.slots.deinit(self.allocator);
        self.free_indices.deinit(self.allocator);
        self.nursery_handles.deinit(self.allocator);
    }

    pub fn count(self: *const HeapStore) usize {
        return self.object_count;
    }

    pub fn configureNursery(self: *HeapStore, config: NurseryConfig) void {
        self.nursery_config = config;
    }

    pub fn nurseryCount(self: *const HeapStore) usize {
        return self.nursery_stats.objects;
    }

    pub fn spaceStats(self: *const HeapStore, space: Space) SpaceStats {
        return switch (space) {
            .nursery => self.nursery_stats,
            .major => self.major_stats,
        };
    }

    pub fn allocTupleFields(self: *HeapStore, payload_allocator: std.mem.Allocator, len: usize) !TupleStorage {
        if (len == 0) {
            return .{
                .fields = @constCast(&[_]value.Value{}),
                .owner = .static,
            };
        }

        if (self.preferredTupleSpace(len) == .nursery) {
            const fields = try self.allocTupleFieldsFromPage(payload_allocator, len);
            return .{
                .fields = fields,
                .owner = .nursery_page,
            };
        }

        const fields = try payload_allocator.alloc(value.Value, len);
        @memset(fields, value.Value.fromInt(0));
        return .{
            .fields = fields,
            .owner = .host_allocator,
        };
    }

    pub fn add(self: *HeapStore, object: Object) !value.HeapRef {
        return self.addInSpace(object, self.preferredSpace(object));
    }

    pub fn addInSpace(self: *HeapStore, object: Object, space: Space) !value.HeapRef {
        const slot_index: usize = if (self.free_indices.items.len > 0) blk: {
            const reused = self.free_indices.pop() orelse unreachable;
            break :blk @intCast(reused);
        } else self.slots.items.len;

        if (slot_index < self.slots.items.len) {
            const slot = &self.slots.items[slot_index];
            slot.alive = true;
            slot.space = space;
            slot.object = object;
            self.object_count += 1;
            const handle: value.HeapRef = .{ .index = @intCast(slot_index), .generation = slot.generation };
            self.adjustSpaceStats(space, object.allocationCostUnits(), true);
            if (space == .nursery) {
                self.nursery_handles.append(self.allocator, handle) catch {
                    @panic("zort: out of memory while tracking nursery object");
                };
            }
            return handle;
        }

        try self.slots.append(self.allocator, .{
            .generation = 1,
            .alive = true,
            .space = space,
            .object = object,
        });
        self.object_count += 1;
        const handle: value.HeapRef = .{ .index = @intCast(slot_index), .generation = 1 };
        self.adjustSpaceStats(space, object.allocationCostUnits(), true);
        if (space == .nursery) {
            self.nursery_handles.append(self.allocator, handle) catch {
                @panic("zort: out of memory while tracking nursery object");
            };
        }
        return handle;
    }

    pub fn get(self: *const HeapStore, handle: value.HeapRef) ?*Object {
        if (handle.index >= self.slots.items.len) return null;
        const slot = &self.slots.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return &slot.object;
    }

    pub fn reclaim(self: *HeapStore, handle: value.HeapRef, fixed_arena: bool) bool {
        if (handle.index >= self.slots.items.len) return false;
        const slot = &self.slots.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return false;
        self.reclaimSlot(handle.index, fixed_arena);
        return true;
    }

    pub fn reclaimSlot(self: *HeapStore, slot_index: usize, fixed_arena: bool) void {
        if (slot_index >= self.slots.items.len) return;
        const slot = &self.slots.items[slot_index];
        if (!slot.alive) return;

        const reclaimed_units = slot.object.allocationCostUnits();
        const reclaimed_space = slot.space;
        self.releasePageBackedStorage(&slot.object);
        slot.object.deinit(self.allocator, fixed_arena);
        slot.alive = false;
        slot.space = .major;
        slot.generation +%= 1;
        self.object_count -%= 1;
        self.adjustSpaceStats(reclaimed_space, reclaimed_units, false);
        self.free_indices.append(self.allocator, @intCast(slot_index)) catch {
            @panic("zort: out of memory while storing reclaimed slot");
        };
    }

    pub fn clear(self: *HeapStore, fixed_arena: bool) void {
        var i: usize = 0;
        while (i < self.slots.items.len) : (i += 1) {
            self.reclaimSlot(i, fixed_arena);
        }
        self.nursery_handles.clearRetainingCapacity();
    }

    pub fn spaceOf(self: *const HeapStore, handle: value.HeapRef) ?Space {
        if (handle.index >= self.slots.items.len) return null;
        const slot = &self.slots.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return null;
        return slot.space;
    }

    pub fn promote(self: *HeapStore, handle: value.HeapRef) bool {
        if (handle.index >= self.slots.items.len) return false;
        const slot = &self.slots.items[handle.index];
        if (!slot.alive or slot.generation != handle.generation) return false;
        if (slot.space == .major) return true;
        const allocation_units = slot.object.allocationCostUnits();
        slot.object.promoteStorageOwner();
        self.adjustSpaceStats(.nursery, allocation_units, false);
        self.adjustSpaceStats(.major, allocation_units, true);
        slot.space = .major;
        return true;
    }

    pub fn nurseryHandles(self: *const HeapStore) []const value.HeapRef {
        return self.nursery_handles.items;
    }

    pub fn visitLiveConst(
        self: *const HeapStore,
        context: anytype,
        comptime visit: fn (@TypeOf(context), value.HeapRef, Space, *const Object) void,
    ) void {
        for (self.slots.items, 0..) |slot, slot_index| {
            if (!slot.alive) continue;
            visit(context, .{
                .index = @intCast(slot_index),
                .generation = slot.generation,
            }, slot.space, &slot.object);
        }
    }

    pub fn sweepMarked(
        self: *HeapStore,
        fixed_arena: bool,
        context: anytype,
        comptime on_reclaim: fn (@TypeOf(context), value.HeapRef, ObjectKind) void,
        comptime on_keep: fn (@TypeOf(context), *Object, ObjectKind, Space) void,
    ) usize {
        var reclaimed: usize = 0;
        for (self.slots.items, 0..) |*slot, slot_index| {
            if (!slot.alive) continue;
            const handle: value.HeapRef = .{
                .index = @intCast(slot_index),
                .generation = slot.generation,
            };
            const kind = slot.object.kind().?;
            if (!slot.object.marked) {
                on_reclaim(context, handle, kind);
                self.reclaimSlot(slot_index, fixed_arena);
                reclaimed += 1;
                continue;
            }
            on_keep(context, &slot.object, kind, slot.space);
        }
        self.compactNurseryHandles();
        return reclaimed;
    }

    pub fn sweepNurseryMarked(
        self: *HeapStore,
        fixed_arena: bool,
        context: anytype,
        comptime on_reclaim: fn (@TypeOf(context), value.HeapRef, ObjectKind) void,
        comptime on_promote: fn (@TypeOf(context), value.HeapRef, *Object, ObjectKind) void,
    ) usize {
        var reclaimed: usize = 0;
        for (self.nursery_handles.items) |handle| {
            const obj = self.get(handle) orelse continue;
            const space = self.spaceOf(handle) orelse continue;
            if (space != .nursery) continue;
            const kind = obj.kind().?;
            if (!obj.marked) {
                on_reclaim(context, handle, kind);
                _ = self.reclaim(handle, fixed_arena);
                reclaimed += 1;
                continue;
            }
            on_promote(context, handle, obj, kind);
            _ = self.promote(handle);
        }
        self.compactNurseryHandles();
        return reclaimed;
    }

    pub fn compactNurseryHandles(self: *HeapStore) void {
        var write_index: usize = 0;
        for (self.nursery_handles.items) |handle| {
            const space = self.spaceOf(handle) orelse continue;
            if (space != .nursery) continue;
            self.nursery_handles.items[write_index] = handle;
            write_index += 1;
        }
        self.nursery_handles.shrinkRetainingCapacity(write_index);
    }

    pub fn shouldCollectBeforeNurseryAlloc(self: *const HeapStore, additional_units: usize) bool {
        if (!self.nursery_config.enabled) return false;
        const effective_units = @max(additional_units, 1);
        if (self.nursery_stats.objects + 1 > self.nursery_config.max_live_objects) return true;
        if (self.nursery_stats.allocation_units + effective_units > self.nursery_config.max_live_units) return true;
        return false;
    }

    pub const VerifyError = error{
        ObjectCountMismatch,
        InvalidGeneration,
        InvalidFreeSlot,
        LiveSlotInFreeList,
        DuplicateFreeSlot,
        SpaceCountMismatch,
        SpaceAllocationMismatch,
        NurseryTrackingMismatch,
        PageTrackingMismatch,
    };

    pub fn verifyInvariants(self: *const HeapStore) VerifyError!void {
        var alive_count: usize = 0;
        var actual_nursery = SpaceStats{};
        var actual_major = SpaceStats{};
        var tracked_page_words: usize = 0;
        var tracked_page_allocations: usize = 0;
        for (self.slots.items) |slot| {
            if (slot.generation == 0) return error.InvalidGeneration;
            if (slot.alive) {
                alive_count += 1;
                switch (slot.space) {
                    .nursery => {
                        actual_nursery.objects += 1;
                        actual_nursery.allocation_units += slot.object.allocationCostUnits();
                    },
                    .major => {
                        actual_major.objects += 1;
                        actual_major.allocation_units += slot.object.allocationCostUnits();
                    },
                }
                if (slot.object.storageOwner()) |owner| {
                    switch (owner) {
                        .nursery_page, .major_page => {
                            tracked_page_allocations += 1;
                            tracked_page_words += slot.object.scanWords();
                        },
                        .host_allocator, .static => {},
                    }
                }
            }
        }
        if (alive_count != self.object_count) return error.ObjectCountMismatch;
        if (actual_nursery.objects != self.nursery_stats.objects or actual_major.objects != self.major_stats.objects) {
            return error.SpaceCountMismatch;
        }
        if (actual_nursery.allocation_units != self.nursery_stats.allocation_units or actual_major.allocation_units != self.major_stats.allocation_units) {
            return error.SpaceAllocationMismatch;
        }

        var tracked_nursery: usize = 0;
        for (self.nursery_handles.items) |handle| {
            const space = self.spaceOf(handle) orelse continue;
            if (space == .nursery) tracked_nursery += 1;
        }
        if (tracked_nursery != actual_nursery.objects) {
            return error.NurseryTrackingMismatch;
        }

        var live_page_words: usize = 0;
        var live_page_allocations: usize = 0;
        for (self.tuple_pages.items) |page| {
            live_page_words += page.live_words;
            live_page_allocations += page.live_allocations;
        }
        if (live_page_words != tracked_page_words or live_page_allocations != tracked_page_allocations) {
            return error.PageTrackingMismatch;
        }

        for (self.free_indices.items, 0..) |free_index, idx| {
            if (free_index >= self.slots.items.len) return error.InvalidFreeSlot;
            if (self.slots.items[free_index].alive) return error.LiveSlotInFreeList;
            var j: usize = idx + 1;
            while (j < self.free_indices.items.len) : (j += 1) {
                if (self.free_indices.items[j] == free_index) return error.DuplicateFreeSlot;
            }
        }
    }

    fn preferredSpace(self: *const HeapStore, object: Object) Space {
        if (!self.nursery_config.enabled) return .major;
        if (object.kind() == .custom) return .major;
        if (object.allocationCostUnits() > self.nursery_config.max_object_units) return .major;
        return .nursery;
    }

    fn preferredTupleSpace(self: *const HeapStore, len: usize) Space {
        if (!self.nursery_config.enabled) return .major;
        if (@max(len, 1) > self.nursery_config.max_object_units) return .major;
        return .nursery;
    }

    fn adjustSpaceStats(self: *HeapStore, space: Space, allocation_units: usize, comptime increment: bool) void {
        const stats = switch (space) {
            .nursery => &self.nursery_stats,
            .major => &self.major_stats,
        };
        if (increment) {
            stats.objects += 1;
            stats.allocation_units += allocation_units;
        } else {
            stats.objects -%= 1;
            stats.allocation_units -%= allocation_units;
        }
    }

    fn allocTupleFieldsFromPage(self: *HeapStore, payload_allocator: std.mem.Allocator, len: usize) ![]value.Value {
        const page_index = self.findTuplePageWithSpace(len) orelse try self.appendTuplePage(payload_allocator, len);
        const page = &self.tuple_pages.items[page_index];
        const start = page.used_words;
        const end = start + len;
        page.used_words = end;
        page.live_words += len;
        page.live_allocations += 1;
        const fields = page.storage[start..end];
        @memset(fields, value.Value.fromInt(0));
        return fields;
    }

    fn findTuplePageWithSpace(self: *const HeapStore, len: usize) ?usize {
        for (self.tuple_pages.items, 0..) |page, index| {
            if (page.remaining() >= len) return index;
        }
        return null;
    }

    fn appendTuplePage(self: *HeapStore, payload_allocator: std.mem.Allocator, min_words: usize) !usize {
        const capacity = @max(self.tuple_page_capacity_words, min_words);
        const storage = try payload_allocator.alloc(value.Value, capacity);
        try self.tuple_pages.append(self.allocator, .{
            .allocator = payload_allocator,
            .storage = storage,
        });
        return self.tuple_pages.items.len - 1;
    }

    fn releasePageBackedStorage(self: *HeapStore, obj: *const Object) void {
        const owner = obj.storageOwner() orelse return;
        switch (owner) {
            .nursery_page, .major_page => {
                const fields = obj.tupleFieldsConst() orelse return;
                self.releaseTuplePageFields(fields);
            },
            .host_allocator, .static => {},
        }
    }

    fn releaseTuplePageFields(self: *HeapStore, fields: []const value.Value) void {
        if (fields.len == 0) return;
        for (self.tuple_pages.items) |*page| {
            if (!page.containsFields(fields)) continue;
            page.live_words -%= fields.len;
            page.live_allocations -%= 1;
            if (page.live_allocations == 0) page.reset();
            return;
        }
        @panic("zort: page-backed tuple fields were not tracked by heap store");
    }
};

fn bytesToAllocationUnits(byte_count: usize) usize {
    if (byte_count == 0) return 1;
    return std.math.divCeil(usize, byte_count, @sizeOf(usize)) catch unreachable;
}

test "heap_store: add and get object" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);

    const fields = try std.testing.allocator.alloc(value.Value, 1);
    fields[0] = value.Value.fromInt(7);

    const handle = try store.add(Object.initTuple(fields));
    const got = store.get(handle).?;

    try std.testing.expectEqual(@as(u32, 1), handle.generation);
    try std.testing.expectEqual(@as(usize, 1), store.count());
    try std.testing.expectEqual(@as(?Space, .major), store.spaceOf(handle));
    try std.testing.expectEqual(@as(?ObjectKind, .tuple), got.kind());
    try std.testing.expectEqual(value.Value.fromInt(7), got.tupleFields().?[0]);
    try std.testing.expectEqual(BackendKind.slot_registry, store.backendKind());
}

test "heap_store: reclaim enables deterministic LIFO slot reuse" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);

    const fields = try std.testing.allocator.alloc(value.Value, 1);
    fields[0] = value.Value.fromInt(1);
    _ = try store.add(Object.initTuple(fields));

    const h1 = try store.add(Object.initBoxedF64(12.5));
    try std.testing.expectEqual(@as(usize, 2), store.count());
    try std.testing.expect(store.reclaim(h1, false));

    const h2 = try store.add(Object.initBoxedI64(17));
    try std.testing.expectEqual(h1.index, h2.index);
    try std.testing.expectEqual(h1.generation +% 1, h2.generation);
    try std.testing.expect(store.get(h1) == null);
    try std.testing.expectEqual(@as(usize, 2), store.count());
    try std.testing.expectEqual(@as(i64, 17), store.get(h2).?.boxedI64().?);
}

test "heap_store: clear drops all objects" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);

    const left = try std.testing.allocator.alloc(value.Value, 1);
    left[0] = value.Value.fromInt(1);
    _ = try store.add(Object.initTuple(left));

    const buffer = try std.testing.allocator.alloc(u8, 3);
    @memcpy(buffer, "hi\x00");
    _ = try store.add(Object.initString(2, buffer));

    try std.testing.expectEqual(@as(usize, 2), store.count());
    store.clear(false);
    try std.testing.expectEqual(@as(usize, 0), store.count());

    const handle = try store.add(Object.initBoxedI64(42));
    try std.testing.expectEqual(@as(u32, 1), handle.index);
    try std.testing.expectEqual(@as(usize, 1), store.count());
}

test "heap_store: verify invariants accepts healthy store" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);

    const handle = try store.add(Object.initBoxedI64(42));
    _ = handle;
    try store.verifyInvariants();
}

test "heap_store: nursery allocation and promotion are explicit" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    store.configureNursery(.{
        .enabled = true,
        .max_object_units = 4,
    });

    const small = try store.add(Object.initBoxedI64(42));
    try std.testing.expectEqual(@as(?Space, .nursery), store.spaceOf(small));
    try std.testing.expectEqual(@as(usize, 1), store.nurseryCount());
    try std.testing.expectEqual(@as(usize, 1), store.spaceStats(.nursery).allocation_units);
    try std.testing.expect(store.promote(small));
    try std.testing.expectEqual(@as(?Space, .major), store.spaceOf(small));
    try std.testing.expectEqual(@as(usize, 0), store.nurseryCount());
    try std.testing.expectEqual(@as(usize, 1), store.spaceStats(.major).objects);
}

test "heap_store: custom blocks allocate directly in major even with nursery enabled" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    store.configureNursery(.{
        .enabled = true,
        .max_object_units = 64,
    });

    const bytes = try std.testing.allocator.alloc(u8, 8);
    @memset(bytes, 0);
    const custom = try store.add(Object.initCustom(bytes));
    try std.testing.expectEqual(@as(?Space, .major), store.spaceOf(custom));
    try std.testing.expectEqual(@as(usize, 0), store.nurseryCount());
    try std.testing.expectEqual(@as(usize, 1), store.spaceStats(.major).objects);
}

test "heap_store: nursery pressure check uses live words and objects" {
    var store = HeapStore.init(std.testing.allocator);
    defer store.deinit(false);
    store.configureNursery(.{
        .enabled = true,
        .max_object_units = 8,
        .max_live_units = 2,
        .max_live_objects = 2,
    });

    _ = try store.add(Object.initBoxedI64(1));
    try std.testing.expect(!store.shouldCollectBeforeNurseryAlloc(1));
    _ = try store.add(Object.initBoxedI64(2));
    try std.testing.expect(store.shouldCollectBeforeNurseryAlloc(1));
}

test "heap_store: callback sweep hides slot-registry iteration details" {
    const Context = struct {
        reclaimed: usize = 0,
        kept: usize = 0,

        fn onReclaim(ctx: *@This(), _: value.HeapRef, _: ObjectKind) void {
            ctx.reclaimed += 1;
        }

        fn onKeep(ctx: *@This(), obj: *Object, _: ObjectKind, _: Space) void {
            ctx.kept += 1;
            obj.marked = false;
        }
    };

    var store = HeapStore.initWithConfig(std.testing.allocator, .{
        .backend = .slot_registry,
    });
    defer store.deinit(false);

    const kept = try store.add(Object.initBoxedI64(1));
    const dropped = try store.add(Object.initBoxedI64(2));
    _ = dropped;
    store.get(kept).?.marked = true;

    var ctx = Context{};
    const reclaimed = store.sweepMarked(false, &ctx, Context.onReclaim, Context.onKeep);

    try std.testing.expectEqual(@as(usize, 1), reclaimed);
    try std.testing.expectEqual(@as(usize, 1), ctx.reclaimed);
    try std.testing.expectEqual(@as(usize, 1), ctx.kept);
    try std.testing.expectEqual(@as(usize, 1), store.count());
    try std.testing.expect(!store.get(kept).?.marked);
}
