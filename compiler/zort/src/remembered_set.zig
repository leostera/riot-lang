const std = @import("std");
const heap_store = @import("heap_store.zig");
const value = @import("value.zig");

pub const HeapStore = heap_store.HeapStore;
pub const Space = heap_store.Space;
pub const Value = value.Value;
pub const HeapRef = value.HeapRef;

pub const RememberedSet = struct {
    allocator: std.mem.Allocator,
    targets: std.ArrayListUnmanaged(HeapRef) = .{},

    pub fn init(allocator: std.mem.Allocator) RememberedSet {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RememberedSet) void {
        self.targets.deinit(self.allocator);
    }

    pub fn clear(self: *RememberedSet) void {
        self.targets.clearRetainingCapacity();
    }

    pub fn count(self: *const RememberedSet) usize {
        return self.targets.items.len;
    }

    pub fn targetsSlice(self: *const RememberedSet) []const HeapRef {
        return self.targets.items;
    }

    pub fn compact(self: *RememberedSet, heap: *const HeapStore) void {
        var write_index: usize = 0;
        for (self.targets.items) |target| {
            const target_space = heap.spaceOf(target) orelse continue;
            if (target_space != .major) continue;
            if (!targetHasNurseryChildren(heap, target)) continue;
            self.targets.items[write_index] = target;
            write_index += 1;
        }
        self.targets.shrinkRetainingCapacity(write_index);
    }

    pub fn ownerCount(self: *const RememberedSet, target: HeapRef) usize {
        for (self.targets.items) |candidate| {
            if (candidate.index == target.index and candidate.generation == target.generation) return 1;
        }
        return 0;
    }

    pub fn record(self: *RememberedSet, target: HeapRef) !bool {
        if (self.ownerCount(target) != 0) return false;
        try self.targets.append(self.allocator, target);
        return true;
    }
};

fn targetHasNurseryChildren(heap: *const HeapStore, target: HeapRef) bool {
    const obj = heap.get(target) orelse return false;
    const fields = obj.tupleFields() orelse return false;
    for (fields) |field| {
        const handle = field.asHeapRef() orelse continue;
        const space = heap.spaceOf(handle) orelse continue;
        if (space == .nursery) return true;
    }
    return false;
}

test "remembered_set: records only unique major targets" {
    var set = RememberedSet.init(std.testing.allocator);
    defer set.deinit();

    const target = HeapRef{ .index = 1, .generation = 1 };
    try std.testing.expect(try set.record(target));
    try std.testing.expect(!(try set.record(target)));
    try std.testing.expectEqual(@as(usize, 1), set.count());
    try std.testing.expectEqual(@as(usize, 1), set.ownerCount(target));
}

test "remembered_set: compact keeps only major targets with nursery children" {
    var set = RememberedSet.init(std.testing.allocator);
    defer set.deinit();
    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    heap.configureNursery(.{
        .enabled = true,
        .max_object_units = 4,
    });

    const major_fields = try std.testing.allocator.alloc(Value, 1);
    major_fields[0] = Value.fromInt(0);
    const major = try heap.addInSpace(heap_store.Object.initTuple(major_fields), .major);

    const nursery = try heap.addInSpace(heap_store.Object.initBoxedI64(2), .nursery);
    const other_major = try heap.addInSpace(heap_store.Object.initBoxedI64(3), .major);
    heap.get(major).?.tupleFields().?[0] = Value.fromHeapRef(nursery);

    try std.testing.expect(try set.record(major));
    try std.testing.expectEqual(@as(usize, 1), set.count());

    set.compact(&heap);
    try std.testing.expectEqual(@as(usize, 1), set.count());
    try std.testing.expectEqual(major, set.targetsSlice()[0]);

    heap.get(major).?.tupleFields().?[0] = Value.fromHeapRef(other_major);
    set.compact(&heap);
    try std.testing.expectEqual(@as(usize, 0), set.count());
}
