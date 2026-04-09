const std = @import("std");
const heap_store = @import("heap_store.zig");
const value = @import("value.zig");

pub const HeapStore = heap_store.HeapStore;
pub const Space = heap_store.Space;
pub const Value = value.Value;
pub const HeapRef = value.HeapRef;

pub const RememberedEdge = struct {
    target: HeapRef,
    value: HeapRef,
};

pub const RememberedSet = struct {
    allocator: std.mem.Allocator,
    edges: std.ArrayListUnmanaged(RememberedEdge) = .{},

    pub fn init(allocator: std.mem.Allocator) RememberedSet {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RememberedSet) void {
        self.edges.deinit(self.allocator);
    }

    pub fn clear(self: *RememberedSet) void {
        self.edges.clearRetainingCapacity();
    }

    pub fn count(self: *const RememberedSet) usize {
        return self.edges.items.len;
    }

    pub fn edgesSlice(self: *const RememberedSet) []const RememberedEdge {
        return self.edges.items;
    }

    pub fn compact(self: *RememberedSet, heap: *const HeapStore) void {
        var write_index: usize = 0;
        for (self.edges.items) |edge| {
            const target_space = heap.spaceOf(edge.target) orelse continue;
            const value_space = heap.spaceOf(edge.value) orelse continue;
            if (target_space != .major or value_space != .nursery) continue;
            self.edges.items[write_index] = edge;
            write_index += 1;
        }
        self.edges.shrinkRetainingCapacity(write_index);
    }

    pub fn ownerCount(self: *const RememberedSet, target: HeapRef) usize {
        var total: usize = 0;
        for (self.edges.items) |edge| {
            if (edge.target.index == target.index and edge.target.generation == target.generation) {
                total += 1;
            }
        }
        return total;
    }

    pub fn record(self: *RememberedSet, target: HeapRef, next: Value) !bool {
        const next_handle = next.asHeapRef() orelse return false;
        try self.edges.append(self.allocator, .{
            .target = target,
            .value = next_handle,
        });
        return true;
    }
};

test "remembered_set: records only block-to-block edges" {
    var set = RememberedSet.init(std.testing.allocator);
    defer set.deinit();

    const target = HeapRef{ .index = 1, .generation = 1 };
    try std.testing.expect(!(try set.record(target, Value.fromInt(7))));
    try std.testing.expectEqual(@as(usize, 0), set.count());

    try std.testing.expect(try set.record(target, Value.fromHeapRef(.{ .index = 2, .generation = 1 })));
    try std.testing.expectEqual(@as(usize, 1), set.count());
    try std.testing.expectEqual(@as(usize, 1), set.ownerCount(target));
}

test "remembered_set: compact drops invalid and non-major-to-nursery edges" {
    var set = RememberedSet.init(std.testing.allocator);
    defer set.deinit();
    var heap = HeapStore.init(std.testing.allocator);
    defer heap.deinit(false);
    heap.configureNursery(.{
        .enabled = true,
        .max_object_words = 4,
    });

    const major = try heap.addInSpace(heap_store.Object.initBoxedI64(1), .major);
    const nursery = try heap.addInSpace(heap_store.Object.initBoxedI64(2), .nursery);
    const other_major = try heap.addInSpace(heap_store.Object.initBoxedI64(3), .major);
    try std.testing.expect(try set.record(major, Value.fromHeapRef(nursery)));
    try std.testing.expect(try set.record(major, Value.fromHeapRef(other_major)));
    try std.testing.expectEqual(@as(usize, 2), set.count());

    set.compact(&heap);
    try std.testing.expectEqual(@as(usize, 1), set.count());
    try std.testing.expectEqual(major, set.edgesSlice()[0].target);
    try std.testing.expectEqual(nursery, set.edgesSlice()[0].value);
}
