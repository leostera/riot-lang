const std = @import("std");
const value = @import("value.zig");

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
