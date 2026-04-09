const std = @import("std");

pub const Orders = struct {
    pub const observe = std.builtin.AtomicOrder.acquire;
    pub const publish = std.builtin.AtomicOrder.release;
    pub const update = std.builtin.AtomicOrder.acq_rel;
    pub const total = std.builtin.AtomicOrder.seq_cst;
};

pub const AtomicCounter = struct {
    raw: std.atomic.Value(usize),

    pub fn init(value: usize) AtomicCounter {
        return .{ .raw = std.atomic.Value(usize).init(value) };
    }

    pub fn load(self: *const AtomicCounter) usize {
        return self.raw.load(Orders.observe);
    }

    pub fn store(self: *AtomicCounter, value: usize) void {
        self.raw.store(value, Orders.publish);
    }

    pub fn increment(self: *AtomicCounter) usize {
        return self.raw.fetchAdd(1, Orders.update) + 1;
    }

    pub fn add(self: *AtomicCounter, delta: usize) usize {
        return self.raw.fetchAdd(delta, Orders.update) + delta;
    }

    pub fn compareExchange(self: *AtomicCounter, expected: usize, new: usize) ?usize {
        return self.raw.cmpxchgStrong(expected, new, Orders.update, Orders.observe);
    }
};

pub const AtomicFlag = struct {
    raw: std.atomic.Value(u8),

    pub fn init(value: bool) AtomicFlag {
        return .{ .raw = std.atomic.Value(u8).init(@intFromBool(value)) };
    }

    pub fn isSet(self: *const AtomicFlag) bool {
        return self.raw.load(Orders.observe) != 0;
    }

    pub fn store(self: *AtomicFlag, value: bool) void {
        self.raw.store(@intFromBool(value), Orders.publish);
    }

    pub fn set(self: *AtomicFlag) bool {
        return self.raw.swap(1, Orders.update) != 0;
    }

    pub fn clear(self: *AtomicFlag) bool {
        return self.raw.swap(0, Orders.update) != 0;
    }

    pub fn take(self: *AtomicFlag) bool {
        return self.clear();
    }
};

pub const OptionalTokenCell = struct {
    raw: std.atomic.Value(u64),

    const Self = @This();
    const null_sentinel = std.math.maxInt(u64);

    pub fn init(value: ?u64) Self {
        return .{ .raw = std.atomic.Value(u64).init(encode(value)) };
    }

    pub fn load(self: *const Self) ?u64 {
        return decode(self.raw.load(Orders.observe));
    }

    pub fn store(self: *Self, value: ?u64) void {
        self.raw.store(encode(value), Orders.publish);
    }

    pub fn swap(self: *Self, value: ?u64) ?u64 {
        return decode(self.raw.swap(encode(value), Orders.update));
    }

    pub fn claim(self: *Self, token: u64) bool {
        return self.raw.cmpxchgStrong(null_sentinel, token, Orders.update, Orders.observe) == null;
    }

    pub fn release(self: *Self, token: u64) bool {
        return self.raw.cmpxchgStrong(token, null_sentinel, Orders.update, Orders.observe) == null;
    }

    fn encode(value: ?u64) u64 {
        return value orelse null_sentinel;
    }

    fn decode(encoded: u64) ?u64 {
        if (encoded == null_sentinel) return null;
        return encoded;
    }
};

pub fn OptionalHandleCell(comptime Handle: type) type {
    comptime {
        if (!@hasField(Handle, "index") or !@hasField(Handle, "generation")) {
            @compileError("OptionalHandleCell requires `index` and `generation` fields");
        }
    }

    return struct {
        raw: std.atomic.Value(u64),

        const Self = @This();
        const null_sentinel = std.math.maxInt(u64);

        pub fn init(value: ?Handle) Self {
            return .{ .raw = std.atomic.Value(u64).init(encode(value)) };
        }

        pub fn load(self: *const Self) ?Handle {
            return decode(self.raw.load(Orders.observe));
        }

        pub fn store(self: *Self, value: ?Handle) void {
            self.raw.store(encode(value), Orders.publish);
        }

        pub fn swap(self: *Self, value: ?Handle) ?Handle {
            return decode(self.raw.swap(encode(value), Orders.update));
        }

        pub fn clear(self: *Self) ?Handle {
            return self.swap(null);
        }

        fn encode(value: ?Handle) u64 {
            if (value) |handle| {
                const index: u64 = @intCast(@field(handle, "index"));
                const generation: u64 = @intCast(@field(handle, "generation"));
                return (generation << 32) | index;
            }
            return null_sentinel;
        }

        fn decode(encoded: u64) ?Handle {
            if (encoded == null_sentinel) return null;
            return .{
                .index = @intCast(encoded & std.math.maxInt(u32)),
                .generation = @intCast(encoded >> 32),
            };
        }
    };
}

test "atomic_primitives: flag set and clear are explicit" {
    var flag = AtomicFlag.init(false);
    try std.testing.expect(!flag.isSet());
    try std.testing.expect(!flag.set());
    try std.testing.expect(flag.isSet());
    try std.testing.expect(flag.clear());
    try std.testing.expect(!flag.isSet());
}

test "atomic_primitives: optional handle cell round-trips handles" {
    const FiberHandle = struct {
        index: u32,
        generation: u32,
    };

    const Cell = OptionalHandleCell(FiberHandle);
    var cell = Cell.init(null);
    try std.testing.expectEqual(@as(?FiberHandle, null), cell.load());

    const handle = FiberHandle{ .index = 7, .generation = 11 };
    cell.store(handle);
    try std.testing.expectEqual(handle, cell.load().?);
    try std.testing.expectEqual(handle, cell.clear().?);
    try std.testing.expectEqual(@as(?FiberHandle, null), cell.load());
}

test "atomic_primitives: optional token cell claims and releases ownership" {
    var token = OptionalTokenCell.init(null);
    try std.testing.expectEqual(@as(?u64, null), token.load());
    try std.testing.expect(token.claim(42));
    try std.testing.expectEqual(@as(?u64, 42), token.load());
    try std.testing.expect(!token.claim(7));
    try std.testing.expect(!token.release(7));
    try std.testing.expect(token.release(42));
    try std.testing.expectEqual(@as(?u64, null), token.load());
}

test "atomic_primitives: counter is safe under threaded increments" {
    var counter = AtomicCounter.init(0);

    const Worker = struct {
        fn run(target: *AtomicCounter, iterations: usize) void {
            for (0..iterations) |_| _ = target.increment();
        }
    };

    var threads: [4]std.Thread = undefined;
    for (&threads) |*thread| {
        thread.* = try std.Thread.spawn(.{}, Worker.run, .{ &counter, 1000 });
    }
    for (threads) |thread| thread.join();

    try std.testing.expectEqual(@as(usize, 4000), counter.load());
}
