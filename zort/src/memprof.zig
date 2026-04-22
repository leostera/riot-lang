const std = @import("std");
const event_sink = @import("event_sink.zig");
const heap_store = @import("heap_store.zig");
const value = @import("value.zig");

pub const HeapRef = value.HeapRef;
pub const ObjectKind = heap_store.ObjectKind;
pub const Space = heap_store.Space;
pub const EventSink = event_sink.EventSink;

pub const SamplingMode = enum {
    probabilistic_allocation_units,
    deterministic_interval,
};

pub const Config = struct {
    enabled: bool = false,
    sample_interval_units: usize = 64,
    capture_backtraces: bool = false,
    sampling: SamplingMode = .probabilistic_allocation_units,
    seed: ?u64 = null,
};

pub const SampleView = struct {
    sample_ordinal: u64,
    kind: ObjectKind,
    payload_bytes: usize,
    storage_bytes: usize,
    scan_words: usize,
    allocation_cost_units: usize,
    current_space: Space,
    promotion_count: usize,
    backtrace_sites: []const u32,
};

const Sample = struct {
    sample_ordinal: u64,
    kind: ObjectKind,
    payload_bytes: usize,
    storage_bytes: usize,
    scan_words: usize,
    allocation_cost_units: usize,
    current_space: Space,
    promotion_count: usize = 0,
    backtrace_sites: []u32 = &.{},
};

pub const MemprofState = struct {
    allocator: std.mem.Allocator,
    event_sink: EventSink,
    config: Config,
    units_until_next_sample: usize = 1,
    sample_sequence: u64 = 0,
    rng_state: u64 = 0,
    samples: std.AutoHashMapUnmanaged(u64, Sample) = .{},

    pub fn init(allocator: std.mem.Allocator, sink: EventSink, config: Config) MemprofState {
        var state = MemprofState{
            .allocator = allocator,
            .event_sink = sink,
            .config = config,
            .rng_state = config.seed orelse defaultSeed(),
        };
        state.units_until_next_sample = state.drawNextGap();
        return state;
    }

    pub fn deinit(self: *MemprofState) void {
        var it = self.samples.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.backtrace_sites);
        }
        self.samples.deinit(self.allocator);
    }

    pub fn enabled(self: *const MemprofState) bool {
        return self.config.enabled and self.config.sample_interval_units > 0;
    }

    pub fn capturesBacktraces(self: *const MemprofState) bool {
        return self.enabled() and self.config.capture_backtraces;
    }

    pub fn trackedSampleCount(self: *const MemprofState) usize {
        return self.samples.count();
    }

    pub fn beginAllocation(self: *MemprofState, allocation_cost_units: usize) ?u64 {
        if (!self.enabled()) return null;

        const effective_units = @max(allocation_cost_units, 1);
        if (effective_units < self.units_until_next_sample) {
            self.units_until_next_sample -= effective_units;
            return null;
        }

        self.sample_sequence +%= 1;
        const leftover_units = effective_units - self.units_until_next_sample;
        self.units_until_next_sample = self.drawNextGap();
        if (leftover_units > 0) {
            if (leftover_units >= self.units_until_next_sample) {
                self.units_until_next_sample = 1;
            } else {
                self.units_until_next_sample -= leftover_units;
            }
        }
        return self.sample_sequence;
    }

    pub fn recordAllocation(
        self: *MemprofState,
        sample_ordinal: u64,
        handle: HeapRef,
        kind: ObjectKind,
        payload_bytes: usize,
        storage_bytes: usize,
        scan_words: usize,
        allocation_cost_units: usize,
        space: Space,
        backtrace_sites: []const u32,
    ) void {
        if (!self.enabled()) return;

        const copied_sites = if (self.capturesBacktraces() and backtrace_sites.len > 0)
            self.allocator.dupe(u32, backtrace_sites) catch return
        else
            self.allocator.dupe(u32, &.{}) catch return;

        const key = handleKey(handle);
        if (self.samples.getPtr(key)) |existing| {
            self.allocator.free(existing.backtrace_sites);
            existing.* = .{
                .sample_ordinal = sample_ordinal,
                .kind = kind,
                .payload_bytes = payload_bytes,
                .storage_bytes = storage_bytes,
                .scan_words = scan_words,
                .allocation_cost_units = allocation_cost_units,
                .current_space = space,
                .promotion_count = 0,
                .backtrace_sites = copied_sites,
            };
        } else {
            self.samples.put(self.allocator, key, .{
                .sample_ordinal = sample_ordinal,
                .kind = kind,
                .payload_bytes = payload_bytes,
                .storage_bytes = storage_bytes,
                .scan_words = scan_words,
                .allocation_cost_units = allocation_cost_units,
                .current_space = space,
                .backtrace_sites = copied_sites,
            }) catch {
                self.allocator.free(copied_sites);
                return;
            };
        }

        self.event_sink.emit(.{ .memprof = .{
            .action = .sampled_alloc,
            .handle = handle,
            .sample_ordinal = sample_ordinal,
            .kind = kind,
            .payload_bytes = payload_bytes,
            .storage_bytes = storage_bytes,
            .scan_words = scan_words,
            .allocation_cost_units = allocation_cost_units,
            .space = space,
            .promotion_count = 0,
            .backtrace_depth = backtrace_sites.len,
        } });
    }

    pub fn notePromotion(self: *MemprofState, handle: HeapRef, next_space: Space) void {
        const sample = self.samples.getPtr(handleKey(handle)) orelse return;
        if (sample.current_space == next_space) return;
        sample.current_space = next_space;
        sample.promotion_count +%= 1;
        self.event_sink.emit(.{ .memprof = .{
            .action = .promoted,
            .handle = handle,
            .sample_ordinal = sample.sample_ordinal,
            .kind = sample.kind,
            .payload_bytes = sample.payload_bytes,
            .storage_bytes = sample.storage_bytes,
            .scan_words = sample.scan_words,
            .allocation_cost_units = sample.allocation_cost_units,
            .space = next_space,
            .promotion_count = sample.promotion_count,
            .backtrace_depth = sample.backtrace_sites.len,
        } });
    }

    pub fn noteReclaim(self: *MemprofState, handle: HeapRef) void {
        const key = handleKey(handle);
        const sample_ptr = self.samples.getPtr(key) orelse return;
        const sample = sample_ptr.*;
        _ = self.samples.remove(key);
        defer self.allocator.free(sample.backtrace_sites);

        self.event_sink.emit(.{ .memprof = .{
            .action = .reclaimed,
            .handle = handle,
            .sample_ordinal = sample.sample_ordinal,
            .kind = sample.kind,
            .payload_bytes = sample.payload_bytes,
            .storage_bytes = sample.storage_bytes,
            .scan_words = sample.scan_words,
            .allocation_cost_units = sample.allocation_cost_units,
            .space = sample.current_space,
            .promotion_count = sample.promotion_count,
            .backtrace_depth = sample.backtrace_sites.len,
        } });
    }

    pub fn sampleFor(self: *const MemprofState, handle: HeapRef) ?SampleView {
        const sample = self.samples.get(handleKey(handle)) orelse return null;
        return .{
            .sample_ordinal = sample.sample_ordinal,
            .kind = sample.kind,
            .payload_bytes = sample.payload_bytes,
            .storage_bytes = sample.storage_bytes,
            .scan_words = sample.scan_words,
            .allocation_cost_units = sample.allocation_cost_units,
            .current_space = sample.current_space,
            .promotion_count = sample.promotion_count,
            .backtrace_sites = sample.backtrace_sites,
        };
    }

    fn handleKey(handle: HeapRef) u64 {
        return (@as(u64, handle.index) << 32) | @as(u64, handle.generation);
    }

    fn drawNextGap(self: *MemprofState) usize {
        const interval = @max(self.config.sample_interval_units, 1);
        return switch (self.config.sampling) {
            .deterministic_interval => interval,
            .probabilistic_allocation_units => self.drawProbabilisticGap(interval),
        };
    }

    fn drawProbabilisticGap(self: *MemprofState, interval: usize) usize {
        if (interval <= 1) return 1;

        const p = 1.0 / @as(f64, @floatFromInt(interval));
        const u = self.nextUnitF64();
        const numerator = std.math.log1p(-u);
        const denominator = std.math.log1p(-p);
        const gap = @as(usize, @intFromFloat(@floor(numerator / denominator))) + 1;
        return @max(gap, 1);
    }

    fn nextUnitF64(self: *MemprofState) f64 {
        const bits = self.nextRandomU64() >> 11;
        const denom = 9007199254740992.0;
        return (@as(f64, @floatFromInt(bits)) + 0.5) / denom;
    }

    fn nextRandomU64(self: *MemprofState) u64 {
        self.rng_state +%= 0x9e3779b97f4a7c15;
        var z = self.rng_state;
        z = (z ^ (z >> 30)) *% 0xbf58476d1ce4e5b9;
        z = (z ^ (z >> 27)) *% 0x94d049bb133111eb;
        return z ^ (z >> 31);
    }

    fn defaultSeed() u64 {
        const ns: u128 = @intCast(std.time.nanoTimestamp());
        return @truncate(ns);
    }
};

test "memprof: deterministic sampling preserves simple lifecycle tests" {
    var recorder = event_sink.Recorder{};
    var memprof = MemprofState.init(std.testing.allocator, recorder.sink(), .{
        .enabled = true,
        .sample_interval_units = 2,
        .capture_backtraces = true,
        .sampling = .deterministic_interval,
    });
    defer memprof.deinit();

    try std.testing.expectEqual(@as(?u64, null), memprof.beginAllocation(1));
    const sample_ordinal = memprof.beginAllocation(1).?;
    try std.testing.expectEqual(@as(u64, 1), sample_ordinal);

    const handle = HeapRef{ .index = 2, .generation = 9 };
    memprof.recordAllocation(sample_ordinal, handle, .tuple, 16, 16, 2, 2, .nursery, &.{ 11, 22 });

    const sample = memprof.sampleFor(handle).?;
    try std.testing.expectEqual(@as(u64, 1), sample.sample_ordinal);
    try std.testing.expectEqual(@as(usize, 16), sample.payload_bytes);
    try std.testing.expectEqual(@as(usize, 16), sample.storage_bytes);
    try std.testing.expectEqual(@as(usize, 2), sample.scan_words);
    try std.testing.expectEqual(@as(usize, 2), sample.allocation_cost_units);
    try std.testing.expectEqual(@as(usize, 2), sample.backtrace_sites.len);
    try std.testing.expectEqualSlices(u32, &.{ 11, 22 }, sample.backtrace_sites);

    memprof.notePromotion(handle, .major);
    try std.testing.expectEqual(@as(usize, 1), memprof.sampleFor(handle).?.promotion_count);
    memprof.noteReclaim(handle);
    try std.testing.expect(memprof.sampleFor(handle) == null);

    const counters = recorder.snapshot();
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_samples);
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_promotions);
    try std.testing.expectEqual(@as(usize, 1), counters.memprof_reclaims);
}

test "memprof: disabled state never samples" {
    var memprof = MemprofState.init(std.testing.allocator, EventSink.noop(), .{});
    defer memprof.deinit();

    try std.testing.expect(memprof.beginAllocation(64) == null);
    try std.testing.expectEqual(@as(usize, 0), memprof.trackedSampleCount());
}

test "memprof: probabilistic sampling is reproducible with a fixed seed" {
    var left = MemprofState.init(std.testing.allocator, EventSink.noop(), .{
        .enabled = true,
        .sample_interval_units = 8,
        .sampling = .probabilistic_allocation_units,
        .seed = 42,
    });
    defer left.deinit();

    var right = MemprofState.init(std.testing.allocator, EventSink.noop(), .{
        .enabled = true,
        .sample_interval_units = 8,
        .sampling = .probabilistic_allocation_units,
        .seed = 42,
    });
    defer right.deinit();

    const pattern = [_]usize{ 1, 3, 2, 5, 1, 4, 2, 8, 1, 1, 6, 2, 7, 3, 1, 4 };
    for (pattern) |units| {
        try std.testing.expectEqual(left.beginAllocation(units), right.beginAllocation(units));
    }
}
