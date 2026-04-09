const std = @import("std");
const event_sink = @import("event_sink.zig");
const heap_store = @import("heap_store.zig");
const value = @import("value.zig");

pub const HeapRef = value.HeapRef;
pub const ObjectKind = heap_store.ObjectKind;
pub const Space = heap_store.Space;
pub const EventSink = event_sink.EventSink;

pub const Config = struct {
    enabled: bool = false,
    sample_interval_words: usize = 64,
    capture_backtraces: bool = false,
};

pub const SampleView = struct {
    sample_ordinal: u64,
    kind: ObjectKind,
    size: usize,
    current_space: Space,
    promotion_count: usize,
    backtrace_sites: []const u32,
};

const Sample = struct {
    sample_ordinal: u64,
    kind: ObjectKind,
    size: usize,
    current_space: Space,
    promotion_count: usize = 0,
    backtrace_sites: []u32 = &.{},
};

pub const MemprofState = struct {
    allocator: std.mem.Allocator,
    event_sink: EventSink,
    config: Config,
    allocated_words: usize = 0,
    next_sample_words: usize = 1,
    sample_sequence: u64 = 0,
    samples: std.AutoHashMapUnmanaged(u64, Sample) = .{},

    pub fn init(allocator: std.mem.Allocator, sink: EventSink, config: Config) MemprofState {
        return .{
            .allocator = allocator,
            .event_sink = sink,
            .config = config,
            .next_sample_words = if (config.sample_interval_words == 0) 1 else config.sample_interval_words,
        };
    }

    pub fn deinit(self: *MemprofState) void {
        var it = self.samples.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.backtrace_sites);
        }
        self.samples.deinit(self.allocator);
    }

    pub fn enabled(self: *const MemprofState) bool {
        return self.config.enabled and self.config.sample_interval_words > 0;
    }

    pub fn capturesBacktraces(self: *const MemprofState) bool {
        return self.enabled() and self.config.capture_backtraces;
    }

    pub fn trackedSampleCount(self: *const MemprofState) usize {
        return self.samples.count();
    }

    pub fn beginAllocation(self: *MemprofState, object_words: usize) ?u64 {
        if (!self.enabled()) return null;

        const effective_words = @max(object_words, 1);
        self.allocated_words +%= effective_words;
        if (self.allocated_words < self.next_sample_words) return null;

        self.sample_sequence +%= 1;
        while (self.next_sample_words <= self.allocated_words) {
            self.next_sample_words +%= self.config.sample_interval_words;
        }
        return self.sample_sequence;
    }

    pub fn recordAllocation(
        self: *MemprofState,
        sample_ordinal: u64,
        handle: HeapRef,
        kind: ObjectKind,
        size: usize,
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
                .size = size,
                .current_space = space,
                .promotion_count = 0,
                .backtrace_sites = copied_sites,
            };
        } else {
            self.samples.put(self.allocator, key, .{
                .sample_ordinal = sample_ordinal,
                .kind = kind,
                .size = size,
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
            .size = size,
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
            .size = sample.size,
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
            .size = sample.size,
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
            .size = sample.size,
            .current_space = sample.current_space,
            .promotion_count = sample.promotion_count,
            .backtrace_sites = sample.backtrace_sites,
        };
    }

    fn handleKey(handle: HeapRef) u64 {
        return (@as(u64, handle.index) << 32) | @as(u64, handle.generation);
    }
};

test "memprof: samples allocations and lifecycle transitions" {
    var recorder = event_sink.Recorder{};
    var memprof = MemprofState.init(std.testing.allocator, recorder.sink(), .{
        .enabled = true,
        .sample_interval_words = 2,
        .capture_backtraces = true,
    });
    defer memprof.deinit();

    try std.testing.expectEqual(@as(?u64, null), memprof.beginAllocation(1));
    const sample_ordinal = memprof.beginAllocation(1).?;
    try std.testing.expectEqual(@as(u64, 1), sample_ordinal);

    const handle = HeapRef{ .index = 2, .generation = 9 };
    memprof.recordAllocation(sample_ordinal, handle, .tuple, 1, .nursery, &.{ 11, 22 });

    const sample = memprof.sampleFor(handle).?;
    try std.testing.expectEqual(@as(u64, 1), sample.sample_ordinal);
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
