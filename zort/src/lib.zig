const std = @import("std");

pub const Value = @import("value.zig").Value;
pub const Tag = @import("value.zig").Tag;
pub const HeapRef = @import("value.zig").HeapRef;
pub const Error = @import("runtime.zig").Error;
pub const Collector = @import("runtime.zig").Collector;
pub const Event = @import("runtime.zig").Event;
pub const EventCounters = @import("runtime.zig").EventCounters;
pub const EventRecorder = @import("runtime.zig").EventRecorder;
pub const EventSink = @import("runtime.zig").EventSink;
pub const Language = @import("runtime.zig").Language;
pub const HeapStore = @import("runtime.zig").HeapStore;
pub const Object = @import("runtime.zig").Object;
pub const ObjectKind = @import("runtime.zig").ObjectKind;
pub const Mutator = @import("runtime.zig").Mutator;
pub const RootRegistry = @import("runtime.zig").RootRegistry;
pub const RootHandle = @import("runtime.zig").RootHandle;
pub const Runtime = @import("runtime.zig").Runtime;

test {
    std.testing.refAllDecls(@import("value.zig"));
    std.testing.refAllDecls(@import("collector.zig"));
    std.testing.refAllDecls(@import("event_sink.zig"));
    std.testing.refAllDecls(@import("heap_store.zig"));
    std.testing.refAllDecls(@import("language.zig"));
    std.testing.refAllDecls(@import("mutator.zig"));
    std.testing.refAllDecls(@import("root_registry.zig"));
    std.testing.refAllDecls(@import("runtime.zig"));
}
