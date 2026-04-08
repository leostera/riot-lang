const std = @import("std");

pub const Value = @import("value.zig").Value;
pub const Tag = @import("value.zig").Tag;
pub const HeapRef = @import("value.zig").HeapRef;
pub const Error = @import("runtime.zig").Error;
pub const HeapStore = @import("runtime.zig").HeapStore;
pub const Object = @import("runtime.zig").Object;
pub const ObjectKind = @import("runtime.zig").ObjectKind;
pub const Runtime = @import("runtime.zig").Runtime;

test {
    std.testing.refAllDecls(@import("value.zig"));
    std.testing.refAllDecls(@import("heap_store.zig"));
    std.testing.refAllDecls(@import("runtime.zig"));
}
