const value = @import("value.zig");

pub const Value = value.Value;

pub const RootVisitor = struct {
    ctx: ?*anyopaque,
    visit_fn: *const fn (?*anyopaque, Value) void,

    pub fn visit(self: RootVisitor, rooted: Value) void {
        self.visit_fn(self.ctx, rooted);
    }
};

pub const RootProvider = struct {
    name: []const u8,
    ctx: ?*anyopaque,
    count_fn: *const fn (?*anyopaque) usize,
    visit_fn: *const fn (?*anyopaque, RootVisitor) void,

    pub fn count(self: RootProvider) usize {
        return self.count_fn(self.ctx);
    }

    pub fn visit(self: RootProvider, visitor: RootVisitor) void {
        self.visit_fn(self.ctx, visitor);
    }
};
