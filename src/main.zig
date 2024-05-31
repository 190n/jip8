const std = @import("std");

const assemble = @import("./x86_64/assemble.zig");

pub fn main() !void {
    var code = std.ArrayListAligned(u8, std.mem.page_size).init(std.heap.page_allocator);
    defer code.deinit();
    try assemble.movRegImm(&code, .rax, -1);
    @breakpoint();
}

comptime {
    std.testing.refAllDecls(@This());
}
