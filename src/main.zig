const std = @import("std");

const assemble = @import("./x86_64/assemble.zig");

pub fn main() !void {}

comptime {
    std.testing.refAllDecls(@This());
}
