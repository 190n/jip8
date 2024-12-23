const std = @import("std");
const builtin = @import("builtin");

const Cpu = @import("./chip8.zig").Cpu;

pub const x86_64 = @import("./x86_64.zig");
pub const riscv64 = @import("./riscv64.zig");

const Assembler = @import("./Assembler.zig");

fn meow(x: u32) callconv(.C) void {
    std.log.info("meow: \"{s}\"", .{std.mem.asBytes(&x)});
}

pub fn main() !void {
    const stack = try std.heap.page_allocator.alignedAlloc(u8, std.mem.page_size, 4 * std.mem.page_size);
    defer std.heap.page_allocator.free(stack);

    var assembler = switch (builtin.cpu.arch) {
        .x86_64 => x86_64.Assembler.init(std.heap.page_allocator),
        .riscv64 => riscv64.Assembler.init(std.heap.page_allocator, builtin.cpu.features),
        else => @compileError("unsupported architecture"),
    };
    defer assembler.deinit();

    switch (builtin.cpu.arch) {
        .x86_64 => {
            try assembler.movRegImm(.ax, @intFromError(error.HelloX86_64));
            try assembler.ret();
        },
        .riscv64 => {
            try assembler.li(.a0, @intFromError(error.HelloRiscv64));
            try assembler.ret();
        },
        else => unreachable,
    }

    try assembler.makeExecutable();

    var cpu = Cpu.init(stack, assembler.entrypoint(Cpu.GuestFunction, 0));
    const log = std.log.scoped(.host);

    const retval = blk: while (true) {
        cpu.run(0) catch |e| break :blk e;
        log.info("guest still running", .{});
    };
    log.info("child returned: {}", .{retval});
}

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    _ = x86_64.Assembler;
    _ = riscv64.Assembler;
}
