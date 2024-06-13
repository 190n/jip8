const std = @import("std");
const builtin = @import("builtin");

const Cpu = @import("./chip8.zig").Cpu;

const x86_64 = @import("./x86_64.zig");
const riscv64 = @import("./riscv64.zig");

noinline fn guestCallee(comptime log: type, ctx: *Cpu.Context) void {
    log.info("=> inside guest callee", .{});
    ctx.yield();
    log.info("=> after yield", .{});
}

fn guestFn(ctx: *Cpu.Context) callconv(.C) u16 {
    const log = std.log.scoped(.guest);
    log.info("inside guest code", .{});
    guestCallee(log, ctx);
    ctx.yield();
    log.info("after second yield, in caller", .{});
    return @intFromError(error.Meow);
}

pub fn main() !void {
    const stack = try std.heap.page_allocator.alignedAlloc(u8, std.mem.page_size, 4 * std.mem.page_size);
    defer std.heap.page_allocator.free(stack);

    var assembler = switch (builtin.cpu.arch) {
        .x86_64 => x86_64.Assembler.init(std.heap.page_allocator),
        .riscv64 => riscv64.Assembler.init(std.heap.page_allocator),
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

    var cpu = Cpu.init(stack, @ptrCast(assembler.inner.code.items.ptr));
    const log = std.log.scoped(.host);

    const retval = blk: while (true) {
        cpu.run(5) catch |e| break :blk e;
        log.info("guest still running", .{});
    };
    log.info("child returned: {}", .{retval});
}

pub const std_options = std.Options{
    .log_level = .info,
};

comptime {
    std.testing.refAllDeclsRecursive(@This());
    std.testing.refAllDeclsRecursive(@import("./x86_64.zig"));
}
