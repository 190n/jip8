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

fn meow(x: u32) callconv(.C) void {
    std.log.info("meow: \"{s}\"", .{std.mem.asBytes(&x)});
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
            // stack alignment
            try assembler.push(.rax);

            try assembler.movRegImm(.rax, @intCast(@intFromPtr(&Cpu.Context.yield)));
            try assembler.call(.rax);

            try assembler.movRegImm(.rdi, 0x776f656d);
            try assembler.movRegImm(.rax, @intCast(@intFromPtr(&meow)));
            try assembler.call(.rax);

            try assembler.pop(.rax);
            try assembler.movRegImm(.ax, @intFromError(error.HelloX86_64));
            try assembler.ret();
        },
        .riscv64 => {
            try assembler.addi(.sp, .sp, -16);
            try assembler.sd(.ra, 0, .sp);
            try assembler.li(.a1, @intCast(@intFromPtr(&Cpu.Context.yield)));
            try assembler.jalr(.ra, .a1, 0);

            try assembler.li(.a0, 0x776f656d);
            try assembler.li(.a1, @intCast(@intFromPtr(&meow)));
            try assembler.jalr(.ra, .a1, 0);

            try assembler.li(.a0, @intFromError(error.HelloRiscv64));

            try assembler.ld(.ra, 0, .sp);
            try assembler.addi(.sp, .sp, 16);
            try assembler.ret();
        },
        else => unreachable,
    }
    try assembler.makeExecutable();

    var cpu = Cpu.init(stack, @ptrCast(assembler.inner.code.items.ptr));
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

comptime {
    std.testing.refAllDeclsRecursive(@This());
    std.testing.refAllDeclsRecursive(@import("./x86_64.zig"));
}
