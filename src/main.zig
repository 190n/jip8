const std = @import("std");
const builtin = @import("builtin");

const Cpu = @import("./chip8.zig").Cpu;

const x86_64 = @import("./x86_64.zig");
const riscv64 = @import("./riscv64.zig");

const Compiler = @import("./Compiler.zig").Compiler;

fn meow(x: u32) callconv(.c) void {
    std.log.info("meow: \"{s}\"", .{std.mem.asBytes(&x)});
}

pub fn main() !void {
    const stack = try std.heap.page_allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        16 << 10,
    );
    defer std.heap.page_allocator.free(stack);

    var compiler: Compiler(.riscv64) = undefined;
    try compiler.init(std.heap.page_allocator, if (builtin.cpu.arch.isRISCV())
        builtin.cpu.features
    else
        std.Target.riscv.featureSet(&.{ .@"64bit", .c }));
    defer {
        compiler.deinit() catch unreachable;
    }

    try compiler.prologue();
    for ([_]u16{
        // set V0-VF to 0x00, 0x11, ..., 0xFF
        0x6000,
        0x6111,
        0x6222,
        0x6333,
        0x6444,
        0x6555,
        0x6666,
        0x6777,
        0x6888,
        0x6999,
        0x6aaa,
        0x6bbb,
        0x6ccc,
        0x6ddd,
        0x6eee,
        0x6fff,
        // I = 0x400
        0xa400,
        // store all registers
        0xff55,
        // basically a nop so we can see what I was set to after
        0x7000,
    }) |ins| {
        try compiler.compile(@enumFromInt(ins));
    }
    try compiler.epilogue();

    if (comptime builtin.cpu.arch == .riscv64) {
        try compiler.makeExecutable();

        var cpu = Cpu.init(stack, compiler.entrypoint());
        const log = std.log.scoped(.host);

        const retval = retval: while (true) {
            cpu.run(0) catch |e| break :retval e;
            log.info("guest still running", .{});
        };
        log.info("child returned: {}", .{retval});
        for (0x400..0x410) |i| {
            log.info("memory[{x}] = {x}", .{ i, cpu.context.memory[i] });
        }
    } else {
        const code = compiler.code_buffer.writable.list.items;
        std.log.info("generated code: {x}", .{code});
    }
}

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    _ = x86_64.Assembler;
    _ = riscv64.Assembler;
}
