const std = @import("std");
const builtin = @import("builtin");

const Cpu = @import("./chip8.zig").Cpu;

const x86_64 = @import("./x86_64.zig");
const riscv64 = @import("./riscv64.zig");

const Assembler = @import("./Assembler.zig");
const Compiler = @import("./Compiler.zig").Compiler;

fn meow(x: u32) callconv(.c) void {
    std.log.info("meow: \"{s}\"", .{std.mem.asBytes(&x)});
}

pub fn main() !void {
    const stack = try std.heap.page_allocator.alignedAlloc(u8, std.mem.page_size, 4 * std.mem.page_size);
    defer std.heap.page_allocator.free(stack);

    var compiler = try Compiler(.riscv64).init(std.heap.page_allocator, if (builtin.cpu.arch.isRISCV())
        builtin.cpu.features
    else
        std.Target.riscv.featureSet(&.{ .@"64bit", .c }));
    defer compiler.deinit();

    try compiler.prologue();
    for ([_]u16{
        0x607b, // v0 := 123
        0x8100, // v1 := v0
        0x7105, // v1 += 5
        0x1206,
    }) |ins| {
        try compiler.compile(@enumFromInt(ins));
    }
    try compiler.debug();
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
    } else {
        const code = compiler.assembler.slice();
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
