const std = @import("std");
const builtin = @import("builtin");

const Cpu = @import("./chip8.zig").Cpu;

const x86_64 = @import("./x86_64.zig");
const riscv64 = @import("./riscv64.zig");

const Compiler = @import("./riscv64.zig").Compiler;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {
    const allocator = switch (builtin.mode) {
        .Debug => debug_allocator.allocator(),
        else => std.heap.smp_allocator,
    };
    defer if (builtin.mode == .Debug) std.debug.assert(debug_allocator.deinit() == .ok);
    const stack = try allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        16 << 10,
    );
    defer allocator.free(stack);

    var compiler: Compiler = undefined;
    try compiler.init(allocator, if (builtin.cpu.arch.isRISCV())
        builtin.cpu.features
    else
        std.Target.riscv.featureSet(&.{ .@"64bit", .c }));
    defer {
        compiler.deinit() catch unreachable;
    }

    try compiler.prologue();
    for ([_]u16{
        // 0xa800,
        // 0xff55,
    }) |ins| {
        try compiler.compile(@enumFromInt(ins));
    }
    try compiler.epilogue();

    if (comptime builtin.cpu.arch == .riscv64) {
        try compiler.makeExecutable();
        const seed: u64 = s: {
            const time_unsigned: u128 = @bitCast(std.time.nanoTimestamp());
            break :s @truncate(time_unsigned);
        };

        var cpu = Cpu.init(stack, compiler.entrypoint(), seed);
        const log = std.log.scoped(.host);

        const retval = retval: while (true) {
            cpu.run(1) catch |e| break :retval e;
            log.info("guest still running", .{});
        };
        log.info("child returned: {}", .{retval});
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
