const std = @import("std");
const builtin = @import("builtin");

const Cpu = @import("./chip8.zig").Cpu;

const x86_64 = @import("./x86_64.zig");
const riscv = @import("./riscv.zig");
const chip8 = @import("./chip8.zig");

const Compiler = riscv.Compiler;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {
    const allocator = switch (builtin.mode) {
        .Debug => debug_allocator.allocator(),
        else => std.heap.smp_allocator,
    };
    defer if (builtin.mode == .Debug) std.debug.assert(debug_allocator.deinit() == .ok);

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len != 2) {
        std.log.err("usage: {s} <path to ROM>", .{args[0]});
        return error.BadUsage;
    }

    var write_buffer: [1024]u8 = undefined;
    var stdout_file = std.fs.File.stdout().writer(&write_buffer);
    const stdout = &stdout_file.interface;

    var program_buf: [4096 - 0x200]u8 = undefined;
    const program = try std.fs.cwd().readFile(args[1], &program_buf);

    const stack = try allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        16 << 10,
    );
    defer allocator.free(stack);

    var compiler: Compiler = undefined;
    compiler.init(allocator, if (builtin.cpu.arch.isRISCV())
        builtin.cpu.features
    else
        std.Target.riscv.featureSet(&.{ .@"64bit", .c }));
    defer compiler.deinit() catch unreachable;

    try compiler.prologue();
    var i: usize = 0;
    while (i < program.len) : (i += 2) {
        const ins: chip8.Instruction = @enumFromInt(std.mem.readInt(u16, program[i..][0..2], .big));
        if (ins.decode() == .invalid) break;
        try compiler.compile(ins);
    }
    try compiler.epilogue();

    if (comptime builtin.cpu.arch.isRISCV()) {
        try compiler.makeExecutable();
        const seed: u64 = @truncate(@as(u128, @bitCast(std.time.nanoTimestamp())));
        var cpu = Cpu.init(stack, compiler.entrypoint(), seed, {});
        @memcpy(cpu.context.memory[0x200..][0..program.len], program);

        while (true) {
            try cpu.run(100);
            // std.log.info("{any}", .{cpu.context.screen});
            // if (6 > 5) return;
            cpu.context.dt -|= 1;
            cpu.context.st -|= 1;
            // print screen
            try stdout.writeAll("\x1b[H\x1b[2J\x1b[3J");
            for (0..16) |line| {
                const y = 2 * line;
                for (0..64) |x| {
                    const lut = [_][]const u8{
                        " ",
                        "\u{2580}", // upper half block
                        "\u{2584}", // lower half block
                        "\u{2588}", // full block
                    };
                    const p1 = cpu.context.screen[(64 * y + x) / 8] >> @truncate(x % 8) & 1;
                    const p2 = cpu.context.screen[(64 * (y + 1) + x) / 8] >> @truncate(x % 8) & 1;
                    try stdout.writeAll(lut[2 * p2 + p1]);
                }
                try stdout.writeByte('\n');
            }
            try stdout.flush();
            std.Thread.sleep(std.time.ns_per_s / 60);
        }
    } else {
        const code = compiler.code_buffer.writable.list.items;
        std.log.info("generated code: {x}", .{code});
    }
}

comptime {
    if (builtin.is_test) {
        // _ = x86_64.Assembler;
        _ = riscv.Assembler;
        _ = riscv.Compiler;
    }
}
