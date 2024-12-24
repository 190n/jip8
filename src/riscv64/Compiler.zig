const std = @import("std");

const riscv64 = @import("../riscv64.zig");
const Assembler = riscv64.Assembler;
const GenericAssembler = @import("../Assembler.zig");
const chip8 = @import("../chip8.zig");
const Compiler = @This();

assembler: Assembler,
allocator: std.mem.Allocator,
entry_offset: usize,
yield_offset: usize,
log_offset: usize,

fn log(a: u32) callconv(.c) void {
    std.log.scoped(.runtime).info("a = {}", .{a});
}

pub fn init(allocator: std.mem.Allocator, feature_set: std.Target.Cpu.Feature.Set) !Compiler {
    var assembler = Assembler.init(allocator, feature_set);
    errdefer assembler.deinit();

    var offsets: struct { yield_offset: usize, log_offset: usize } = undefined;

    inline for (.{
        .{ @intFromPtr(&chip8.Cpu.Context.yield), "yield_offset" },
        .{ @intFromPtr(&log), "log_offset" },
    }) |pair| {
        const address, const field_name = pair;
        @field(offsets, field_name) = assembler.code.dynamic.code.items.len;
        try assembler.auipc(.t0, 0);
        // placeholder offset
        try assembler.ld(.t0, 0, .t0);
        try assembler.jr(.t0, 0);

        while (@rem(assembler.code.dynamic.code.items.len, 8) != 0) {
            try assembler.ebreak();
        }

        const offset = assembler.code.dynamic.code.items.len - @field(offsets, field_name);
        {
            var offset_assembler = assembler.atOffset(@field(offsets, field_name) + 4);
            try offset_assembler.ld(.t0, @intCast(offset), .t0);
        }

        try assembler.insertBytes(std.mem.asBytes(&address));
    }

    return .{
        .assembler = assembler,
        .allocator = allocator,
        .entry_offset = assembler.code.dynamic.code.items.len,
        .yield_offset = offsets.yield_offset,
        .log_offset = offsets.log_offset,
    };
}

pub fn prologue(self: *Compiler) !void {
    try self.assembler.addi(.sp, .sp, -32);
    try self.assembler.sd(.ra, 0, .sp);
    try self.assembler.sd(.s0, 8, .sp);
    try self.assembler.sd(.s1, 16, .sp);
    try self.assembler.addi(.s1, .a0, 0);
}

pub fn genSomeCode(self: *Compiler) !void {
    for (0..3) |_| {
        try self.assembler.addi(.s0, .s0, 1);
        try self.assembler.addi(.a0, .s0, 0);
        try self.assembler.jal(.ra, @intCast(@as(isize, @intCast(self.log_offset)) - @as(isize, @intCast(self.assembler.code.dynamic.code.items.len))));
        try self.assembler.addi(.a0, .s1, 0);
        try self.assembler.jal(.ra, @intCast(@as(isize, @intCast(self.yield_offset)) - @as(isize, @intCast(self.assembler.code.dynamic.code.items.len))));
    }
}

pub fn epilogue(self: *Compiler) !void {
    try self.assembler.ld(.ra, 0, .sp);
    try self.assembler.ld(.s0, 8, .sp);
    try self.assembler.ld(.s1, 16, .sp);
    try self.assembler.addi(.sp, .sp, 32);
    try self.assembler.li(.a0, @intFromError(error.HelloRiscv64));
    try self.assembler.ret();
}

pub fn makeExecutable(self: *Compiler) !void {
    return self.assembler.makeExecutable();
}

pub fn entrypoint(self: *const Compiler) chip8.Cpu.GuestFunction {
    return self.assembler.entrypoint(chip8.Cpu.GuestFunction, self.entry_offset);
}

pub fn deinit(self: *Compiler) void {
    self.assembler.deinit();
}
