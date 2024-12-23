const std = @import("std");
const assert = std.debug.assert;

const Assembler = @This();

const x86_64 = @import("../x86_64.zig");
const Register = x86_64.Register;
const Rex = x86_64.Rex;
const ModRM = x86_64.ModRM;
const Opcode = x86_64.Opcode;

const GenericAssembler = @import("../Assembler.zig");

code: union(enum) {
    dynamic: GenericAssembler,
    fixed: std.io.FixedBufferStream([]u8),
},

pub fn init(allocator: std.mem.Allocator) Assembler {
    return .{ .code = .{ .dynamic = GenericAssembler.init(allocator) } };
}

pub fn deinit(self: *Assembler) void {
    switch (self.code) {
        .dynamic => |*d| d.deinit(),
        else => {},
    }
}

pub fn makeExecutable(self: *Assembler) !void {
    try self.code.dynamic.makeExecutable(&.{@intFromEnum(Opcode.int3)});
}

pub fn entrypoint(self: *const Assembler, comptime T: type, offset: usize) T {
    return self.code.dynamic.entrypoint(T, offset);
}

pub fn atOffset(self: *Assembler, index: usize) Assembler {
    return .{ .code = .{ .fixed = std.io.fixedBufferStream(self.code.dynamic.code.items[index..]) } };
}

/// Returns all the code added to this Assembler since its creation as a slice
pub fn slice(self: *const Assembler) []const u8 {
    return switch (self.code) {
        .dynamic => |d| d.code.items,
        .fixed => |f| f.buffer[0..f.pos],
    };
}

fn writeInt(self: *Assembler, comptime T: type, value: T) !void {
    switch (self.code) {
        inline else => |*c| try c.writer().writeInt(T, value, .little),
    }
}

fn binOpRegReg(
    self: *Assembler,
    opcode: Opcode,
    dst: Register,
    src: Register,
) !void {
    if (dst.region().bits() != src.region().bits()) {
        return error.WidthMismatch;
    }

    const dst_rex = dst.rex();
    const src_rex = src.rex();
    if (dst_rex != null and src_rex != null and dst_rex != src_rex) {
        return error.IncompatibleArguments;
    }

    if (dst.region() == .word) {
        try self.writeInt(u8, 0x66);
    }

    const dst_ex = dst.isExtendedHalf();
    const src_ex = src.isExtendedHalf();
    if (dst_rex == .mandatory or src_rex == .mandatory) {
        try self.writeInt(u8, @bitCast(Rex{
            .w = dst.region() == .qword,
            .b = dst_ex,
            .r = src_ex,
        }));
    }
    try self.writeInt(u8, @intFromEnum(opcode));
    try self.writeInt(u8, @bitCast(ModRM.register(dst, src)));
}

/// value must fit in dst
fn binOpRegInOpcodeImm(
    self: *Assembler,
    opcode: Opcode,
    dst: Register,
    value: i64,
) !void {
    if (dst.region() == .word) {
        try self.writeInt(u8, 0x66);
    }
    if (dst.rex() == .mandatory) {
        try self.writeInt(u8, @bitCast(Rex{
            .w = dst.region() == .qword,
            .b = dst.isExtendedHalf(),
        }));
    }
    try self.writeInt(u8, @intFromEnum(opcode.plusRegister(dst)));
    switch (dst.region()) {
        inline else => |r| try self.writeInt(
            std.meta.Int(.signed, r.bits()),
            @intCast(value),
        ),
    }
}

pub fn movRegReg(self: *Assembler, dst: Register, src: Register) !void {
    return self.binOpRegReg(
        if (dst.region().bits() == 8) .mov_rm8_r8 else .mov_rm_r,
        dst,
        src,
    );
}

pub fn movRegImm(self: *Assembler, dst: Register, value: i64) !void {
    if (dst.region() == .qword) {
        if (std.math.cast(i32, value)) |dword| {
            // we can use a shorter sign-extending instruction if we are moving 32 bits into a 64
            // bit register
            try self.writeInt(u8, @bitCast(Rex{
                .w = true,
                .b = dst.isExtendedHalf(),
            }));
            try self.writeInt(u8, @intFromEnum(Opcode.mov_r_imm32));
            try self.writeInt(u8, @bitCast(ModRM.register(dst, Register.numbered32(0))));
            try self.writeInt(i32, dword);
            return;
        }
    }
    return self.binOpRegInOpcodeImm(
        switch (dst.region()) {
            .byte_h, .byte_l => .mov_r8_imm8,
            .word, .dword, .qword => .mov_r_imm,
        },
        dst,
        value,
    );
}

pub fn ret(self: *Assembler) !void {
    try self.byteInstruction(.ret);
}

fn byteInstruction(self: *Assembler, opcode: Opcode) !void {
    try self.writeInt(u8, @intFromEnum(opcode));
}

fn byteInstructionRegInOpcode(self: *Assembler, opcode: Opcode, register: Register) !void {
    try self.writeInt(u8, @intFromEnum(opcode.plusRegister(register)));
}

pub fn push(self: *Assembler, register: Register) !void {
    switch (register.region()) {
        .byte_l, .byte_h, .dword => return error.InvalidOperand,
        else => |r| {
            if (r == .word) try self.writeInt(u8, 0x66);
            if (register.isExtendedHalf()) {
                try self.writeInt(u8, @bitCast(Rex{ .b = true }));
            }
            try self.writeInt(u8, @intFromEnum(Opcode.push.plusRegister(register)));
        },
    }
}

pub fn pop(self: *Assembler, register: Register) !void {
    switch (register.region()) {
        .byte_l, .byte_h, .dword => return error.InvalidOperand,
        else => |r| {
            if (r == .word) try self.writeInt(u8, 0x66);
            if (register.isExtendedHalf()) {
                try self.writeInt(u8, @bitCast(Rex{ .b = true }));
            }
            try self.writeInt(u8, @intFromEnum(Opcode.pop.plusRegister(register)));
        },
    }
}

/// offset relative to the next instruction
pub fn callRelative(self: *Assembler, offset: i32) !void {
    try self.writeInt(u8, @intFromEnum(Opcode.call));
    try self.writeInt(i32, offset);
}

pub fn callIndirect(self: *Assembler, register: Register) !void {
    if (register.region() != .qword) return error.InvalidOperand;
    if (register.isExtendedHalf()) {
        try self.writeInt(u8, @bitCast(Rex{ .b = true }));
    }
    try self.writeInt(u8, 0xff);
    try self.writeInt(u8, @bitCast(ModRM{
        .mod = .register,
        .reg = 2,
        .rm = register.num(),
    }));
}

/// Compare machine code generated by clang with our assembler. If clang exits with code zero, our
/// assembler must produce the same machine code; otherwise, our assembler must produce some error.
///
/// dir:           temporary directory to write files into
/// expected_fmt:  format string for the (Intel syntax) assembly code to use as input to clang
/// expected_args: arguments for expected_fmt
/// actual_func:   function to use to generate code, taking *Assembler as its first argument
/// func_args:     arguments to pass to actual_func, excluding the assembler pointer
fn testResultMatches(
    dir: std.fs.Dir,
    comptime expected_fmt: []const u8,
    expected_args: anytype,
    comptime actual_func: anytype,
    func_args: anytype,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var assembler = Assembler.init(allocator);
    defer assembler.deinit();

    var asm_file = try dir.createFile("code.S", .{});
    defer asm_file.close();
    try asm_file.writeAll(".intel_syntax\n");
    try asm_file.writer().print(expected_fmt, expected_args);

    const clang_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "zig", "cc", "-target", "x86_64-linux-gnu", "-c", "code.S" },
        .cwd_dir = dir,
    });

    if (clang_result.term.Exited == 0) {
        // our assembler should succeed
        const objcopy_result = try std.process.Child.run(.{
            .allocator = allocator,
            .argv = &.{ "zig", "objcopy", "-O", "binary", "code.o", "code.bin" },
            .cwd_dir = dir,
        });
        try std.testing.expectEqual(0, objcopy_result.term.Exited);

        try @call(.auto, actual_func, .{&assembler} ++ func_args);

        var bin_file = try dir.openFile("code.bin", .{});
        defer bin_file.close();
        var read_buf: [64]u8 = undefined;
        const size = try bin_file.readAll(&read_buf);

        try std.testing.expectEqualSlices(u8, read_buf[0..size], assembler.slice());
    } else {
        // our assembler should fail
        const did_error = if (@call(.auto, actual_func, .{&assembler} ++ func_args))
            false
        else |_|
            true;
        try std.testing.expect(did_error);
    }
}

test "movRegReg" {
    if (@import("builtin").cpu.arch != .x86_64) return error.SkipZigTest;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const regs = [_]Register{ .al, .ah, .ax, .eax, .rax, .spl, .r8b, .r8w, .r8d, .r8 };

    for (regs) |r1| {
        for (regs) |r2| {
            try testResultMatches(
                tmp_dir.dir,
                "mov {s}, {s}",
                .{ @tagName(r1), @tagName(r2) },
                movRegReg,
                .{ r1, r2 },
            );
        }
    }
}

test "movRegImm" {
    if (@import("builtin").cpu.arch != .x86_64) return error.SkipZigTest;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const regs = [_]Register{ .al, .ah, .ax, .eax, .rax, .spl, .r8b, .r8w, .r8d, .r8 };
    const immediates = [_]i64{ 0x01, 0x0123, 0x01234567, 0x0123456789abcdef };
    for (regs) |r| {
        for (immediates) |i| {
            const bits = r.region().bits();
            if (@clz(i) < 64 - bits) {
                continue;
            }

            try testResultMatches(
                tmp_dir.dir,
                "mov {s}, 0x{x}",
                .{ @tagName(r), i },
                movRegImm,
                .{ r, i },
            );
        }
    }
}
