const std = @import("std");

const Assembler = @This();

const riscv64 = @import("../riscv64.zig");
const Register = riscv64.Register;
const Instruction = riscv64.Instruction;

const GenericAssembler = @import("../Assembler.zig");

const assert = std.debug.assert;

inner: GenericAssembler,

pub fn init(allocator: std.mem.Allocator) Assembler {
    return .{ .inner = GenericAssembler.init(allocator) };
}

pub fn deinit(self: *Assembler) void {
    self.inner.deinit();
}

pub fn makeExecutable(self: *Assembler) !void {
    // TODO compressed ebreak
    const ebreak_bin: u16 = 0x9002;
    try self.inner.makeExecutable(std.mem.asBytes(&std.mem.nativeToLittle(u16, ebreak_bin)));
}

fn emit(self: *Assembler, instruction: anytype) !void {
    if (@TypeOf(instruction) != Instruction and @TypeOf(instruction) != Instruction.Compressed) {
        @compileError("invalid type passed into emit(): " ++ @typeName(@TypeOf(instruction)));
    }
    try instruction.any().writeTo(self.inner.writer());
}

pub fn ebreak(self: *Assembler) !void {
    try self.emit(Instruction{ .i = .{
        .opcode = .system,
        .rd = .zero,
        .funct3 = 0,
        .rs1 = .zero,
        .imm = 1,
    } });
}

fn c_addi(self: *Assembler, rd: Register.NonZero, nzimm: i6) !void {
    assert(nzimm != 0);
    const u_nzimm: u6 = @bitCast(nzimm);
    try self.emit(Instruction.Compressed{ .ci = .{
        .op = 0b01,
        .imm_1 = @truncate(u_nzimm),
        .rd_rs1 = rd,
        .imm_2 = @truncate(u_nzimm >> 5),
        .funct3 = 0b000,
    } });
}

fn c_addiw(self: *Assembler, rd: Register.NonZero, nzimm: i6) !void {
    assert(nzimm != 0);
    const u_nzimm: u6 = @bitCast(nzimm);
    try self.emit(Instruction.Compressed{ .ci = .{
        .op = 0b01,
        .imm_1 = @truncate(u_nzimm),
        .rd_rs1 = rd,
        .imm_2 = @truncate(u_nzimm >> 5),
        .funct3 = 0b001,
    } });
}

fn c_addi16sp(self: *Assembler, nzimm: i6) !void {
    assert(nzimm != 0);

    const u_nzimm: u6 = @bitCast(nzimm);
    const wide_nzimm = @as(u10, u_nzimm) << 4;
    const bits: @Vector(10, u1) = @bitCast(wide_nzimm);

    try self.emit(Instruction.Compressed{ .ci = .{
        .op = 0b01,
        .imm_1 = @bitCast(@shuffle(u1, bits, undefined, [_]i32{ 5, 7, 8, 6, 4 })),
        .rd_rs1 = .sp,
        .imm_2 = @truncate(wide_nzimm >> 9),
        .funct3 = 0b011,
    } });
}

pub fn addi(self: *Assembler, rd: Register, rs1: Register, value: i12) !void {
    if (rs1 == .zero) {
        if (rd.nonZero()) |nz_rd| {
            if (std.math.cast(i6, value)) |compressed_immediate| {
                return self.c_li(nz_rd, compressed_immediate);
            }
        }
    } else if (rd == .sp and rs1 == .sp and @rem(value, 16) == 0 and value != 0) {
        const truncated = @divExact(value, 16);
        if (std.math.cast(i6, truncated)) |compressed_immediate| {
            return self.c_addi16sp(compressed_immediate);
        }
    }
    try self.emit(Instruction{ .i = .{
        .opcode = .op_imm,
        .funct3 = 0b000,
        .rd = rd,
        .rs1 = rs1,
        .imm = @bitCast(value),
    } });
}

pub fn addiw(self: *Assembler, rd: Register, rs1: Register, value: i12) !void {
    try self.emit(Instruction{ .i = .{
        .opcode = .op_imm_32,
        .funct3 = 0b000,
        .rd = rd,
        .rs1 = rs1,
        .imm = @bitCast(value),
    } });
}

pub fn lui(self: *Assembler, rd: Register, value: u20) !void {
    try self.emit(Instruction{ .u = .{
        .opcode = .lui,
        .rd = rd,
        .imm_31_12 = value,
    } });
}

pub fn slli(self: *Assembler, rd: Register, rs1: Register, shamt: u6) !void {
    try self.emit(Instruction{ .i = .{
        .opcode = .op_imm,
        .funct3 = 0b001,
        .rd = rd,
        .rs1 = rs1,
        .imm = shamt,
    } });
}

fn li32(self: *Assembler, rd: Register, value: i32) !void {
    if (std.math.cast(i12, value)) |lower_immediate| {
        return self.addi(rd, .zero, lower_immediate);
    } else {
        const lower: i12 = @truncate(value);
        var upper: u20 = @truncate(@as(u32, @bitCast(value)) >> 12);
        // if the low bits are negative (when interpreted as a signed integer), we need to increase
        // the upper bits so they get decremented by the lower bits
        if (lower < 0) {
            upper += 1;
        }
        try self.lui(rd, upper);
        if (lower != 0) {
            // we need addiw here, not addi
            // if upper was 0x7ffff but we incremented it to account for a negative lower, then
            // upper is 0x80000 but it got sign-extended into bits 63:32. this means our overall
            // result will be negative even though value was positive.
            // addiw solves this, because it will truncate the result (0x7ffff | lower) to 32 bits
            // before sign-extending back to 64. the truncated result does not have bit 31 set so it
            // is positive.
            try self.addiw(rd, rd, lower);
        }
    }
}

fn c_li(self: *Assembler, rd: Register.NonZero, imm: i6) !void {
    const u_imm: u6 = @bitCast(imm);
    try self.emit(Instruction.Compressed{ .ci = .{
        .op = 0b01,
        .imm_1 = @truncate(u_imm),
        .rd_rs1 = Register.from(rd),
        .imm_2 = @truncate(u_imm >> 5),
        .funct3 = 0b010,
    } });
}

pub fn li(self: *Assembler, rd: Register, value: i64) !void {
    // TODO support more complex cases
    if (std.math.cast(i32, value)) |word_immediate| {
        return self.li32(rd, word_immediate);
    } else if (std.math.cast(i44, value)) |word_and_shift| {
        var upper: i32 = @truncate(word_and_shift >> 12);
        const lower: i12 = @truncate(word_and_shift);

        if (lower < 0 and upper == std.math.maxInt(i32)) {
            // load 0x80000000 (2^31) without sign extension
            try self.addi(rd, .zero, 1);
            try self.slli(rd, rd, 31);
        } else {
            if (lower < 0) {
                upper += 1;
            }
            try self.li32(rd, upper);
        }
        try self.slli(rd, rd, 12);
        try self.addi(rd, rd, lower);
    } else std.debug.panic("{} too big", .{value});
}

fn c_jr(self: *Assembler, rs1: Register.NonZero) !void {
    try self.emit(Instruction.Compressed{ .cr = .{
        .op = 0b10,
        .rs2 = .zero,
        .rd_rs1 = Register.from(rs1),
        .funct4 = 0b1000,
    } });
}

fn c_jalr(self: *Assembler, rs1: Register.NonZero) !void {
    try self.emit(Instruction.Compressed{ .cr = .{
        .op = 0b10,
        .rs2 = .zero,
        .rd_rs1 = Register.from(rs1),
        .funct4 = 0b1001,
    } });
}

pub fn jalr(self: *Assembler, rd: Register, rs1: Register, offset: i12) !void {
    if (offset == 0) {
        if (rs1.nonZero()) |nz_rs1| {
            if (rd == .zero) {
                return self.c_jr(nz_rs1);
            } else if (rd == .ra) {
                return self.c_jalr(nz_rs1);
            }
        }
    }

    try self.emit(Instruction{ .i = .{
        .opcode = .jalr,
        .funct3 = 0b000,
        .rd = rd,
        .rs1 = rs1,
        .imm = @bitCast(offset),
    } });
}

pub fn jr(self: *Assembler, rs1: Register, offset: i12) !void {
    try self.jalr(.zero, rs1, offset);
}

pub fn ret(self: *Assembler) !void {
    try self.jr(.ra, 0);
}

const LoadStoreSize = enum(u3) {
    byte = 0b000,
    halfword = 0b001,
    word = 0b010,
    byte_unsigned = 0b100,
    halfword_unsigned = 0b101,
    word_unsigned = 0b110,
    doubleword = 0b011,
};

fn load(self: *Assembler, size: LoadStoreSize, dst: Register, offset: i12, base: Register) !void {
    try self.emit(Instruction{ .i = .{
        .opcode = .load,
        .funct3 = @intFromEnum(size),
        .rd = dst,
        .rs1 = base,
        .imm = @bitCast(offset),
    } });
}

fn store(self: *Assembler, size: LoadStoreSize, src: Register, offset: i12, base: Register) !void {
    try self.emit(Instruction.makeS(
        .store,
        @intFromEnum(size),
        base,
        src,
        @bitCast(offset),
    ));
}

pub fn ld(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    return self.load(.doubleword, dst, offset, base);
}

pub fn sd(self: *Assembler, src: Register, offset: i12, base: Register) !void {
    return self.store(.doubleword, src, offset, base);
}

test "li" {
    if (@import("builtin").cpu.arch != .riscv64) return error.SkipZigTest;

    const immediates = [_]i64{
        0,
        1,
        -1,
        // i6 boundaries, boundaries + 1
        -32,
        31,
        -33,
        32,
        // i12 boundaries
        -0x800,
        -0x801,
        0x7ff,
        0x800,
        // i32 boundaries
        -0x80000000,
        -0x80000001,
        0x80000000,
        0x7fffffff,
        // i44 boundaries
        0x7ffffffffff,
        -0x80000000000,
    };

    for (immediates) |i| {
        var assembler = Assembler.init(std.testing.allocator);
        defer assembler.deinit();
        try assembler.li(.a0, i);
        try assembler.ret();
        try assembler.makeExecutable();
        const code: *const fn () callconv(.C) i64 = @ptrCast(assembler.inner.code.items.ptr);
        try std.testing.expectEqual(i, code());
    }
}
