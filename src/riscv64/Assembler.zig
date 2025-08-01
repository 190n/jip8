const std = @import("std");
const builtin = @import("builtin");

const Assembler = @This();

const riscv64 = @import("../riscv64.zig");
const Register = riscv64.Register;
const Instruction = riscv64.Instruction;

const code_buffer = @import("../code_buffer.zig");

const assert = std.debug.assert;

/// RISC-V ISA extensions that are relevant to this assembler
pub const Features = packed struct {
    /// C (compressed instruction) extension, minus floating-point loads and stores
    zca: bool,

    pub fn from(set: std.Target.Cpu.Feature.Set) Features {
        return .{
            .zca = std.Target.riscv.featureSetHasAny(set, [_]std.Target.riscv.Feature{ .c, .zca }),
        };
    }
};

/// TODO: delete
writer: *std.io.Writer,
features: Features,

pub fn init(writer: *std.io.Writer, feature_set: std.Target.Cpu.Feature.Set) Assembler {
    return .{
        .writer = writer,
        .features = .from(feature_set),
    };
}

pub fn hasCompressed(self: *const Assembler) bool {
    return self.features.zca;
}

/// instructions: array of tuples containing an enum literal specifying an Assembler function to
/// call, and tuples specifying the arguments
/// e.g. .{ .{ .addi, .{ .a0, .a0, 5 } } }
pub fn assemble(comptime instructions: anytype) []const u8 {
    // no comptime allocator :(
    var code: []const u8 = &.{};
    for (instructions) |i| {
        const name, const args = i;
        var single_instruction_buf: [4]u8 = undefined;
        var writer: std.io.Writer = .fixed(&single_instruction_buf);
        var assembler: Assembler = .init(&writer, .{ .zca = true });
        @call(.auto, @field(Assembler, @tagName(name)), .{&assembler} ++ args) catch unreachable;
        code = code ++ assembler.slice();
    }
    return code;
}

fn permute(n: anytype, comptime order: []const i32) @Type(.{ .int = .{
    .signedness = .unsigned,
    .bits = order.len,
} }) {
    comptime for (order) |x| {
        assert(x >= 0);
    };

    const T = @TypeOf(n);
    const bits: @Vector(@typeInfo(T).int.bits, u1) = @bitCast(n);
    const shuffled = @shuffle(u1, bits, undefined, order[0..].*);
    return @bitCast(shuffled);
}

fn emit(self: *Assembler, instruction: anytype) !void {
    if (@TypeOf(instruction) != Instruction and
        @TypeOf(instruction) != Instruction.Compressed)
    {
        @compileError("invalid type passed into emit(): " ++
            @typeName(@TypeOf(instruction)));
    }
    if (@TypeOf(instruction) == Instruction.Compressed) {
        assert(self.hasCompressed());
    }
    try instruction.any().writeTo(self.writer);
}

/// Debug breakpoint
pub fn ebreak(self: *Assembler) !void {
    if (self.hasCompressed()) {
        try self.emit(Instruction.Compressed{ .cr = .{
            .funct4 = 0b1001,
            .rd_rs1 = .zero,
            .rs2 = .zero,
            .op = 0b10,
        } });
    } else {
        try self.emit(Instruction{ .i = .{
            .opcode = .system,
            .rd = .zero,
            .funct3 = 0,
            .rs1 = .zero,
            .imm = 1,
        } });
    }
}

/// Compressed immediate add
/// rd += nzimm
/// nzimm != 0
fn c_addi(self: *Assembler, rd: Register.NonZero, nzimm: i6) !void {
    assert(self.hasCompressed());
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

/// Compressed 32-bit immediate add
/// rd += nzimm, truncated to 32 bits, then sign-extended to 64
fn c_addiw(self: *Assembler, rd: Register.NonZero, nzimm: i6) !void {
    assert(self.hasCompressed());
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

fn c_addi16sp(self: *Assembler, nzimm: i10) !void {
    assert(self.hasCompressed());
    assert(nzimm != 0);
    assert(@rem(nzimm, 16) == 0);

    try self.emit(Instruction.Compressed{ .ci = .{
        .op = 0b01,
        .imm_1 = permute(nzimm, &.{ 5, 7, 8, 6, 4 }),
        .rd_rs1 = .sp,
        .imm_2 = permute(nzimm, &.{9}),
        .funct3 = 0b011,
    } });
}

pub fn addi(self: *Assembler, rd: Register, rs1: Register, value: i12) !void {
    if (self.hasCompressed()) {
        if (rs1 == .zero) {
            if (rd.nonZero()) |nz_rd| {
                if (std.math.cast(i6, value)) |compressed_immediate| {
                    return self.c_li(nz_rd, compressed_immediate);
                }
            }
        } else if (rd == .sp and rs1 == .sp and @rem(value, 16) == 0 and value != 0) {
            if (std.math.cast(i10, value)) |compressed_immediate| {
                return self.c_addi16sp(compressed_immediate);
            }
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

pub fn andi(self: *Assembler, rd: Register, rs1: Register, value: i12) !void {
    try self.emit(Instruction{ .i = .{
        .opcode = .op_imm,
        .funct3 = 0b111,
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

pub fn li(self: *Assembler, rd: Register, value: i32) !void {
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

pub fn add(self: *Assembler, rd: Register, rs1: Register, rs2: Register) !void {
    // TODO try c.add
    try self.emit(Instruction{ .r = .{
        .opcode = .op,
        .funct3 = 0b000,
        .rd = rd,
        .rs1 = rs1,
        .rs2 = rs2,
        .funct7 = 0b0000000,
    } });
}

fn c_li(self: *Assembler, rd: Register.NonZero, imm: i6) !void {
    assert(self.hasCompressed());
    const u_imm: u6 = @bitCast(imm);
    try self.emit(Instruction.Compressed{ .ci = .{
        .op = 0b01,
        .imm_1 = @truncate(u_imm),
        .rd_rs1 = Register.from(rd),
        .imm_2 = @truncate(u_imm >> 5),
        .funct3 = 0b010,
    } });
}

fn c_jr(self: *Assembler, rs1: Register.NonZero) !void {
    assert(self.hasCompressed());
    try self.emit(Instruction.Compressed{ .cr = .{
        .op = 0b10,
        .rs2 = .zero,
        .rd_rs1 = Register.from(rs1),
        .funct4 = 0b1000,
    } });
}

fn c_jalr(self: *Assembler, rs1: Register.NonZero) !void {
    assert(self.hasCompressed());
    try self.emit(Instruction.Compressed{ .cr = .{
        .op = 0b10,
        .rs2 = .zero,
        .rd_rs1 = Register.from(rs1),
        .funct4 = 0b1001,
    } });
}

pub fn jalr(self: *Assembler, rd: Register, rs1: Register, offset: i12) !void {
    if (self.hasCompressed() and offset == 0) {
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

pub fn jal(self: *Assembler, rd: Register, offset: i21) !void {
    // TODO compressed
    assert(@rem(offset, 2) == 0);
    try self.emit(Instruction.makeJ(.jal, rd, @bitCast(offset)));
}

/// Shift imm left by 12, sign-extend to 64 bits, add to the address of the auipc instruction, and
/// store in rd
pub fn auipc(self: *Assembler, rd: Register, imm: u20) !void {
    try self.emit(Instruction{ .u = .{
        .opcode = .auipc,
        .rd = rd,
        .imm_31_12 = imm,
    } });
}

pub fn ret(self: *Assembler) !void {
    try self.jr(.ra, 0);
}

fn load(self: *Assembler, size: riscv64.LoadStoreSize, dst: Register, offset: i12, base: Register) !void {
    try self.emit(Instruction{ .i = .{
        .opcode = .load,
        .funct3 = @intFromEnum(size),
        .rd = dst,
        .rs1 = base,
        .imm = @bitCast(offset),
    } });
}

fn store(self: *Assembler, size: riscv64.LoadStoreSize, src: Register, offset: i12, base: Register) !void {
    // unsigned stores do not exist
    assert(size != .byte_unsigned);
    assert(size != .halfword_unsigned);
    assert(size != .word_unsigned);
    try self.emit(Instruction.makeS(
        .store,
        @intFromEnum(size),
        base,
        src,
        @bitCast(offset),
    ));
}

pub fn ld(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    // TODO try c.ldsp, c.ld
    return self.load(.doubleword, dst, offset, base);
}

pub fn lw(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    // TODO try c.lwsp, c.lw
    return self.load(.word, dst, offset, base);
}

pub fn lwu(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    return self.load(.word_unsigned, dst, offset, base);
}

pub fn lh(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    // TODO try c.lh (Zcb)
    return self.load(.halfword, dst, offset, base);
}

pub fn lhu(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    // TODO try c.lhu (Zcb)
    return self.load(.halfword_unsigned, dst, offset, base);
}

pub fn lb(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    return self.load(.byte, dst, offset, base);
}

pub fn lbu(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    // TODO try c.lbu (Zcb)
    return self.load(.byte_unsigned, dst, offset, base);
}

pub fn sd(self: *Assembler, src: Register, offset: i12, base: Register) !void {
    // TODO try c.sdsp, c.sd
    return self.store(.doubleword, src, offset, base);
}

pub fn sw(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    // TODO try c.swsp, c.sw
    return self.store(.word, dst, offset, base);
}

pub fn sh(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    // TODO try c.sh (Zcb)
    return self.store(.halfword, dst, offset, base);
}

pub fn sb(self: *Assembler, dst: Register, offset: i12, base: Register) !void {
    // TODO try c.sb (Zcb)
    return self.store(.byte, dst, offset, base);
}

fn c_mv(self: *Assembler, dst: Register.NonZero, src: Register.NonZero) !void {
    try self.emit(Instruction.Compressed{ .cr = .{
        .funct4 = 0b1000,
        .rd_rs1 = .from(dst),
        .rs2 = .from(src),
        .op = 0b10,
    } });
}

pub fn mv(self: *Assembler, dst: Register, src: Register) !void {
    if (self.hasCompressed() and dst != .zero and src != .zero) {
        return self.c_mv(dst.nonZero().?, src.nonZero().?);
    }
    return self.addi(dst, src, 0);
}

test "load-immediates are executed correctly" {
    if (@import("builtin").cpu.arch != .riscv64) return error.SkipZigTest;

    const immediates = [_]i32{
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
        0x7fffffff,
    };

    for (immediates) |i| {
        var any: code_buffer.Any = .{ .writable = .init(std.testing.allocator) };
        defer {
            any.makeWritable() catch unreachable;
            any.writable.deinit();
        }
        var assembler: Assembler = .init(&any.writable.interface, builtin.cpu.features);
        try assembler.li(.a0, i);
        try assembler.ret();
        try any.makeExecutable(&.{});
        const code = any.executable.entrypoint(*const fn () callconv(.c) i64, 0);
        try std.testing.expectEqual(i, code());
    }
}
