const std = @import("std");

pub const Assembler = @import("./riscv64/Assembler.zig");

pub const Register = enum(u5) {
    zero = 0,
    ra = 1,
    sp = 2,
    gp = 3,
    tp = 4,
    t0 = 5,
    t1 = 6,
    t2 = 7,
    s0 = 8,
    s1 = 9,
    a0 = 10,
    a1 = 11,
    a2 = 12,
    a3 = 13,
    a4 = 14,
    a5 = 15,
    a6 = 16,
    a7 = 17,
    s2 = 18,
    s3 = 19,
    s4 = 20,
    s5 = 21,
    s6 = 22,
    s7 = 23,
    s8 = 24,
    s9 = 25,
    s10 = 26,
    s11 = 27,
    t3 = 28,
    t4 = 29,
    t5 = 30,
    t6 = 31,

    pub const Compressed = enum(u3) { s0, s1, a0, a1, a2, a3, a4, a5 };

    pub fn compressed(self: Register) ?Compressed {
        return if (8 <= @intFromEnum(self) and @intFromEnum(self) < 16)
            @enumFromInt(@as(u3, @truncate(@intFromEnum(self))))
        else
            null;
    }
};

/// Major opcode
pub const Opcode = enum(u7) {
    load = 0b00_000_11,
    load_fp = 0b00_001_11,
    misc_mem = 0b00_011_11,
    op_imm = 0b00_100_11,
    auipc = 0b00_101_11,
    op_imm_32 = 0b00_110_11,

    store = 0b01_000_11,
    store_fp = 0b01_001_11,
    amo = 0b01_011_11,
    op = 0b01_100_11,
    lui = 0b01_101_11,
    op_32 = 0b01_110_11,

    madd = 0b10_000_11,
    msub = 0b10_001_11,
    nmsub = 0b10_010_11,
    nmadd = 0b10_011_11,
    op_fp = 0b10_100_11,
    op_v = 0b10_101_11,

    branch = 0b11_000_11,
    jalr = 0b11_001_11,
    jal = 0b11_011_11,
    system = 0b11_100_11,
    op_ve = 0b11_101_11,
};

pub const Instruction = packed union {
    r: packed struct(u32) {
        opcode: Opcode,
        rd: Register,
        funct3: u3, // todo enum
        rs1: Register,
        rs2: Register,
        funct7: u7, // todo enum
    },
    i: packed struct(u32) {
        opcode: Opcode,
        rd: Register,
        funct3: u3,
        rs1: Register,
        imm: u12,
    },
    s: packed struct(u32) {
        opcode: Opcode,
        imm_4_0: u5,
        funct3: u3,
        rs1: Register,
        rs2: Register,
        imm_11_5: u7,
    },
    b: packed struct(u32) {
        opcode: Opcode,
        imm_11: u1,
        imm_4_1: u4,
        funct3: u3,
        rs1: Register,
        rs2: Register,
        imm_10_5: u6,
        imm_12: u1,
    },
    u: packed struct(u32) {
        opcode: Opcode,
        rd: Register,
        imm_31_12: u20,
    },
    j: packed struct(u32) {
        opcode: Opcode,
        rd: Register,
        imm_19_12: u8,
        imm_11: u1,
        imm_10_1: u10,
        imm_20: u1,
    },

    pub fn makeS(opcode: Opcode, funct3: u3, rs1: Register, rs2: Register, imm: u12) Instruction {
        return .{ .s = .{
            .opcode = opcode,
            .imm_4_0 = @truncate(imm),
            .funct3 = funct3,
            .rs1 = rs1,
            .rs2 = rs2,
            .imm_11_5 = @truncate(imm >> 5),
        } };
    }

    pub fn makeB(opcode: Opcode, funct3: u3, rs1: Register, rs2: Register, imm: u13) Instruction {
        std.debug.assert(imm % 2 == 0);
        return .{ .b = .{
            .opcode = opcode,
            .imm_11 = @truncate(imm >> 11),
            .imm_4_1 = @truncate(imm >> 1),
            .funct3 = funct3,
            .rs1 = rs1,
            .rs2 = rs2,
            .imm_10_5 = @truncate(imm >> 5),
            .imm_12 = @truncate(imm >> 12),
        } };
    }

    pub fn makeJ(opcode: Opcode, rd: Register, imm: u21) Instruction {
        std.debug.assert(imm % 2 == 0);
        return .{ .j = .{
            .opcode = opcode,
            .rd = rd,
            .imm_19_12 = @truncate(imm >> 12),
            .imm_11 = @truncate(imm >> 11),
            .imm_10_1 = @truncate(imm >> 1),
            .imm_20 = @truncate(imm >> 20),
        } };
    }

    pub fn any(self: Instruction) AnyInstruction {
        return .{ .rv = self };
    }
};

pub const AnyInstruction = union(enum) {
    rv: Instruction,

    pub fn writeTo(self: AnyInstruction, writer: anytype) !void {
        switch (self) {
            .rv => |i| try writer.writeInt(u32, @bitCast(i), .little),
        }
    }
};
