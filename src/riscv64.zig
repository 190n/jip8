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
            @enumFromInt(@as(u3, @truncate(self)))
        else
            null;
    }
};

pub const Instruction = packed union {
    r: packed struct(u32) {
        opcode: u7, // todo enum
        rd: Register,
        funct3: u3, // todo enum
        rs1: Register,
        rs2: Register,
        funct7: u7, // todo enum
    },
    i: packed struct(u32) {
        opcode: u7,
        rd: Register,
        funct3: u3,
        rs1: Register,
        imm: u12,
    },
    s: packed struct(u32) {
        opcode: u7,
        imm_4_0: u5,
        funct3: u3,
        rs1: Register,
        rs2: Register,
        imm_11_5: u7,
    },
    b: packed struct(u32) {
        opcode: u7,
        imm_11: u1,
        imm_4_1: u4,
        funct3: u3,
        rs1: Register,
        rs2: Register,
        imm_10_5: u6,
        imm_12: u1,
    },
    u: packed struct(u32) {
        opcode: u7,
        rd: Register,
        imm_31_12: u20,
    },
    j: packed struct(u32) {
        opcode: u7,
        rd: Register,
        imm_19_12: u8,
        imm_11: u1,
        imm_10_1: u10,
        imm_20: u1,
    },

    pub fn makeS(opcode: u7, funct3: u3, rs1: Register, rs2: Register, imm: u12) Instruction {
        return .{ .s = .{
            .opcode = opcode,
            .imm_4_0 = @truncate(imm),
            .funct3 = funct3,
            .rs1 = rs1,
            .rs2 = rs2,
            .imm_11_5 = @truncate(imm >> 5),
        } };
    }

    pub fn makeB(opcode: u7, funct3: u3, rs1: Register, rs2: Register, imm: u13) Instruction {
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

    pub fn makeJ(opcode: u7, rd: Register, imm: u21) Instruction {
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
