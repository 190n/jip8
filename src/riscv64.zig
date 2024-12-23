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

    /// Frame pointer
    pub const fp: Register = .s0;

    pub const NonZero = @Type(.{
        .@"enum" = .{
            .tag_type = u5,
            .decls = &.{},
            .is_exhaustive = true,
            .fields = blk: {
                const reg_fields = @typeInfo(Register).@"enum".fields;
                std.debug.assert(reg_fields[0].value == 0);
                break :blk reg_fields[1..];
            },
        },
    });

    pub const Compressed = enum(u3) { s0, s1, a0, a1, a2, a3, a4, a5 };

    pub fn compressed(self: Register) ?Compressed {
        return if (8 <= @intFromEnum(self) and @intFromEnum(self) < 16)
            @enumFromInt(@as(u3, @truncate(@intFromEnum(self))))
        else
            null;
    }

    pub fn nonZero(self: Register) ?NonZero {
        return if (self == .zero)
            null
        else
            @enumFromInt(@intFromEnum(self));
    }

    pub fn from(other: anytype) Register {
        return switch (@TypeOf(other)) {
            Compressed => @enumFromInt(@as(u5, @intFromEnum(other)) + 8),
            NonZero => @enumFromInt(@intFromEnum(other)),
            else => |T| @compileError("wrong type passed to Register.from(): " ++ @typeName(T)),
        };
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

    pub const Compressed = packed union {
        cr: packed struct(u16) {
            op: u2,
            rs2: Register,
            rd_rs1: Register,
            funct4: u4,
        },
        ci: packed struct(u16) {
            op: u2,
            imm_1: u5,
            rd_rs1: Register,
            imm_2: u1,
            funct3: u3,
        },
        css: packed struct(u16) {
            op: u2,
            rs2: Register,
            imm: u6,
            funct3: u3,
        },
        ciw: packed struct(u16) {
            op: u2,
            rd: Register.Compressed,
            imm: u8,
            funct3: u3,
        },
        cl: packed struct(u16) {
            op: u2,
            rd: Register.Compressed,
            imm_1: u2,
            rs1: Register.Compressed,
            imm_2: u3,
            funct3: u3,
        },
        cs: packed struct(u16) {
            op: u2,
            rs2: Register.Compressed,
            imm_1: u2,
            rs1: Register.Compressed,
            imm_2: u3,
            funct3: u3,
        },
        ca: packed struct(u16) {
            op: u2,
            rs2: Register.Compressed,
            funct2: u2,
            rd_rs1: Register.Compressed,
            funct6: u6,
        },
        cb: packed struct(u16) {
            op: u2,
            offset_1: u5,
            rd_rs1: Register.Compressed,
            offset_2: u3,
            funct3: u3,
        },
        cj: packed struct(u16) {
            op: u2,
            target: u11,
            funct3: u3,
        },

        pub fn any(self: Compressed) AnyInstruction {
            return .{ .rvc = self };
        }
    };

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
    rvc: Instruction.Compressed,

    pub fn writeTo(self: AnyInstruction, writer: anytype) !void {
        switch (self) {
            .rv => |i| try writer.writeInt(u32, @bitCast(i), .little),
            .rvc => |i| try writer.writeInt(u16, @bitCast(i), .little),
        }
    }
};
