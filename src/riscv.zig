const std = @import("std");

pub const Assembler = @import("./riscv/Assembler.zig");
pub const Compiler = @import("./riscv/Compiler.zig");

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

    pub fn isCalleeSaved(self: Register) bool {
        return switch (self) {
            .ra, .sp, .gp, .tp, .s0, .s1, .s2, .s3, .s4, .s5, .s6, .s7, .s8, .s9, .s10, .s11 => true,
            else => false,
        };
    }

    pub const NonZero = @Type(.{
        .@"enum" = .{
            .tag_type = u5,
            .decls = &.{},
            .is_exhaustive = true,
            .fields = fields: {
                const reg_fields = @typeInfo(Register).@"enum".fields;
                std.debug.assert(reg_fields[0].value == 0);
                break :fields reg_fields[1..];
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
    custom_forward_chip8_jump = 0b00_010_11,
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

/// Used for funct3 in load/store instructions
pub const LoadStoreSize = enum(u3) {
    byte = 0b000,
    halfword = 0b001,
    word = 0b010,
    byte_unsigned = 0b100,
    halfword_unsigned = 0b101,
    /// RV64 only
    word_unsigned = 0b110,
    /// RV64 only
    doubleword = 0b011,
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
        funct3: Funct3Branch,
        rs1: Register,
        rs2: Register,
        imm_10_5: u6,
        imm_12: u1,

        /// Edit the B-immediate bits of this instruction to be imm. The rest of the instruction is unchanged.
        pub fn setOffset(self: *align(2) @This(), imm: i13) void {
            std.debug.assert(@rem(imm, 2) == 0);
            const uimm: u13 = @bitCast(imm);
            self.imm_4_1 = @truncate(uimm >> 1);
            self.imm_10_5 = @truncate(uimm >> 5);
            self.imm_11 = @truncate(uimm >> 11);
            self.imm_12 = @truncate(uimm >> 12);
        }
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

        /// Edit the J-immediate bits of this instruction to be imm. The rest of the instruction is unchanged.
        pub fn setOffset(self: *align(2) @This(), imm: i21) void {
            std.debug.assert(@rem(imm, 2) == 0);
            const uimm: u21 = @bitCast(imm);
            self.imm_10_1 = @truncate(uimm >> 1);
            self.imm_11 = @truncate(uimm >> 11);
            self.imm_19_12 = @truncate(uimm >> 12);
            self.imm_20 = @truncate(uimm >> 20);
        }

        pub fn offset(self: @This()) i21 {
            var unsigned: u21 = 0;
            unsigned |= @as(u21, self.imm_10_1) << 1;
            unsigned |= @as(u21, self.imm_11) << 11;
            unsigned |= @as(u21, self.imm_19_12) << 12;
            unsigned |= @as(u21, self.imm_20) << 20;
            return @bitCast(unsigned);
        }
    },
    custom_forward_chip8_jump: packed struct(u32) {
        opcode: Opcode = .custom_forward_chip8_jump,
        target: u12,
        kind: enum(u1) { jump, call },
        pad: u12 = 0,
    },

    pub const Funct3Branch = enum(u3) {
        beq = 0b000,
        bne = 0b001,
        blt = 0b100,
        bge = 0b101,
        bltu = 0b110,
        bgeu = 0b111,
    };

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

    pub fn makeB(opcode: Opcode, funct3: Funct3Branch, rs1: Register, rs2: Register, imm: u13) Instruction {
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

    pub fn writeTo(self: AnyInstruction, writer: *std.io.Writer) !void {
        switch (self) {
            .rv => |i| try writer.writeInt(u32, @bitCast(i), .little),
            .rvc => |i| try writer.writeInt(u16, @bitCast(i), .little),
        }
    }
};

pub const AnyInstructionPtr = union(enum) {
    rv: *align(2) Instruction,
    rvc: *Instruction.Compressed,
};

pub const FloatWidth = enum {
    /// F extension, 32 bits
    f,
    /// D extension, 64 bits
    d,
};

pub const Bits = enum(u1) {
    @"32",
    @"64",

    pub fn bits(self: Bits) u8 {
        return switch (self) {
            .@"32" => 32,
            .@"64" => 64,
        };
    }

    pub fn bytes(self: Bits) u8 {
        return self.bits() / 8;
    }
};

pub const InstructionIterator = struct {
    bytes: []u8,

    pub fn init(bytes: []u8) InstructionIterator {
        return .{ .bytes = bytes };
    }

    pub fn next(self: *InstructionIterator) ?AnyInstructionPtr {
        if (self.bytes.len == 0) {
            return null;
        }
        const halfword = std.mem.readInt(u16, self.bytes[0..2], .little);
        switch (halfword & 0b11) {
            0b11 => {
                defer self.bytes = self.bytes[4..];
                return .{ .rv = @ptrCast(@alignCast(&self.bytes[0])) };
            },
            else => {
                defer self.bytes = self.bytes[2..];
                return .{ .rvc = @ptrCast(@alignCast(&self.bytes[0])) };
            },
        }
    }
};

test InstructionIterator {
    var bytes = [_]u8{ 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x02, 0x00 };
    var iter: InstructionIterator = .init(&bytes);
    const t = std.testing;
    try t.expectEqual(0x00000003, @as(u32, @bitCast(iter.next().?.rv.*)));
    try t.expectEqual(0x0000, @as(u16, @bitCast(iter.next().?.rvc.*)));
    try t.expectEqual(0x0001, @as(u16, @bitCast(iter.next().?.rvc.*)));
    try t.expectEqual(0x0002, @as(u16, @bitCast(iter.next().?.rvc.*)));
    try t.expectEqual(null, iter.next());
}
