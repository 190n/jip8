const std = @import("std");
const assert = std.debug.assert;

pub const Assembler = @import("./x86_64/Assembler.zig");

/// REX prefix, used to access 64-bit registers and r8-r15
pub const Rex = packed struct(u8) {
    /// Extension of R/M field in ModR/M
    b: bool = false,
    x: bool = false,
    /// Extension of Reg field in ModR/M
    r: bool = false,
    /// Whether 64-bit wide registers are used
    w: bool = false,

    _pad: enum(u4) { rex = 0b0100 } = .rex,
};

/// Describe who is responsible for saving a register's value across function calls
pub const Saver = enum {
    /// A called function may change this register without restoring its value before returning. If
    /// the caller wants to preserve its value, it must spill the register onto the stack before a
    /// function call.
    caller,
    /// A called function must not change this register's value without restoring it before
    /// returning.
    callee,
};

/// Describe whether a register requires the REX prefix or is incompatible with the REX prefix
pub const RexUsage = enum {
    /// This register can only be used if REX prefix is used
    mandatory,
    /// This register can only be used if REX prefix is not used
    disallowed,
};

pub const Register = packed struct {
    number: Unsized,
    region: Region,

    /// Specifies one of the 16 general-purpose registers
    pub const Unsized = enum(u4) {
        rax,
        rcx,
        rdx,
        rbx,
        rsp,
        rbp,
        rsi,
        rdi,
        r8,
        r9,
        r10,
        r11,
        r12,
        r13,
        r14,
        r15,

        /// Return true if this is one of the registers added with x86_64, r8-r15 (or a part thereof)
        pub fn isExtendedHalf(self: Unsized) bool {
            return @intFromEnum(self) >= 8;
        }

        /// Determine whether the source or target of a function call is responsible for saving this
        /// register's value in the x86_64 Linux calling convention
        pub fn saver(self: Unsized) Saver {
            return switch (self) {
                .rax, .rcx, .rdx, .rsi, .rdi, .r8, .r9, .r10, .r11 => .caller,
                .rbx, .rsp, .rbp, .r12, .r13, .r14, .r15 => .callee,
            };
        }

        /// Determine whether bits 8:15 of this register can be addressed (e.g. ah exists but not sph)
        pub fn hasByteH(self: Unsized) bool {
            return switch (self) {
                .rax, .rcx, .rdx, .rbx => true,
                .rsp, .rbp, .rsi, .rdi, .r8, .r9, .r10, .r11, .r12, .r13, .r14, .r15 => false,
            };
        }
    };

    /// Specifies which region of a 64-bit register is being referred to
    pub const Region = enum(u3) {
        /// Bits 0:7
        byte_l,
        /// Bits 8:15
        byte_h,
        /// Bits 0:15
        word,
        /// Bits 0:31
        dword,
        /// Bits 0:63
        qword,

        /// Get the number of bits this region takes up
        pub fn bits(self: Region) u8 {
            return switch (self) {
                .byte_l, .byte_h => 8,
                .word => 16,
                .dword => 32,
                .qword => 64,
            };
        }
    };

    /// Determine whether this register places a constraint on whether the REX prefix should be used
    pub fn rex(self: Register) ?RexUsage {
        if (self.number.isExtendedHalf() or self.region == .qword) {
            return .mandatory;
        }
        if ((self.number == .rsp or
            self.number == .rbp or
            self.number == .rsi or
            self.number == .rdi) and self.region == .byte_l)
        {
            return .mandatory;
        }
        if (self.region == .byte_h) {
            return .disallowed;
        }
        // this register can be used with or without REX
        return null;
    }

    pub fn name(self: Register) []const u8 {
        const names_by_region: [5]?[]const u8 = switch (self.number) {
            .rax => .{ "al", "ah", "ax", "eax", "rax" },
            .rcx => .{ "cl", "ch", "cx", "ecx", "rcx" },
            .rdx => .{ "dl", "dh", "dx", "edx", "rdx" },
            .rbx => .{ "bl", "bh", "bx", "ebx", "rbx" },
            .rsp => .{ "spl", null, "sp", "esp", "rsp" },
            .rbp => .{ "bpl", null, "bp", "ebp", "rbp" },
            .rsi => .{ "sil", null, "si", "esi", "rsi" },
            .rdi => .{ "dil", null, "di", "edi", "rdi" },
            inline else => |num| .{
                @tagName(num) ++ "b",
                null,
                @tagName(num) ++ "w",
                @tagName(num) ++ "d",
                @tagName(num),
            },
        };
        return names_by_region[@intFromEnum(self.region)].?;
    }

    /// Return the 3-bit form of this register for ModR/M
    pub fn lowBits(self: Register) u3 {
        if (self.region == .byte_h) {
            assert(self.number.hasByteH());
            return 4 + @as(u3, @truncate(@intFromEnum(self.number)));
        } else {
            return @truncate(@intFromEnum(self.number));
        }
    }

    pub const al: Register = .{ .number = .rax, .region = .byte_l };
    pub const cl: Register = .{ .number = .rcx, .region = .byte_l };
    pub const dl: Register = .{ .number = .rdx, .region = .byte_l };
    pub const bl: Register = .{ .number = .rbx, .region = .byte_l };
    pub const spl: Register = .{ .number = .rsp, .region = .byte_l };
    pub const bpl: Register = .{ .number = .rbp, .region = .byte_l };
    pub const sil: Register = .{ .number = .rsi, .region = .byte_l };
    pub const dil: Register = .{ .number = .rdi, .region = .byte_l };
    pub const r8b: Register = .{ .number = .r8, .region = .byte_l };
    pub const r9b: Register = .{ .number = .r9, .region = .byte_l };
    pub const r10b: Register = .{ .number = .r10, .region = .byte_l };
    pub const r11b: Register = .{ .number = .r11, .region = .byte_l };
    pub const r12b: Register = .{ .number = .r12, .region = .byte_l };
    pub const r13b: Register = .{ .number = .r13, .region = .byte_l };
    pub const r14b: Register = .{ .number = .r14, .region = .byte_l };
    pub const r15b: Register = .{ .number = .r15, .region = .byte_l };

    pub const ah: Register = .{ .number = .rax, .region = .byte_h };
    pub const ch: Register = .{ .number = .rcx, .region = .byte_h };
    pub const dh: Register = .{ .number = .rdx, .region = .byte_h };
    pub const bh: Register = .{ .number = .rbx, .region = .byte_h };

    pub const ax: Register = .{ .number = .rax, .region = .word };
    pub const cx: Register = .{ .number = .rcx, .region = .word };
    pub const dx: Register = .{ .number = .rdx, .region = .word };
    pub const bx: Register = .{ .number = .rbx, .region = .word };
    pub const sp: Register = .{ .number = .rsp, .region = .word };
    pub const bp: Register = .{ .number = .rbp, .region = .word };
    pub const si: Register = .{ .number = .rsi, .region = .word };
    pub const di: Register = .{ .number = .rdi, .region = .word };
    pub const r8w: Register = .{ .number = .r8, .region = .word };
    pub const r9w: Register = .{ .number = .r9, .region = .word };
    pub const r10w: Register = .{ .number = .r10, .region = .word };
    pub const r11w: Register = .{ .number = .r11, .region = .word };
    pub const r12w: Register = .{ .number = .r12, .region = .word };
    pub const r13w: Register = .{ .number = .r13, .region = .word };
    pub const r14w: Register = .{ .number = .r14, .region = .word };
    pub const r15w: Register = .{ .number = .r15, .region = .word };

    pub const eax: Register = .{ .number = .rax, .region = .dword };
    pub const ecx: Register = .{ .number = .rcx, .region = .dword };
    pub const edx: Register = .{ .number = .rdx, .region = .dword };
    pub const ebx: Register = .{ .number = .rbx, .region = .dword };
    pub const esp: Register = .{ .number = .rsp, .region = .dword };
    pub const ebp: Register = .{ .number = .rbp, .region = .dword };
    pub const esi: Register = .{ .number = .rsi, .region = .dword };
    pub const edi: Register = .{ .number = .rdi, .region = .dword };
    pub const r8d: Register = .{ .number = .r8, .region = .dword };
    pub const r9d: Register = .{ .number = .r9, .region = .dword };
    pub const r10d: Register = .{ .number = .r10, .region = .dword };
    pub const r11d: Register = .{ .number = .r11, .region = .dword };
    pub const r12d: Register = .{ .number = .r12, .region = .dword };
    pub const r13d: Register = .{ .number = .r13, .region = .dword };
    pub const r14d: Register = .{ .number = .r14, .region = .dword };
    pub const r15d: Register = .{ .number = .r15, .region = .dword };

    pub const rax: Register = .{ .number = .rax, .region = .qword };
    pub const rcx: Register = .{ .number = .rcx, .region = .qword };
    pub const rdx: Register = .{ .number = .rdx, .region = .qword };
    pub const rbx: Register = .{ .number = .rbx, .region = .qword };
    pub const rsp: Register = .{ .number = .rsp, .region = .qword };
    pub const rbp: Register = .{ .number = .rbp, .region = .qword };
    pub const rsi: Register = .{ .number = .rsi, .region = .qword };
    pub const rdi: Register = .{ .number = .rdi, .region = .qword };
    pub const r8: Register = .{ .number = .r8, .region = .qword };
    pub const r9: Register = .{ .number = .r9, .region = .qword };
    pub const r10: Register = .{ .number = .r10, .region = .qword };
    pub const r11: Register = .{ .number = .r11, .region = .qword };
    pub const r12: Register = .{ .number = .r12, .region = .qword };
    pub const r13: Register = .{ .number = .r13, .region = .qword };
    pub const r14: Register = .{ .number = .r14, .region = .qword };
    pub const r15: Register = .{ .number = .r15, .region = .qword };
};

/// Specifies a register in the R/M field when Mod is 00, which means esp and ebp have different meanings
pub const IndexReg = enum(u3) {
    eax,
    ecx,
    edx,
    ebx,

    /// Memory location is indicated by SIB byte which must follow this ModR/M
    sib,
    /// Memory location is rip plus a signed 32-bit displacement following ModR/M
    rip_disp32,

    esi,
    edi,
};

/// Specifies a register in the R/M field when Mod is 01 or 10, which means esp has a different meaning
pub const IndexDispReg = enum(u3) {
    eax,
    ecx,
    edx,
    ebx,

    /// Memory location is indicated by SIB byte which must follow this ModR/M
    sib,

    ebp,
    esi,
    edi,
};

/// Mod field of the ModR/M byte
pub const Mod = enum(u2) {
    /// 00: R/M indicates a register to use as an index
    index,
    /// 01: R/M indicates a register to use as an index with an 8-bit displacement
    index_with_disp8,
    /// 10: R/M indicates a register to use as an index with a 32-bit displacement
    index_with_disp32,
    /// 11: R/M indicates a register to use directly
    register,
};

pub const ModRM = packed struct(u8) {
    rm: u3,
    reg: u3,
    mod: Mod,

    pub fn index(rm: IndexReg, reg: Register) ModRM {
        return .{
            .mod = .index,
            .rm = @intFromEnum(rm),
            .reg = reg.lowBits(),
        };
    }

    pub fn indexWithDisp8(rm: IndexDispReg, reg: Register) ModRM {
        return .{
            .mod = .index_with_disp8,
            .rm = @intFromEnum(rm),
            .reg = reg.lowBits(),
        };
    }

    pub fn indexWithDisp32(rm: IndexDispReg, reg: Register) ModRM {
        return .{
            .mod = .index_with_disp32,
            .rm = @intFromEnum(rm),
            .reg = reg.lowBits(),
        };
    }

    pub fn register(rm: Register, reg: Register) ModRM {
        return .{
            .mod = .register,
            .rm = rm.lowBits(),
            .reg = reg.lowBits(),
        };
    }
};

pub const Sib = packed struct(u8) {
    base: Base,
    index: Index,
    scale: Scale,

    pub const Base = enum(u3) {
        eax,
        ecx,
        edx,
        ebx,
        esp,
        /// Mod = 0b00: disp32 with no base
        /// Mod = 0b01: ebp + disp8
        /// Mod = 0b10: ebp + disp32
        sometimes_ebp,
        esi,
        edi,
    };

    pub const Index = enum(u3) {
        eax,
        ecx,
        edx,
        ebx,
        /// Named eiz or riz by some disassemblers.
        /// Of dubious utility since with no index you can avoid SIB byte.
        none,
        ebp,
        esi,
        edi,
    };

    pub const Scale = enum(u2) { @"1", @"2", @"4", @"8" };
};

pub const Opcode = enum(u8) {
    /// Move into an 8-bit register or memory location from an 8-bit register
    mov_rm8_r8 = 0x88,
    /// Move into a 16-, 32-, or 64-bit register or memory location from a register of the same size
    mov_rm_r = 0x89,
    /// Move an 8-bit immediate into a register
    mov_r8_imm8 = 0xb0,
    /// Move a 16-, 32-, or 64-bit immediate into a register
    mov_r_imm = 0xb8,
    /// Move sign-extended 32-bit immediate into a 64-bit register
    mov_r_imm32 = 0xc7,
    /// Return from the current function
    ret = 0xc3,
    /// Breakpoint trap
    int3 = 0xcc,
    /// Call a signed 32-bit offset from RIP (from instruction after call)
    call = 0xe8,
    /// Jump to a signed 32-bit offset from RIP (from instruction after jump)
    jmp32 = 0xe9,
    /// Jump to a signed 8-bit offset from RIP (from instruction after jump)
    jmp8 = 0xeb,
    /// With /4, jump to a register
    jmp = 0xff,
    /// Push a register to the stack
    push = 0x50,
    /// Pop a register from the stack
    pop = 0x58,

    _,

    /// Combine this opcode with a register in the low 3 bytes (for instructions listed with +rb,
    /// +rw, +rd, or +ro
    pub fn plusRegister(self: Opcode, register: Register) Opcode {
        return @enumFromInt(@intFromEnum(self) | @as(u8, register.lowBits()));
    }
};
