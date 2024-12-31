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

/// Specifies which region of a 64-bit register is being referred to
pub const Region = enum {
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

pub const Register = enum {
    // zig fmt: off
    al, cl, dl, bl, spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b,
    ah, ch, dh, bh,
    ax, cx, dx, bx, sp, bp, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w,
    eax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d,
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15,
    // zig fmt: on

    /// Get the numeric value of this register for the ModR/M byte
    pub fn num(self: Register) u3 {
        return switch (self) {
            .al, .ax, .eax, .rax => 0,
            .cl, .cx, .ecx, .rcx => 1,
            .dl, .dx, .edx, .rdx => 2,
            .bl, .bx, .ebx, .rbx => 3,
            .spl, .ah, .sp, .esp, .rsp => 4,
            .bpl, .ch, .bp, .ebp, .rbp => 5,
            .sil, .dh, .si, .esi, .rsi => 6,
            .dil, .bh, .di, .edi, .rdi => 7,
            .r8b, .r8w, .r8d, .r8 => 0,
            .r9b, .r9w, .r9d, .r9 => 1,
            .r10b, .r10w, .r10d, .r10 => 2,
            .r11b, .r11w, .r11d, .r11 => 3,
            .r12b, .r12w, .r12d, .r12 => 4,
            .r13b, .r13w, .r13d, .r13 => 5,
            .r14b, .r14w, .r14d, .r14 => 6,
            .r15b, .r15w, .r15d, .r15 => 7,
        };
    }

    /// Return true if this is one of the registers added with x86_64, r8-r15 (or a part thereof)
    pub fn isExtendedHalf(self: Register) bool {
        return switch (self) {
            .r8b, .r8w, .r8d, .r8 => true,
            .r9b, .r9w, .r9d, .r9 => true,
            .r10b, .r10w, .r10d, .r10 => true,
            .r11b, .r11w, .r11d, .r11 => true,
            .r12b, .r12w, .r12d, .r12 => true,
            .r13b, .r13w, .r13d, .r13 => true,
            .r14b, .r14w, .r14d, .r14 => true,
            .r15b, .r15w, .r15d, .r15 => true,
            else => false,
        };
    }

    /// Indicate which part of a register this register refers to
    pub fn region(self: Register) Region {
        return switch (self) {
            .al, .cl, .dl, .bl, .spl, .bpl, .sil, .dil, .r8b, .r9b, .r10b, .r11b, .r12b, .r13b, .r14b, .r15b => .byte_l,
            .ah, .ch, .dh, .bh => .byte_h,
            .ax, .cx, .dx, .bx, .sp, .bp, .si, .di, .r8w, .r9w, .r10w, .r11w, .r12w, .r13w, .r14w, .r15w => .word,
            .eax, .ecx, .edx, .ebx, .esp, .ebp, .esi, .edi, .r8d, .r9d, .r10d, .r11d, .r12d, .r13d, .r14d, .r15d => .dword,
            .rax, .rcx, .rdx, .rbx, .rsp, .rbp, .rsi, .rdi, .r8, .r9, .r10, .r11, .r12, .r13, .r14, .r15 => .qword,
        };
    }

    /// Determine whether this register places a constraint on whether the REX prefix should be used
    pub fn rex(self: Register) ?RexUsage {
        if (self.isExtendedHalf() or self.region() == .qword) {
            return .mandatory;
        }
        if (self == .spl or self == .bpl or self == .sil or self == .dil) {
            return .mandatory;
        }
        if (self.region() == .byte_h) {
            return .disallowed;
        }
        // this register can be used with or without REX
        return null;
    }

    /// Return the 32-bit register numbered n
    pub fn numbered32(n: u3) Register {
        return switch (n) {
            0 => .eax,
            1 => .ecx,
            2 => .edx,
            3 => .ebx,
            4 => .esp,
            5 => .ebp,
            6 => .esi,
            7 => .edi,
        };
    }

    /// Determine whether the source or target of a function call is responsible for saving this
    /// register's value
    pub fn saver(self: Register) Saver {
        // x86_64 *Linux* calling convention
        return switch (self) {
            .bl, .bh, .bx, .ebx, .rbx => .callee,
            .spl, .sp, .esp, .rsp => .callee,
            .bpl, .bp, .ebp, .rbp => .callee,
            .r12b, .r12w, .r12d, .r12 => .callee,
            .r13b, .r13w, .r13d, .r13 => .callee,
            .r14b, .r14w, .r14d, .r14 => .callee,
            .r15b, .r15w, .r15d, .r15 => .callee,

            .al, .ah, .ax, .eax, .rax => .caller,
            .cl, .ch, .cx, .ecx, .rcx => .caller,
            .dl, .dh, .dx, .edx, .rdx => .caller,
            .sil, .si, .esi, .rsi => .caller,
            .dil, .di, .edi, .rdi => .caller,
            .r8b, .r8w, .r8d, .r8 => .caller,
            .r9b, .r9w, .r9d, .r9 => .caller,
            .r10b, .r10w, .r10d, .r10 => .caller,
            .r11b, .r11w, .r11d, .r11 => .caller,
        };
    }
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
            .reg = reg.num(),
        };
    }

    pub fn indexWithDisp8(rm: IndexDispReg, reg: Register) ModRM {
        return .{
            .mod = .index_with_disp8,
            .rm = @intFromEnum(rm),
            .reg = reg.num(),
        };
    }

    pub fn indexWithDisp32(rm: IndexDispReg, reg: Register) ModRM {
        return .{
            .mod = .index_with_disp32,
            .rm = @intFromEnum(rm),
            .reg = reg.num(),
        };
    }

    pub fn register(rm: Register, reg: Register) ModRM {
        return .{
            .mod = .register,
            .rm = rm.num(),
            .reg = reg.num(),
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
        return @enumFromInt(@intFromEnum(self) | @as(u8, register.num()));
    }
};
