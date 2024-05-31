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

pub const Width = enum {
    byte_l,
    byte_h,
    word,
    dword,
    qword,

    pub fn numeric(self: Width) u8 {
        return switch (self) {
            .byte_l, .byte_h => 8,
            .word => 16,
            .dword => 32,
            .qword => 64,
        };
    }
};

pub const RexUsage = enum {
    /// This register can only be used if REX prefix is used
    mandatory,
    /// This register can only be used if REX prefix is not used
    disallowed,
};

pub const Reg = enum {
    // zig fmt: off
    al, cl, dl, bl, spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b,
    ah, ch, dh, bh,
    ax, cx, dx, bx, sp, bp, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w,
    eax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d,
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15,
    // zig fmt: on

    /// Get the numeric value of this register for the ModR/M byte
    pub fn num(self: Reg) u3 {
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
    pub fn isExtendedHalf(self: Reg) bool {
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

    pub fn width(self: Reg) Width {
        return switch (self) {
            .al, .cl, .dl, .bl, .spl, .bpl, .sil, .dil, .r8b, .r9b, .r10b, .r11b, .r12b, .r13b, .r14b, .r15b => .byte_l,
            .ah, .ch, .dh, .bh => .byte_h,
            .ax, .cx, .dx, .bx, .sp, .bp, .si, .di, .r8w, .r9w, .r10w, .r11w, .r12w, .r13w, .r14w, .r15w => .word,
            .eax, .ecx, .edx, .ebx, .esp, .ebp, .esi, .edi, .r8d, .r9d, .r10d, .r11d, .r12d, .r13d, .r14d, .r15d => .dword,
            .rax, .rcx, .rdx, .rbx, .rsp, .rbp, .rsi, .rdi, .r8, .r9, .r10, .r11, .r12, .r13, .r14, .r15 => .qword,
        };
    }

    /// Determine whether this register places a constraint on whether the REX prefix should be used
    pub fn rex(self: Reg) ?RexUsage {
        if (self.isExtendedHalf()) return .mandatory;
        if (self.width() == .byte_h) return .disallowed;
        if (self == .spl or self == .bpl or self == .sil or self == .dil) return .mandatory;
        if (self.width() == .qword) return .mandatory;
        return null;
    }

    /// Return the 32-bit register numbered n
    pub fn numbered32(n: u3) Reg {
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

pub const Mod = enum(u2) {
    index,
    index_with_disp8,
    index_with_disp32,
    register,
};

pub const ModRM = packed struct(u8) {
    rm: u3,
    reg: u3,
    mod: Mod,

    pub fn index(rm: IndexReg, reg: Reg) ModRM {
        return .{
            .mod = .index,
            .rm = @intFromEnum(rm),
            .reg = reg.num(),
        };
    }

    pub fn indexWithDisp8(rm: IndexDispReg, reg: Reg) ModRM {
        return .{
            .mod = .index_with_disp8,
            .rm = @intFromEnum(rm),
            .reg = reg.num(),
        };
    }

    pub fn indexWithDisp32(rm: IndexDispReg, reg: Reg) ModRM {
        return .{
            .mod = .index_with_disp32,
            .rm = @intFromEnum(rm),
            .reg = reg.num(),
        };
    }

    pub fn register(rm: Reg, reg: Reg) ModRM {
        return .{
            .mod = .register,
            .rm = rm.num(),
            .reg = reg.num(),
        };
    }
};
