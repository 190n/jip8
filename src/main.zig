const std = @import("std");

const Rex = packed struct(u8) {
    b: u1 = 0,
    x: u1 = 0,
    r: u1 = 0,
    w: u1 = 0,
    _pad: u4 = 0b0100,
};

const Width = enum {
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

const RexUsage = enum {
    /// This register can only be used if REX prefix is used
    mandatory,
    /// This register can only be used if REX prefix is not used
    disallowed,
};

const Reg = enum {
    // zig fmt: off
    al, cl, dl, bl, spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b,
    ah, ch, dh, bh,
    ax, cx, dx, bx, sp, bp, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w,
    eax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d,
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15,
    // zig fmt: on

    /// Get the numeric value of this register. First tuple element goes in ModR/M byte and second
    /// goes in REX prefix (if used)
    pub fn num(self: Reg) struct { u3, u1 } {
        return switch (self) {
            .al, .ax, .eax, .rax => .{ 0, 0 },
            .cl, .cx, .ecx, .rcx => .{ 1, 0 },
            .dl, .dx, .edx, .rdx => .{ 2, 0 },
            .bl, .bx, .ebx, .rbx => .{ 3, 0 },
            .spl, .ah, .sp, .esp, .rsp => .{ 4, 0 },
            .bpl, .ch, .bp, .ebp, .rbp => .{ 5, 0 },
            .sil, .dh, .si, .esi, .rsi => .{ 6, 0 },
            .dil, .bh, .di, .edi, .rdi => .{ 7, 0 },
            .r8b, .r8w, .r8d, .r8 => .{ 0, 1 },
            .r9b, .r9w, .r9d, .r9 => .{ 1, 1 },
            .r10b, .r10w, .r10d, .r10 => .{ 2, 1 },
            .r11b, .r11w, .r11d, .r11 => .{ 3, 1 },
            .r12b, .r12w, .r12d, .r12 => .{ 4, 1 },
            .r13b, .r13w, .r13d, .r13 => .{ 5, 1 },
            .r14b, .r14w, .r14d, .r14 => .{ 6, 1 },
            .r15b, .r15w, .r15d, .r15 => .{ 7, 1 },
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
        if (self.num()[1] == 1) return .mandatory;
        if (self.width() == .byte_h) return .disallowed;
        if (self == .spl or self == .bpl or self == .sil or self == .dil) return .mandatory;
        if (self.width() == .qword) return .mandatory;
        return null;
    }
};

/// Specifies a register in the R/M field when Mod is 00, which means esp and ebp have different meanings
const IndexReg = enum(u3) {
    eax,
    ecx,
    edx,
    ebx,
    /// Memory location is indicated by SIB byte which must follow this ModR/M
    sib,
    /// Memory location is rip plus a signed 32-bit displacement following ModR/M
    disp32,
    esi,
    edi,
};

/// Specifies a register in the R/M field when Mod is 01 or 10, which means esp has a different meaning
const IndexDispReg = enum(u3) {
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

const Mod = enum(u2) {
    index,
    index_with_disp8,
    index_with_disp32,
    register,
};

const ModRM = packed struct(u8) {
    rm: u3,
    reg: u3,
    mod: Mod,

    pub fn index(rm: IndexReg, reg: Reg) ModRM {
        return .{
            .mod = .index,
            .rm = @intFromEnum(rm),
            .reg = reg.num()[0],
        };
    }

    pub fn indexWithDisp8(rm: IndexDispReg, reg: Reg) ModRM {
        return .{
            .mod = .index_with_disp8,
            .rm = @intFromEnum(rm),
            .reg = reg.num()[0],
        };
    }

    pub fn indexWithDisp32(rm: IndexDispReg, reg: Reg) ModRM {
        return .{
            .mod = .index_with_disp32,
            .rm = @intFromEnum(rm),
            .reg = reg.num()[0],
        };
    }

    pub fn register(rm: Reg, reg: Reg) ModRM {
        return .{
            .mod = .register,
            .rm = rm.num()[0],
            .reg = reg.num()[0],
        };
    }
};

fn movRegReg(
    code: *std.ArrayListAligned(u8, std.mem.page_size),
    dst: Reg,
    src: Reg,
) error{ OutOfMemory, WidthMismatch, IncompatibleArguments }!void {
    if (dst.width().numeric() != src.width().numeric()) {
        return error.WidthMismatch;
    }
    const dst_ex = dst.num()[1];
    const src_ex = src.num()[1];

    const dst_rex = dst.rex();
    const src_rex = src.rex();
    if (dst_rex != null and src_rex != null and dst_rex != src_rex) {
        return error.IncompatibleArguments;
    }

    if (dst.width() == .word) {
        try code.append(0x66);
    }
    if (dst_rex == .mandatory or src_rex == .mandatory) {
        try code.append(@bitCast(Rex{
            .w = @intFromBool(dst.width() == .qword),
            .b = dst_ex,
            .r = src_ex,
        }));
    }
    try code.append(if (dst.width() == .byte_h or dst.width() == .byte_l) 0x88 else 0x89);
    try code.append(@bitCast(ModRM.register(dst, src)));
}

pub fn main() !void {
    var code = std.ArrayListAligned(u8, std.mem.page_size).init(std.heap.page_allocator);
    defer code.deinit();

    // mov rax, qword ptr [rdi]
    // rex.w + 8b /r
    // /r: modr/m byte contains register and r/m

    // // rex.w
    // try code.append(@bitCast(Rex{ .w = 1 }));
    // // mov
    // try code.append(0x8b);
    // // mod r/m ([edi] to eax)
    // try code.append(@bitCast(ModRM.gen(.index, .edi, .eax)));

    // xor eax, eax
    try code.appendSlice(&.{ 0x31, 0xc0 });
    try movRegReg(&code, .rax, .rax);
    @breakpoint();
    try movRegReg(&code, .al, .dil);
    try movRegReg(&code, .cl, .sil);
    try movRegReg(&code, .ah, .cl);
    try movRegReg(&code, .r8b, .spl);
    try movRegReg(&code, .r13w, .bp);
    try movRegReg(&code, .cx, .ax);
    try movRegReg(&code, .rax, .r15);
    try movRegReg(&code, .r10, .rsp);
    try movRegReg(&code, .r12w, .bp);
    try movRegReg(&code, .ebx, .r9d);

    // ret
    try code.append(0xc3);

    try std.io.getStdOut().writeAll(code.items);

    try code.ensureTotalCapacityPrecise(std.mem.alignForward(usize, code.items.len, std.mem.page_size));
    try std.posix.mprotect(code.allocatedSlice(), std.posix.PROT.READ | std.posix.PROT.EXEC);
    defer std.posix.mprotect(code.allocatedSlice(), std.posix.PROT.READ | std.posix.PROT.WRITE) catch unreachable;

    const f: *const fn (u64, u64) callconv(.C) u64 = @ptrCast(code.items.ptr);
    std.debug.print("{}\n", .{f(1, 2)});
}

test "movRegReg" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var asm_file = try tmp_dir.dir.createFile("code.S", .{});
    var code_buf = std.ArrayListAligned(u8, std.mem.page_size).init(std.testing.allocator);
    defer code_buf.deinit();

    var read_buf: [16]u8 = undefined;

    const regs = [_]Reg{ .al, .ah, .ax, .eax, .rax, .spl, .r8b, .r8w, .r8d, .r8 };

    for (regs) |r1| {
        for (regs) |r2| {
            try asm_file.seekTo(0);
            try asm_file.setEndPos(0);
            try asm_file.writer().print(
                \\.intel_syntax
                \\mov {s}, {s}
            , .{ @tagName(r1), @tagName(r2) });

            const clang_result = try std.process.Child.run(.{
                .allocator = std.testing.allocator,
                .argv = &.{ "zig", "cc", "-target", "x86_64-linux-gnu", "-c", "code.S" },
                .cwd_dir = tmp_dir.dir,
            });
            defer std.testing.allocator.free(clang_result.stderr);
            defer std.testing.allocator.free(clang_result.stdout);

            if (clang_result.term.Exited == 0) {
                // our assembler should succeed
                const objcopy_result = try std.process.Child.run(.{
                    .allocator = std.testing.allocator,
                    .argv = &.{ "zig", "objcopy", "-O", "binary", "code.o", "code.bin" },
                    .cwd_dir = tmp_dir.dir,
                });
                defer std.testing.allocator.free(objcopy_result.stderr);
                defer std.testing.allocator.free(objcopy_result.stdout);
                try std.testing.expectEqual(0, objcopy_result.term.Exited);

                code_buf.clearRetainingCapacity();
                try movRegReg(&code_buf, r1, r2);

                var bin_file = try tmp_dir.dir.openFile("code.bin", .{});
                defer bin_file.close();
                const size = try bin_file.readAll(&read_buf);

                try std.testing.expectEqualSlices(u8, read_buf[0..size], code_buf.items);
            } else {
                // our assembler should fail
                const did_error = if (movRegReg(&code_buf, r1, r2))
                    false
                else |_|
                    true;
                try std.testing.expect(did_error);
            }
        }
    }
}
