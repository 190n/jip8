const std = @import("std");
const assert = std.debug.assert;

const util = @import("./util.zig");

const Reg = util.Reg;
const Rex = util.Rex;
const ModRM = util.ModRM;

const CodeBuf = std.ArrayListAligned(u8, std.mem.page_size);

pub fn binOpRegReg(
    code_buf: *CodeBuf,
    opcode: u8,
    dst: Reg,
    src: Reg,
) !void {
    if (dst.width().numeric() != src.width().numeric()) {
        return error.WidthMismatch;
    }

    const dst_rex = dst.rex();
    const src_rex = src.rex();
    if (dst_rex != null and src_rex != null and dst_rex != src_rex) {
        return error.IncompatibleArguments;
    }

    if (dst.width() == .word) {
        try code_buf.append(0x66);
    }

    const dst_ex = dst.isExtendedHalf();
    const src_ex = src.isExtendedHalf();
    if (dst_rex == .mandatory or src_rex == .mandatory) {
        try code_buf.append(@bitCast(Rex{
            .w = dst.width() == .qword,
            .b = dst_ex,
            .r = src_ex,
        }));
    }
    try code_buf.append(opcode);
    try code_buf.append(@bitCast(ModRM.register(dst, src)));
}

pub fn binOpRegInOpcodeImm(
    code_buf: *CodeBuf,
    opcode: u8,
    dst: Reg,
    val: i64,
) !void {
    if (dst.width() == .word) {
        try code_buf.append(0x66);
    }
    if (dst.rex() == .mandatory) {
        try code_buf.append(@bitCast(Rex{
            .w = dst.width() == .qword,
            .b = dst.isExtendedHalf(),
        }));
    }
    try code_buf.append(opcode | @as(u8, dst.num()));
    switch (dst.width()) {
        .byte_h, .byte_l => try code_buf.append(@bitCast(@as(i8, @intCast(val)))),
        .word => try code_buf.appendSlice(std.mem.asBytes(&@as(i16, @intCast(val)))),
        .dword => try code_buf.appendSlice(std.mem.asBytes(&@as(i32, @intCast(val)))),
        .qword => try code_buf.appendSlice(std.mem.asBytes(&val)),
    }
}

pub fn movRegReg(code_buf: *CodeBuf, dst: Reg, src: Reg) !void {
    return binOpRegReg(
        code_buf,
        if (dst.width().numeric() == 8) 0x88 else 0x89,
        dst,
        src,
    );
}

pub fn movRegImm(code_buf: *CodeBuf, dst: Reg, val: i64) !void {
    if (dst.width() == .qword) {
        if (std.math.cast(i32, val)) |v| {
            // we can use a shorter sign-extending instruction if we are moving 32 bits into a 64
            // bit register
            try code_buf.append(@bitCast(Rex{
                .w = true,
                .b = dst.isExtendedHalf(),
            }));
            try code_buf.append(0xc7);
            try code_buf.append(@bitCast(ModRM.register(dst, Reg.numbered32(0))));
            try code_buf.appendSlice(std.mem.asBytes(&v));
            return;
        }
    }
    return binOpRegInOpcodeImm(
        code_buf,
        switch (dst.width()) {
            .byte_h, .byte_l => 0xb0,
            .word, .dword, .qword => 0xb8,
        },
        dst,
        val,
    );
}

fn testResultMatches(
    dir: std.fs.Dir,
    comptime expected_fmt: []const u8,
    expected_args: anytype,
    comptime actual_func: anytype,
    func_args: anytype,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var code_buf = std.ArrayListAligned(u8, std.mem.page_size).init(allocator);

    var asm_file = try dir.createFile("code.S", .{});
    defer asm_file.close();
    try asm_file.writeAll(".intel_syntax\n");
    try asm_file.writer().print(expected_fmt, expected_args);

    const clang_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "zig", "cc", "-target", "x86_64-linux-gnu", "-c", "code.S" },
        .cwd_dir = dir,
    });

    if (clang_result.term.Exited == 0) {
        // our assembler should succeed
        const objcopy_result = try std.process.Child.run(.{
            .allocator = allocator,
            .argv = &.{ "zig", "objcopy", "-O", "binary", "code.o", "code.bin" },
            .cwd_dir = dir,
        });
        try std.testing.expectEqual(0, objcopy_result.term.Exited);

        try @call(.auto, actual_func, .{&code_buf} ++ func_args);

        var bin_file = try dir.openFile("code.bin", .{});
        defer bin_file.close();
        var read_buf: [64]u8 = undefined;
        const size = try bin_file.readAll(&read_buf);

        try std.testing.expectEqualSlices(u8, read_buf[0..size], code_buf.items);
    } else {
        // our assembler should fail
        const did_error = if (@call(.auto, actual_func, .{&code_buf} ++ func_args))
            false
        else |_|
            true;
        try std.testing.expect(did_error);
    }
}

test "movRegReg" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const regs = [_]Reg{ .al, .ah, .ax, .eax, .rax, .spl, .r8b, .r8w, .r8d, .r8 };

    for (regs) |r1| {
        for (regs) |r2| {
            try testResultMatches(
                tmp_dir.dir,
                "mov {s}, {s}",
                .{ @tagName(r1), @tagName(r2) },
                movRegReg,
                .{ r1, r2 },
            );
        }
    }
}

test "movRegImm" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const regs = [_]Reg{ .al, .ah, .ax, .eax, .rax, .spl, .r8b, .r8w, .r8d, .r8 };
    const immediates = [_]i64{ 0x01, 0x0123, 0x01234567, 0x0123456789abcdef };
    for (regs) |r| {
        for (immediates) |i| {
            const bits = r.width().numeric();
            if (@clz(i) < 64 - bits) {
                continue;
            }

            try testResultMatches(
                tmp_dir.dir,
                "mov {s}, 0x{x}",
                .{ @tagName(r), i },
                movRegImm,
                .{ r, i },
            );
        }
    }
}
