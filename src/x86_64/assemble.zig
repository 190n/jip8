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

pub fn movRegReg(code_buf: *CodeBuf, dst: Reg, src: Reg) !void {
    return binOpRegReg(
        code_buf,
        if (dst.width().numeric() == 8) 0x88 else 0x89,
        dst,
        src,
    );
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
