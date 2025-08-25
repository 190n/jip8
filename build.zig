const std = @import("std");

const RiscvFloat = enum { none, single, double };
const RiscvBits = enum(u8) { @"32" = 32, @"64" = 64 };

fn resolveRiscvTarget(b: *std.Build, bits: RiscvBits, compressed: bool, float: RiscvFloat) std.Build.ResolvedTarget {
    const cpu = b.fmt(
        "generic_rv{[bits]}+m+a{[compressed]s}{[float]s}",
        .{
            .bits = @intFromEnum(bits),
            .compressed = if (compressed) "+c" else "",
            .float = switch (float) {
                .none => "",
                .single => "+f",
                .double => "+f+d",
            },
        },
    );
    return b.resolveTargetQuery(std.Target.Query.parse(.{
        .arch_os_abi = if (bits == .@"32") "riscv32-linux-gnu" else "riscv64-linux-gnu",
        .cpu_features = cpu,
    }) catch unreachable);
}

pub fn build(b: *std.Build) void {
    const rv32 = b.option(bool, "rv32", "Compile for 32-bit RISC-V instead of 64-bit") orelse false;
    const compressed = !(b.option(bool, "no_c", "Compile without compressed extension") orelse false);
    const float = b.option(RiscvFloat, "float", "Which if any floating-point extension to support") orelse .none;
    const target = resolveRiscvTarget(b, if (rv32) .@"32" else .@"64", compressed, float);
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "jip8",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = false,
        }),
        // https://github.com/ziglang/zig/issues/24621
        .use_llvm = true,
    });

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the emulator");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const test_step = b.step("test", "Run tests");

    for ([_]struct { RiscvBits, bool, RiscvFloat }{
        .{ .@"32", false, .none },
        .{ .@"32", false, .single },
        // TODO .{ .@"32", false, .double },
        .{ .@"32", true, .none },
        .{ .@"32", true, .single },
        // TODO .{ .@"32", true, .double },
        .{ .@"64", false, .none },
        .{ .@"64", false, .single },
        .{ .@"64", false, .double },
        .{ .@"64", true, .none },
        .{ .@"64", true, .single },
        .{ .@"64", true, .double },
    }) |opts| {
        const test_bits, const test_compressed, const test_float = opts;
        const test_target = resolveRiscvTarget(b, test_bits, test_compressed, test_float);
        const tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = test_target,
            }),
        });
        const run_tests = b.addRunArtifact(tests);
        test_step.dependOn(&run_tests.step);
    }
}
