const std = @import("std");

pub fn build(b: *std.Build) void {
    const rv32 = b.option(bool, "rv32", "Compile for 32-bit RISC-V instead of 64-bit") orelse false;
    const no_c = b.option(bool, "no_c", "Compile without compressed extension") orelse false;
    const float = b.option(enum { none, single, double }, "float", "Which if any floating-point extension to support") orelse .none;
    const cpu = b.fmt(
        "generic_rv{[bits]}+m+a{[compressed]s}{[float]s}",
        .{
            .bits = @as(u8, if (rv32) 32 else 64),
            .compressed = if (no_c) "" else "+c",
            .float = switch (float) {
                .none => "",
                .single => "+f",
                .double => "+f+d",
            },
        },
    );
    const target = b.resolveTargetQuery(std.Target.Query.parse(.{
        .arch_os_abi = if (rv32) "riscv32-linux-gnu" else "riscv64-linux-gnu",
        .cpu_features = cpu,
    }) catch unreachable);
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

    for ([_]std.Target.Query.ParseOptions{
        .{ .arch_os_abi = "x86_64-linux-gnu" },
        .{ .arch_os_abi = "riscv64-linux-gnu", .cpu_features = "baseline_rv64" },
        .{ .arch_os_abi = "riscv64-linux-gnu", .cpu_features = "baseline_rv64+c" },
    }) |opts| {
        const test_target = b.resolveTargetQuery(std.Target.Query.parse(opts) catch unreachable);
        const tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = test_target,
            }),
            // https://github.com/ziglang/zig/issues/24621
            .use_llvm = true,
        });
        const run_tests = b.addRunArtifact(tests);
        test_step.dependOn(&run_tests.step);
    }
}
