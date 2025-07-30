const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "jip8",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
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
