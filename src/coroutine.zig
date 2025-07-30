const Context = @import("./chip8.zig").Cpu.Context;
const builtin = @import("builtin");

const x86_64 = @import("./x86_64/switch.zig");
const riscv64 = @import("./riscv64/switch.zig");

pub const switchStacks = switch (builtin.cpu.arch) {
    .x86_64 => x86_64.switchStacks,
    .riscv64 => riscv64.switchStacks,
    else => unimplementedSwitchStacks,
};
pub const runReturnHere = switch (builtin.cpu.arch) {
    .x86_64 => x86_64.runReturnHere,
    .riscv64 => riscv64.runReturnHere,
    else => unimplementedRunReturnHere,
};

fn unimplementedSwitchStacks(_: *Context) callconv(.c) u16 {
    @panic("unimplemented");
}

fn unimplementedRunReturnHere() callconv(.c) void {
    @panic("unimplemented");
}

pub const StackFrame = switch (builtin.cpu.arch) {
    .x86_64 => @import("./x86_64/switch.zig").StackFrame,
    .riscv64 => @import("./riscv64/switch.zig").StackFrame,
    else => extern struct {},
};
