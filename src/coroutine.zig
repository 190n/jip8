const Context = @import("./chip8.zig").Cpu.Context;
const builtin = @import("builtin");

const x86_64 = @import("./x86_64/switch.zig");
const riscv = @import("./riscv/switch.zig");

const impl = switch (builtin.cpu.arch) {
    .x86_64 => x86_64,
    .riscv32, .riscv64 => riscv,
    else => struct {
        pub const switchStacks = &unimplementedSwitchStacks;
        pub const runReturnHere = &unimplementedRunReturnHere;
        pub const StackFrame = extern struct {};
    },
};

pub const switchStacks = impl.switchStacks;
pub const runReturnHere = impl.runReturnHere;
pub const StackFrame = impl.StackFrame;

fn unimplementedSwitchStacks(_: *Context) callconv(.c) u16 {
    @panic("unimplemented");
}

fn unimplementedRunReturnHere() callconv(.c) void {
    @panic("unimplemented");
}
