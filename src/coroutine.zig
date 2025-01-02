const Context = @import("./chip8.zig").Cpu.Context;
const builtin = @import("builtin");

pub extern fn switchStacks(context: *Context) callconv(.C) u16;
pub extern fn runReturnHere() callconv(.C) void;

fn unimplementedSwitchStacks(_: *Context) callconv(.c) u16 {
    @panic("unimplemented");
}

fn unimplementedRunReturnHere() callconv(.c) void {
    @panic("unimplemented");
}

comptime {
    switch (builtin.cpu.arch) {
        .x86_64, .riscv64 => {},
        else => {
            @export(&unimplementedSwitchStacks, .{ .name = "switchStacks" });
            @export(&unimplementedRunReturnHere, .{ .name = "runReturnHere" });
        },
    }
}

pub const StackFrame = switch (builtin.cpu.arch) {
    .x86_64 => @import("./x86_64/switch.zig").StackFrame,
    .riscv64 => @import("./riscv64/switch.zig").StackFrame,
    else => extern struct {},
};
