const Context = @import("./chip8.zig").Cpu.Context;

pub extern fn switchStacks(context: *Context) callconv(.C) u16;
pub extern fn runReturnHere() callconv(.C) void;

pub const StackFrame = switch (@import("builtin").cpu.arch) {
    .x86_64 => @import("./x86_64/switch.zig").StackFrame,
    .riscv64 => @import("./riscv64/switch.zig").StackFrame,
    else => unreachable,
};
