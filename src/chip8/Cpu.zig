const std = @import("std");
const builtin = @import("builtin");

const coroutine = @import("../coroutine.zig");
const StackFrame = coroutine.StackFrame;

const Cpu = @This();

/// Portion of the CPU that is used by assembly and must be ABI compatible
pub const Context = extern struct {
    /// Saved stack pointer (from the host while running guest code, or from the guest while running
    /// host code)
    stack_pointer: *anyopaque,
    /// How many guest instructions are left to be executed
    instructions_remaining: u16 = 0,
    did_exit: bool = false,

    pub fn yield(self: *Context) callconv(.c) *Context {
        self.did_exit = false;
        _ = coroutine.switchStacks(self);
        return self;
    }
};

context: Context,
guest_stack: []align(@alignOf(StackFrame)) u8,
/// If non-null, reason that the CPU stopped executing code
exit_reason: ?anyerror = null,

const stack_align = @alignOf(StackFrame);

/// Signature for the compiled code that will be ran
pub const GuestFunction = *const fn (context: *Context) callconv(.c) u16;

fn frameLocationFromStack(stack: []align(stack_align) u8) *StackFrame {
    return @ptrFromInt(std.mem.alignBackward(
        usize,
        @intFromPtr(stack.ptr) + stack.len - @sizeOf(StackFrame),
        @alignOf(StackFrame),
    ));
}

fn frameLocation(self: *const Cpu) *StackFrame {
    return frameLocationFromStack(self.guest_stack);
}

pub fn init(guest_stack: []align(stack_align) u8, code: GuestFunction) Cpu {
    const cpu = Cpu{
        .context = .{
            .stack_pointer = frameLocationFromStack(guest_stack),
        },
        .guest_stack = guest_stack,
    };
    cpu.frameLocation().* = StackFrame.init(code);
    return cpu;
}

pub fn run(self: *Cpu, instructions: u16) anyerror!void {
    if (self.exit_reason) |e| {
        return e;
    }
    self.frameLocation().saved_context_pointer = &self.context;
    self.context.instructions_remaining = instructions;
    self.context.did_exit = true;
    const retval = coroutine.switchStacks(&self.context);
    if (self.context.did_exit) {
        const err = @errorFromInt(retval);
        self.exit_reason = err;
        return err;
    }
}
