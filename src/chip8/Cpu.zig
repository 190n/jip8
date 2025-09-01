const std = @import("std");
const builtin = @import("builtin");

const coroutine = @import("../coroutine.zig");
const StackFrame = coroutine.StackFrame;

const Cpu = @This();

pub const Snapshot = struct {
    pc: void,
    i: u16,
    v: [16]u8,
};

pub const enable_snapshot = builtin.is_test;

/// Portion of the CPU that is used by assembly and must be ABI compatible
pub const Context = extern struct {
    /// Saved stack pointer (from the host while running guest code, or from the guest while running
    /// host code)
    stack_pointer: *anyopaque,
    i: *u8,
    v: [16]u8,
    /// How many guest instructions are left to be executed
    instructions_remaining: u16 = 0,
    did_exit: bool = false,
    memory: [4096]u8,
    canary: switch (builtin.mode) {
        .Debug, .ReleaseSafe => enum(u64) { valid = 0x665f0c30b0317cf4, _ },
        .ReleaseFast, .ReleaseSmall => enum(u0) { valid },
    } = .valid,

    pub fn yield(self: *Context) callconv(.c) *Context {
        self.did_exit = false;
        std.log.info(
            "V = {{" ++ (" {x:0>2}" ** 16) ++ " }}",
            .{
                self.v[0x0], self.v[0x1], self.v[0x2], self.v[0x3],
                self.v[0x4], self.v[0x5], self.v[0x6], self.v[0x7],
                self.v[0x8], self.v[0x9], self.v[0xa], self.v[0xb],
                self.v[0xc], self.v[0xd], self.v[0xe], self.v[0xf],
            },
        );
        std.log.info("I = {x}", .{self.guestI()});
        _ = coroutine.switchStacks(self);
        return self;
    }

    /// Returns I as seen by the guest (a 12-bit offset) instead of as a host pointer
    pub fn guestI(self: *const Context) u12 {
        // TODO intCast -- only truncate to work around store being broken and setting I to be OOB
        return @truncate(@intFromPtr(self.i) - @intFromPtr(&self.memory[0]));
    }
};

guest_stack: []align(@alignOf(StackFrame)) u8,
/// If non-null, reason that the CPU stopped executing code
exit_reason: ?anyerror = null,
random: std.Random.DefaultPrng,
snapshots: if (enable_snapshot) struct {
    base: [*]Snapshot,
    next: [*]Snapshot,
    end: [*]Snapshot,
} else void,
context: Context,

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

pub fn init(
    guest_stack: []align(stack_align) u8,
    code: GuestFunction,
    random_seed: u64,
    snapshots: if (enable_snapshot) []Snapshot else void,
) Cpu {
    var cpu = Cpu{
        .context = .{
            .stack_pointer = frameLocationFromStack(guest_stack),
            .i = undefined,
            .v = undefined,
            .memory = undefined,
        },
        .guest_stack = guest_stack,
        .random = .init(random_seed),
        .snapshots = if (enable_snapshot) .{
            .base = snapshots.ptr,
            .next = snapshots.ptr,
            .end = snapshots.ptr + snapshots.len,
        },
    };
    @memset(&cpu.context.v, 0);
    @memset(&cpu.context.memory, 0);
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
    std.debug.assert(self.context.canary == .valid);
    if (self.context.did_exit) {
        const err = @errorFromInt(retval);
        self.exit_reason = err;
        return err;
    }
}
