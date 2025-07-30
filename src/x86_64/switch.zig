const std = @import("std");
const print = std.fmt.comptimePrint;

const Cpu = @import("../chip8.zig").Cpu;
const Context = Cpu.Context;
const GuestFunction = Cpu.GuestFunction;

const saved_registers = switch (@import("builtin").target.os.tag) {
    .linux => [_][]const u8{ "rbx", "rbp", "r12", "r13", "r14", "r15" },
    else => |os| @compileError(print("unsupported OS: {s}", .{@tagName(os)})),
};

pub const switchStacks: *const fn (*Context) callconv(.c) u16 = @ptrCast(&switchStacksImpl);
pub const runReturnHere: *const fn () callconv(.c) void = @ptrCast(&runReturnHereImpl);

fn switchStacksImpl() callconv(.naked) noreturn {
    inline for (saved_registers) |register| {
        asm volatile ("pushq %%" ++ register);
    }

    finalRestore();
}

inline fn finalRestore() void {
    asm volatile (print(
            "xchgq {}(%%rdi), %%rsp",
            .{@offsetOf(Context, "stack_pointer")},
        ));
    comptime var reversed = saved_registers;
    comptime std.mem.reverse([]const u8, &reversed);
    inline for (reversed) |register| {
        asm volatile ("popq %%" ++ register);
    }
    asm volatile ("ret");
}

fn runReturnHereImpl() callconv(.naked) noreturn {
    // when the async function finally returns, instead of yielding, it will return here
    // the stack pointer will be 16 bytes below the top of the stack, pointing to a copy of
    // the context pointer that was saved by run(). we restore that context pointer so that
    // we know what the original stack to switch back into is, and then execute the latter half
    // of switch_stacks() normally (switch to the new stack pointer, pop registers, return).
    asm volatile ("popq %%rdi");
    finalRestore();
}

pub const StackFrame = extern struct {
    saved_registers: [num_saved_regs]usize = undefined,
    /// The address switchStacks() should return to (which is initially the child function)
    return_address: GuestFunction,
    /// The address that the child function should return to when it finishes (does not yield)
    final_return_address: *const fn () callconv(.c) void,
    /// The location of the context struct which is restored by switchStacks when the child function
    /// is returning. Alignment is set to ensure correct x86_64 alignment as if the child function
    /// were an ordinary call -- this field would have been at the stack pointer before the call,
    /// with a 16-byte alignment, so the final return address we push will be off by 8.
    saved_context_pointer: ?*Context align(16) = null,

    pub const num_saved_regs = saved_registers.len;

    pub fn init(code: GuestFunction) StackFrame {
        return .{
            .return_address = code,
            .final_return_address = &runReturnHere,
        };
    }
};
