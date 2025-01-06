const std = @import("std");
const print = std.fmt.comptimePrint;

const Cpu = @import("../chip8.zig").Cpu;
const Context = Cpu.Context;
const GuestFunction = Cpu.GuestFunction;

const runReturnHere = @import("../coroutine.zig").runReturnHere;

pub const StackFrame = stack_frame: {
    const saved_registers = switch (@import("builtin").target.os.tag) {
        .linux => .{ "rbx", "rbp", "r12", "r13", "r14", "r15" },
        else => |os| @compileError(print("unsupported OS: {s}", .{@tagName(os)})),
    };

    const StackFrameInner = extern struct {
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

    var save_code: []const u8 = "";
    var restore_code: []const u8 = "";

    for (saved_registers) |register| {
        save_code = save_code ++ print(
            "push {s}\n",
            .{register},
        );
        // restores go in opposite order
        restore_code = print(
            "pop {s}\n",
            .{register},
        ) ++ restore_code;
    }

    const switch_code = print(
        "xchg rsp, qword ptr [rdi + {}]\n",
        .{@offsetOf(Context, "stack_pointer")},
    );

    asm (
        \\.intel_syntax
        \\switchStacks:
        \\
        ++ save_code ++
            // mark this location so we can jump to it from runReturnHere
            \\finalRestore:
            \\
        ++ switch_code ++
            restore_code ++
            "ret");

    asm (
        \\.intel_syntax
        \\runReturnHere:
        // when the async function finally returns, instead of yielding, it will return here
        // the stack pointer will be 16 bytes below the top of the stack, pointing to a copy of
        // the context pointer that was saved by run(). we restore that context pointer so that
        // we know what the original stack to switch back into is, and then execute the latter half
        // of switch_stacks() normally (switch to the new stack pointer, pop registers, return).
        \\pop rdi
        \\jmp finalRestore
        \\
    );

    break :stack_frame StackFrameInner;
};
