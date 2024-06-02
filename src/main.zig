const std = @import("std");

fn async_fn(ctx: *Context) callconv(.C) i64 {
    const log = std.log.scoped(.child);
    log.info("inside async_fn", .{});
    ctx.yield();
    log.info("after yield", .{});
    return -30;
}

pub fn main() !void {
    const stack = try std.heap.page_allocator.alignedAlloc(u8, std.mem.page_size, 16384);
    defer std.heap.page_allocator.free(stack);

    var ctx = Context.init(stack, async_fn);
    const log = std.log.scoped(.parent);

    const retval = blk: while (true) {
        break :blk ctx.run() orelse {
            log.info("child is still running", .{});
            continue;
        };
    };
    log.info("child returned: {}", .{retval});
}

pub const std_options = std.Options{
    .log_level = .info,
};

const Context = extern struct {
    rsp: *anyopaque,
    did_return: bool = true,
    stack_ptr: [*]align(16) u8,
    stack_size: usize,

    fn frameLocationFromStack(stack: []align(16) u8) *StackFrame {
        return @alignCast(@ptrCast(&stack[stack.len - @sizeOf(StackFrame)]));
    }

    fn stackSlice(self: *const Context) []align(16) u8 {
        return self.stack_ptr[0..self.stack_size];
    }

    fn frameLocation(self: *const Context) *StackFrame {
        return frameLocationFromStack(self.stackSlice());
    }

    pub fn init(stack: []align(16) u8, code: *const fn (*Context) callconv(.C) i64) Context {
        const ctx = Context{
            .rsp = @ptrCast(frameLocationFromStack(stack)),
            .stack_ptr = stack.ptr,
            .stack_size = stack.len,
        };
        ctx.frameLocation().* = StackFrame{
            .return_address = code,
            .final_return_address = runReturnHere,
            .saved_context_ptr = null,
        };
        return ctx;
    }

    pub fn yield(self: *Context) void {
        self.did_return = false;
        _ = switchStacks(self);
    }

    pub fn run(self: *Context) ?i64 {
        self.did_return = true;
        self.frameLocation().saved_context_ptr = self;
        const retval = switchStacks(self);
        if (self.did_return) {
            return retval;
        } else {
            return null;
        }
    }
};

/// Matches the order registers are pushed inside switchStacks()
const StackFrame = extern struct {
    r15: usize = 0,
    r14: usize = 0,
    r13: usize = 0,
    r12: usize = 0,
    rbp: usize = 0,
    rbx: usize = 0,
    /// The address switchStacks() should return to (which is initially the child function)
    return_address: *const fn (*Context) callconv(.C) i64,
    /// The address that the child function should return to when it finishes (does not yield)
    final_return_address: *const fn () callconv(.C) void,
    /// The location of the context struct which is restored by switchStacks when the child function
    /// is returning
    saved_context_ptr: ?*Context,
    /// Padding so that the stack is correctly aligned upon entry to the child function
    _pad: usize = 0,
};

extern fn switchStacks(context: *Context) callconv(.C) i64;
extern fn runReturnHere() callconv(.C) void;

export const context_rsp_offset: usize = @offsetOf(Context, "rsp");

comptime {
    asm (
        \\runReturnHere:
        // when the async function finally returns, instead of yielding, it will return here
        // the stack pointer will be 16 bytes below the top of the stack, pointing to a copy of
        // the context pointer that was saved by run(). we restore that context pointer so that
        // we know what the original stack to switch back into is, and then execute the latter half
        // of switch_stacks() normally (switch to the new stack pointer, pop registers, return).
        \\pop %rdi
        \\jmp finalRestore
        \\switchStacks:
        // push registers to old stack
        \\push %rbx
        \\push %rbp
        \\push %r12
        \\push %r13
        \\push %r14
        \\push %r15
        // swap stacks
        \\finalRestore:
        \\mov context_rsp_offset, %rcx
        \\xchg %rsp, (%rdi,%rcx,1)
        // pop registers from new stack
        \\pop %r15
        \\pop %r14
        \\pop %r13
        \\pop %r12
        \\pop %rbp
        \\pop %rbx
        // return into new code
        \\ret
    );
}

comptime {
    std.testing.refAllDecls(@This());
}
