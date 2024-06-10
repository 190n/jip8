const std = @import("std");
const builtin = @import("builtin");

fn asyncFn(ctx: *Context) callconv(.C) i64 {
    const log = std.log.scoped(.child);
    log.info("inside asyncFn", .{});
    ctx.yield();
    log.info("after yield", .{});
    return 12345;
}

pub fn main() !void {
    const stack = try std.heap.page_allocator.alignedAlloc(u8, std.mem.page_size, 16384);
    defer std.heap.page_allocator.free(stack);

    var ctx = Context.init(stack, asyncFn);
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

const stack_align = builtin.target.stackAlignment();

pub const Context = extern struct {
    stack_pointer: *anyopaque,
    stack_base_address: [*]align(stack_align) u8,
    stack_size: u32,
    did_return: bool = true,

    fn frameLocationFromStack(stack: []align(stack_align) u8) *StackFrame {
        return @ptrFromInt(std.mem.alignBackward(
            usize,
            @intFromPtr(stack.ptr) + stack.len - @sizeOf(StackFrame),
            stack_align,
        ));
    }

    fn stackSlice(self: *const Context) []align(stack_align) u8 {
        return self.stack_base_address[0..self.stack_size];
    }

    fn frameLocation(self: *const Context) *StackFrame {
        return frameLocationFromStack(self.stackSlice());
    }

    pub fn init(stack: []align(stack_align) u8, code: *const fn (*Context) callconv(.C) i64) Context {
        const ctx = Context{
            .stack_pointer = @ptrCast(frameLocationFromStack(stack)),
            .stack_base_address = stack.ptr,
            .stack_size = @intCast(stack.len),
        };
        ctx.frameLocation().* = StackFrame.init(code);
        return ctx;
    }

    pub fn yield(self: *Context) void {
        self.did_return = false;
        _ = switchStacks(self);
    }

    pub fn run(self: *Context) ?i64 {
        self.did_return = true;
        self.frameLocation().saved_context_pointer = self;
        const retval = switchStacks(self);
        if (self.did_return) {
            return retval;
        } else {
            return null;
        }
    }
};

const num_saved_regs = switch (builtin.target.cpu.arch) {
    .x86_64 => 6, // rbx, rbp, r12-r15
    .riscv64 => 27, // gp, tp, s0-s11, fs0-fs11, ra
    else => |a| @compileError("unsupported architecture: " ++ @tagName(a)),
};

/// Matches the order registers are pushed inside switchStacks()
const StackFrame = switch (builtin.target.cpu.arch) {
    .x86_64 => @import("./x86_64/switch.zig").StackFrame,
    .riscv64 => @import("./riscv64/switch.zig").StackFrame,
    else => unreachable,
};

extern fn switchStacks(context: *Context) callconv(.C) i64;
extern fn runReturnHere() callconv(.C) void;

comptime {
    std.debug.assert(@offsetOf(Context, "stack_pointer") == 0);
}

comptime {
    std.testing.refAllDecls(@This());
}
