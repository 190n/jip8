const std = @import("std");
const builtin = @import("builtin");
const print = std.fmt.comptimePrint;
const mem = std.mem;

const Cpu = @import("../chip8.zig").Cpu;
const Context = Cpu.Context;
const GuestFunction = Cpu.GuestFunction;

const FloatWidth = enum { double, single };

const float_width: ?FloatWidth = if (std.Target.riscv.featureSetHas(builtin.cpu.features, .d))
    .double
else if (std.Target.riscv.featureSetHas(builtin.cpu.features, .f))
    .single
else
    null;

const float_data_type: u8 = if (float_width) |w| switch (w) {
    .single => 'w',
    .double => 'd',
} else 0;

const saved_int_registers = blk: {
    // TODO ra might not be necessary
    var regs: [13][]const u8 = .{"ra"} ++ .{"invalid"} ** 12;
    for (0..12, regs[1..]) |i, *name| {
        name.* = print("s{}", .{i});
    }
    break :blk regs;
};

const saved_float_registers = blk: {
    var regs: [12][]const u8 = .{"invalid"} ** 12;
    for (0..12, &regs) |i, *name| {
        name.* = print("fs{}", .{i});
    }
    break :blk regs;
};

pub const switchStacks: *const fn (*Context) callconv(.c) u16 = @ptrCast(&switchStacksImpl);
pub const runReturnHere: *const fn () callconv(.c) void = @ptrCast(&runReturnHereImpl);

/// Code shared by switchStacks and runReturnHere
inline fn switchAndRestore() void {
    // swap stack pointers
    asm volatile (print(
            // load new stack pointer out of context
            \\ld a1, {0}(a0)
            // store current stack pointer in context
            \\sd sp, {0}(a0)
            // replace stack pointer with new one
            \\mv sp, a1
            \\
        , .{@offsetOf(Context, "stack_pointer")}));

    // restore all registers
    comptime var offset: usize = 0;
    inline for (saved_int_registers) |register| {
        asm volatile (print(
                "ld {s}, {}(sp)",
                .{ register, offset },
            ));
        offset += 8;
    }

    if (float_width != null) {
        inline for (saved_float_registers) |register| {
            asm volatile (print(
                    "fl{c} {s}, {}(sp)",
                    .{ float_data_type, register, offset },
                ));
            offset += 8;
        }
    }

    asm volatile (print(
            // load the place we are going to return to
            \\ld a2, {}(sp)
            // free the stack space we just restored from
            \\addi sp, sp, {}
        ,
            .{
                @offsetOf(StackFrame, "return_address"),
                @offsetOf(StackFrame, "saved_context_pointer"),
            },
        ));
}

fn switchStacksImpl() callconv(.naked) noreturn {
    // allocate enough stack space for everything we're going to push
    asm volatile (print(
            "addi sp, sp, {}",
            .{-@offsetOf(StackFrame, "saved_context_pointer")},
        ));

    // save all registers
    comptime var offset: usize = 0;
    inline for (saved_int_registers) |register| {
        asm volatile (print(
                "sd {s}, {}(sp)",
                .{ register, offset },
            ));
        offset += 8;
    }

    if (float_width != null) {
        inline for (saved_float_registers) |register| {
            asm volatile (print(
                    "fs{c} {s}, {}(sp)",
                    .{ float_data_type, register, offset },
                ));
            // yes we waste some space if you have RV64F
            offset += 8;
        }
    }

    // return address is saved in another location -- this lets us separate the place we should
    // return to right now (the place where yield() was called) from the content of the ra register
    // in the child context (which always points to runReturnHere)
    asm volatile (print(
            "sd ra, {}(sp)",
            .{@offsetOf(StackFrame, "return_address")},
        ));

    switchAndRestore();

    // return to the address we loaded out of the context
    asm volatile ("jr a2");
}

fn runReturnHereImpl() callconv(.naked) noreturn {
    asm volatile (
    // preserve return value of child function
        \\mv a3, a0
        // retrieve context pointer from stack
        // it is stored right at the stack pointer because it's what the stack pointer pointed to
        // when we entered the child function
        \\ld a0, 0(sp)
    );
    switchAndRestore();

    asm volatile (
    // put the return value of the child function where it's supposed to be
        \\mv a0, a3
        \\jr a2
    );
}

pub const StackFrame = extern struct {
    saved_registers: [num_saved_regs]usize = undefined,
    return_address: GuestFunction,
    /// sp will be pointing here when we start running the child function, so this is the field
    /// that determines our alignment
    saved_context_pointer: ?*Context align(16) = null,

    pub const num_saved_regs = if (float_width == null)
        saved_int_registers.len
    else
        saved_int_registers.len + saved_float_registers.len;

    pub fn init(code: GuestFunction) StackFrame {
        var frame = StackFrame{
            .return_address = code,
        };
        // initialize registers that need it
        inline for (saved_int_registers, 0..) |reg_name, reg_index| {
            if (comptime mem.eql(u8, reg_name, "ra")) {
                frame.saved_registers[reg_index] = @intFromPtr(runReturnHere);
            }
        }
        return frame;
    }
};
