const std = @import("std");
const builtin = @import("builtin");
const print = std.fmt.comptimePrint;
const mem = std.mem;

const Cpu = @import("../chip8.zig").Cpu;
const Context = Cpu.Context;
const GuestFunction = Cpu.GuestFunction;

const runReturnHere = @import("../coroutine.zig").runReturnHere;

const FloatWidth = enum { double, single };

const float_width: ?FloatWidth = if (std.Target.riscv.featureSetHas(builtin.cpu.features, .d))
    .double
else if (std.Target.riscv.featureSetHas(builtin.cpu.features, .f))
    .single
else
    null;

pub const StackFrame = blk: {
    var saved_int_registers: [13][]const u8 = .{"ra"} ++ .{"invalid"} ** 12;
    var saved_float_registers: [12][]const u8 = .{"invalid"} ** 12;

    const StackFrameInner = extern struct {
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
                    frame.saved_registers[reg_index] = @intFromPtr(&runReturnHere);
                }
            }
            return frame;
        }
    };

    // generate the names of all twelve "standard" saved registers for integer and float
    // (s0-s11, fs0-fs11)
    for (0..12, saved_int_registers[1..], &saved_float_registers) |i, *int_reg, *float_reg| {
        int_reg.* = print("s{}", .{i});
        float_reg.* = print("fs{}", .{i});
    }

    var save_code: []const u8 = print(
        // allocate enough stack space for everything we're going to push
        "addi sp, sp, {}\n",
        .{-@offsetOf(StackFrameInner, "saved_context_pointer")},
    );
    var restore_code: []const u8 = "";
    var offset: usize = 0;

    for (saved_int_registers) |register| {
        save_code = save_code ++ print(
            "sd {s}, {}(sp)\n",
            .{ register, offset },
        );
        restore_code = restore_code ++ print(
            "ld {s}, {}(sp)\n",
            .{ register, offset },
        );
        offset += 8;
    }

    if (float_width) |w| {
        const data_type: u8 = switch (w) {
            .single => 'w',
            .double => 'd',
        };
        for (saved_float_registers) |register| {
            save_code = save_code ++ print(
                "fs{c} {s}, {}(sp)\n",
                .{ data_type, register, offset },
            );
            restore_code = restore_code ++ print(
                "fl{c} {s}, {}(sp)\n",
                .{ data_type, register, offset },
            );
            offset += 8;
        }
    }

    // return address is saved in another location -- this lets us separate the place we should
    // return to right now (the place where yield() was called) from the content of the ra register
    // in the child context (which always points to runReturnHere)
    save_code = save_code ++ print(
        "sd ra, {}(sp)\n",
        .{@offsetOf(StackFrameInner, "return_address")},
    );
    restore_code = restore_code ++ print(
        \\
        // load the place we are going to return to
        \\ld a2, {}(sp)
        // free the stack space we just restored from
        \\addi sp, sp, {}
        \\
    ,
        .{
            @offsetOf(StackFrameInner, "return_address"),
            @offsetOf(StackFrameInner, "saved_context_pointer"),
        },
    );

    const switch_code = print(
        \\
        // load new stack pointer out of context
        \\ld a1, {0}(a0)
        // store current stack pointer in context
        \\sd sp, {0}(a0)
        // replace stack pointer with new one
        \\mv sp, a1
        \\
    , .{@offsetOf(Context, "stack_pointer")});

    asm (
        \\switchStacks:
        \\
        ++ save_code ++
            switch_code ++
            restore_code ++
            // return to the address we loaded out of the context
            "jr a2");

    asm (
        \\runReturnHere:
        // preserve return value of child function
        \\mv a3, a0
        // retrieve context pointer from stack
        // it is stored right at the stack pointer because it's what the stack pointer pointed to
        // when we entered the child function
        \\ld a0, 0(sp)
        \\
        ++ switch_code ++ restore_code ++
            // put the return value of the child function where it's supposed to be
            \\mv a0, a3
            \\jr a2
            \\
    );

    break :blk StackFrameInner;
};
