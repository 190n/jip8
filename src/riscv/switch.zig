const std = @import("std");
const builtin = @import("builtin");
const print = std.fmt.comptimePrint;
const mem = std.mem;

const Cpu = @import("../chip8.zig").Cpu;
const Context = Cpu.Context;
const GuestFunction = Cpu.GuestFunction;
const riscv = @import("../riscv.zig");

const float_width: ?riscv.FloatWidth = if (std.Target.riscv.featureSetHas(builtin.cpu.features, .d))
    .d
else if (std.Target.riscv.featureSetHas(builtin.cpu.features, .f))
    .f
else
    null;

comptime {
    if (float_width == .d and builtin.cpu.arch == .riscv32) @compileError("unsupported");
}

const float_data_type: u8 = if (float_width) |w| switch (w) {
    .f => 'w',
    .d => 'd',
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

const load: []const u8 = switch (builtin.cpu.arch) {
    .riscv32 => "lw",
    .riscv64 => "ld",
    else => unreachable,
};

const store: []const u8 = switch (builtin.cpu.arch) {
    .riscv32 => "sw",
    .riscv64 => "sd",
    else => unreachable,
};

const register_size_bytes: usize = switch (builtin.cpu.arch) {
    .riscv32 => 4,
    .riscv64 => 8,
    else => unreachable,
};

/// Code shared by switchStacks and runReturnHere
inline fn switchAndRestore() void {
    // swap stack pointers
    asm volatile (print(
            // load new stack pointer out of context
            \\{[load]s} a1, {[offset]}(a0)
            // store current stack pointer in context
            \\{[store]s} sp, {[offset]}(a0)
            // replace stack pointer with new one
            \\mv sp, a1
            \\
        , .{ .offset = @offsetOf(Context, "stack_pointer"), .load = load, .store = store }));

    // restore all registers
    comptime var offset: usize = 0;
    inline for (saved_int_registers) |register| {
        asm volatile (print(
                "{[load]s} {[register]s}, {[offset]}(sp)",
                .{ .load = load, .register = register, .offset = offset },
            ));
        offset += register_size_bytes;
    }

    if (float_width != null) {
        inline for (saved_float_registers) |register| {
            asm volatile (print(
                    "fl{c} {s}, {}(sp)",
                    .{ float_data_type, register, offset },
                ));
            offset += register_size_bytes;
        }
    }

    asm volatile (print(
            // load the place we are going to return to
            \\{[load]s} a2, {[ret_offset]}(sp)
            // free the stack space we just restored from
            \\addi sp, sp, {[ctx_offset]}
        ,
            .{
                .load = load,
                .ret_offset = @offsetOf(StackFrame, "return_address"),
                .ctx_offset = @offsetOf(StackFrame, "saved_context_pointer"),
            },
        ));
}

fn switchStacksImpl() callconv(.naked) void {
    // allocate enough stack space for everything we're going to push
    asm volatile (print(
            "addi sp, sp, {}",
            .{-@offsetOf(StackFrame, "saved_context_pointer")},
        ));

    // save all registers
    comptime var offset: usize = 0;
    inline for (saved_int_registers) |register| {
        asm volatile (print(
                "{[store]s} {[register]s}, {[offset]}(sp)",
                .{ .store = store, .register = register, .offset = offset },
            ));
        offset += register_size_bytes;
    }

    if (float_width != null) {
        inline for (saved_float_registers) |register| {
            asm volatile (print(
                    "fs{c} {s}, {}(sp)",
                    .{ float_data_type, register, offset },
                ));
            // yes we waste some space if you have only 32-bit floats
            offset += 8;
        }
    }

    // return address is saved in another location -- this lets us separate the place we should
    // return to right now (the place where yield() was called) from the content of the ra register
    // in the child context (which always points to runReturnHere)
    asm volatile (print(
            "{[store]s} ra, {[offset]}(sp)",
            .{ .store = store, .offset = @offsetOf(StackFrame, "return_address") },
        ));

    switchAndRestore();

    // return to the address we loaded out of the context
    asm volatile ("jr a2");
}

fn runReturnHereImpl() callconv(.naked) void {
    asm volatile (print(
            // preserve return value of child function
            \\mv a3, a0
            // retrieve context pointer from stack
            // it is stored right at the stack pointer because it's what the stack pointer pointed to
            // when we entered the child function
            \\ {[load]s} a0, 0(sp)
        ,
            .{ .load = load },
        ));
    switchAndRestore();

    asm volatile (
    // put the return value of the child function where it's supposed to be
        \\mv a0, a3
        \\jr a2
    );
}

pub const StackFrame = extern struct {
    // TODO support RV32D (this array will need more space for float registers than int registers)
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
