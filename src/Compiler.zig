const std = @import("std");

const riscv64 = @import("./riscv64.zig");
const x86_64 = @import("./x86_64.zig");
const chip8 = @import("./chip8.zig");
const Context = chip8.Cpu.Context;
const code_buffer = @import("./code_buffer.zig");

pub const Isa = enum {
    x86_64,
    riscv64,
};

const HostFunction = std.meta.DeclEnum(host_functions);

const host_functions = struct {
    pub const check_remaining = &riscvCheckRemaining;
    pub const random = &randomImpl;
};

fn randomImpl(context: *Context) callconv(.c) extern struct { a0: *Context, a1: u8 } {
    const cpu: *chip8.Cpu = @fieldParentPtr("context", context);
    return .{
        .a0 = context,
        .a1 = cpu.random.random().int(u8),
    };
}

fn riscvCheckRemaining() callconv(.naked) noreturn {
    // TODO:
    // assemble this code into the JIT region, and use hostCall to call yield
    // so that the fast path is a jump nearby and only the uncommon case of
    // actually yielding has to read the address and indirect call
    asm volatile (
        \\beqz s1, yield
        \\addi s1, s1, -1
        \\ret
        \\yield:
        \\addi sp, sp, -16
        \\sd ra, 0(sp)
    );

    asm volatile (std.fmt.comptimePrint("sd a1, {}(a0)", .{@offsetOf(Context, "i")}));
    inline for (0..16) |vx| {
        const host = comptime Compiler(.riscv64).hostRegFromV(@intCast(vx));
        asm volatile (std.fmt.comptimePrint(
                "sb {s}, {}(a0)",
                .{ @tagName(host), vx + @offsetOf(Context, "v") },
            ));
    }

    asm volatile ("call %[yield]"
        :
        : [yield] "X" (&Context.yield),
    );

    asm volatile (std.fmt.comptimePrint("ld a1, {}(a0)", .{@offsetOf(Context, "i")}));
    asm volatile (std.fmt.comptimePrint("lhu s1, {}(a0)", .{@offsetOf(Context, "instructions_remaining")}));
    inline for (0..16) |vx| {
        const host = comptime Compiler(.riscv64).hostRegFromV(@intCast(vx));
        asm volatile (std.fmt.comptimePrint(
                "lbu {s}, {}(a0)",
                .{ @tagName(host), vx + @offsetOf(Context, "v") },
            ));
    }

    asm volatile (
        \\ld ra, 0(sp)
        \\addi sp, sp, 16
        \\ret
    );
}

const HostFunctionTrampolines = struct {
    /// Contains the actual code but null function pointers. The function pointers must be written
    /// here using `pointer_offsets` before the code can be called.
    code_template: []const u8,
    /// Tells the size of each function pointer stored at the beginning of code_template
    pointer_size: enum(u8) { @"32" = 32, @"64" = 64 },
    /// Tells the position where the code for function calls starts
    first_code_offset: usize,
    /// Tells the size of each piece of code to call a function
    each_code_size: usize,

    /// Tells where the code which calls a given host function begins
    pub fn codeOffset(self: *const HostFunctionTrampolines, func: HostFunction) usize {
        return self.first_code_offset + @intFromEnum(func) * self.each_code_size;
    }

    /// Tells where the function pointer for a given host function begins
    pub fn pointerOffset(self: *const HostFunctionTrampolines, func: HostFunction) usize {
        return @intFromEnum(func) * self.pointer_size;
    }

    /// Write completed trampoline code, including the correct function pointers, into buf.
    /// buf must be the same size as self.code_template.
    pub fn write(self: *const HostFunctionTrampolines, buf: []u8) void {
        @memcpy(buf, self.code_template);
        inline for (@typeInfo(host_functions).@"struct".decls) |decl| {
            const id: usize = @intFromEnum(@field(HostFunction, decl.name));
            switch (self.pointer_size) {
                inline else => |pointer_size| {
                    const T = switch (pointer_size) {
                        .@"32" => u32,
                        .@"64" => u64,
                    };
                    std.mem.writeInt(
                        T,
                        buf[@sizeOf(T) * id ..][0..@sizeOf(T)],
                        @intCast(@intFromPtr(@field(host_functions, decl.name))),
                        .little,
                    );
                },
            }
        }
    }
};

const x86_64_trampolines = @compileError("todo");

fn makeRiscv64Trampolines(comptime compressed: bool) !HostFunctionTrampolines {
    var code: [4096]u8 = undefined;
    var writer: std.io.Writer = .fixed(&code);
    var assembler = riscv64.Assembler.init(
        &writer,
        std.Target.riscv.featureSet(if (compressed)
            &.{ .@"64bit", .i, .c }
        else
            &.{ .@"64bit", .i }),
    );

    var pointer_offsets: std.EnumArray(HostFunction, isize) = undefined;

    for (@typeInfo(HostFunction).@"enum".fields) |field| {
        pointer_offsets.set(@enumFromInt(field.value), @intCast(writer.buffered().len));
        try writer.splatByteAll(0, 8);
    }

    const first_code_offset = writer.buffered().len;
    var each_code_size: ?usize = null;

    for (@typeInfo(HostFunction).@"enum".fields) |field| {
        const pointer_offset = pointer_offsets.get(@enumFromInt(field.value));
        const pc_offset_before = writer.buffered().len;
        defer {
            const size = writer.buffered().len - pc_offset_before;
            if (each_code_size) |known_size| {
                std.debug.assert(size == known_size);
            } else {
                each_code_size = size;
            }
        }

        const offset_to_function_pointer = pointer_offset - @as(isize, @intCast(writer.buffered().len));
        // get current PC in t0
        try assembler.auipc(.t0, 0);
        // load the function pointer into t0 using an offset from the PC
        try assembler.ld(.t0, @intCast(offset_to_function_pointer), .t0);
        // jump to the function
        try assembler.jr(.t0, 0);
    }

    const constant_code: [writer.buffered().len]u8 = code[0..writer.buffered().len].*;

    return .{
        .code_template = &constant_code,
        .pointer_size = .@"64",
        .first_code_offset = first_code_offset,
        .each_code_size = each_code_size.?,
    };
}

const riscv64_trampolines = makeRiscv64Trampolines(false) catch unreachable;
const riscv64_c_trampolines = makeRiscv64Trampolines(true) catch unreachable;

pub fn Compiler(comptime isa: Isa) type {
    return struct {
        code_buffer: code_buffer.Any,
        assembler: Assembler,
        trampolines: *const HostFunctionTrampolines,

        const Self = @This();
        const Assembler = switch (isa) {
            .x86_64 => unreachable,
            .riscv64 => riscv64.Assembler,
        };

        pub fn init(self: *Self, allocator: std.mem.Allocator, feature_set: std.Target.Cpu.Feature.Set) !void {
            self.* = .{
                .code_buffer = .{ .writable = .init(allocator) },
                .assembler = undefined,
                .trampolines = undefined,
            };
            self.assembler = .init(&self.code_buffer.writable.interface, feature_set);
            self.trampolines = if (comptime isa == .riscv64)
                (if (self.assembler.hasCompressed())
                    &riscv64_c_trampolines
                else
                    &riscv64_trampolines)
            else
                comptime unreachable;
            try self.code_buffer.writable.interface.writeAll(self.trampolines.code_template);
            self.trampolines.write(self.code_buffer.writable.list.items);
        }

        pub fn prologue(self: *Self) !void {
            const a = &self.assembler;
            try a.addi(.sp, .sp, -16);
            try a.sd(.ra, 0, .sp);
            try a.lhu(.s1, @offsetOf(Context, "instructions_remaining"), .a0);

            // clear all V registers
            for (0..16) |vx| {
                try a.li(hostRegFromV(@intCast(vx)), 0);
            }

            // clear I
            try a.addi(i_reg, ctx_reg, @offsetOf(Context, "memory"));
        }

        fn callHost(self: *Self, function: HostFunction) !void {
            const trampoline_code_offset: isize = @intCast(self.trampolines.codeOffset(function));
            const caller_offset: isize = @intCast(self.code_buffer.writable.list.items.len);
            return self.assembler.jal(.ra, @intCast(trampoline_code_offset - caller_offset));
        }

        fn hostRegFromV(guest_register: u4) switch (isa) {
            .riscv64 => riscv64.Register,
            else => unreachable,
        } {
            return @enumFromInt(@as(u5, guest_register) + 16);
        }

        const ctx_reg = riscv64.Register.a0;
        const i_reg = riscv64.Register.a1;

        pub fn compile(self: *Self, instruction: chip8.Instruction) !void {
            const a = &self.assembler;
            try self.callHost(.check_remaining);
            switch (instruction.decode()) {
                .set_register => |ins| {
                    const vx, const nn = ins;
                    try a.li(hostRegFromV(vx), nn);
                },
                .add_immediate => |ins| {
                    const vx, const nn = ins;
                    try a.addi(hostRegFromV(vx), hostRegFromV(vx), nn);
                    try a.andi(hostRegFromV(vx), hostRegFromV(vx), 0xff);
                },
                .set_register_to_register => |ins| {
                    const vx, const vy = ins;
                    try a.mv(hostRegFromV(vx), hostRegFromV(vy));
                },
                .set_i => |i_val| {
                    const offset_from_ctx: u16 = i_val + @as(u16, @intCast(@offsetOf(Context, "memory")));
                    if (std.math.cast(i12, offset_from_ctx)) |small_imm| {
                        try a.addi(i_reg, ctx_reg, small_imm);
                    } else {
                        try a.li(.t0, offset_from_ctx);
                        try a.add(i_reg, ctx_reg, .t0);
                    }
                },
                .store => |up_to| {
                    // TODO wrap I around
                    const reg_count = @as(u8, up_to) + 1;
                    for (0..reg_count) |vx_usize| {
                        const vx: u4 = @intCast(vx_usize);
                        try a.sb(hostRegFromV(vx), vx, i_reg);
                    }
                    try a.addi(i_reg, i_reg, reg_count);
                },
                .load => |up_to| {
                    // TODO wrap I around
                    const reg_count = @as(u8, up_to) + 1;
                    for (0..reg_count) |vx_usize| {
                        const vx: u4 = @intCast(vx_usize);
                        try a.lbu(hostRegFromV(vx), vx, i_reg);
                    }
                    try a.addi(i_reg, i_reg, reg_count);
                },
                .random => |ins| {
                    const dst_reg, const mask = ins;
                    try a.addi(.sp, .sp, -16);
                    try a.sd(.a1, 0, .sp);
                    for ([_]riscv64.Register{ .a6, .a7, .t3, .t4, .t5, .t6 }, 0..) |reg, i| {
                        try a.sb(reg, @intCast(8 + i), .sp);
                    }
                    try self.callHost(.random);
                    for ([_]riscv64.Register{ .a6, .a7, .t3, .t4, .t5, .t6 }, 0..) |reg, i| {
                        try a.lbu(reg, @intCast(8 + i), .sp);
                    }
                    try a.andi(hostRegFromV(dst_reg), .a1, mask);
                    try a.ld(.a1, 0, .sp);
                    try a.addi(.sp, .sp, 16);
                },

                .invalid => |opcode| {
                    try a.li(.t0, @intFromEnum(opcode));
                    try a.ebreak();
                },

                .clear,
                .ret,
                .jump,
                .call,
                .skip_if_equal,
                .skip_if_not_equal,
                .skip_if_registers_equal,
                .bitwise_or,
                .bitwise_and,
                .bitwise_xor,
                .add_registers,
                .sub_registers,
                .shift_right,
                .sub_registers_reverse,
                .shift_left,
                .skip_if_registers_not_equal,
                .jump_v0,
                .draw,
                .skip_if_pressed,
                .skip_if_not_pressed,
                .read_dt,
                .wait_for_key,
                .set_dt,
                .set_st,
                .increment_i,
                .set_i_to_font,
                .store_bcd,
                => {
                    std.log.scoped(.compiler).warn("unimplemented chip-8 instruction: {x:0>4} ({s})", .{ @intFromEnum(instruction), @tagName(instruction.decode()) });
                },
            }
        }

        pub fn epilogue(self: *Self) !void {
            try self.assembler.ld(.ra, 0, .sp);
            try self.assembler.ld(.s0, 8, .sp);
            try self.assembler.addi(.sp, .sp, 16);
            try self.assembler.li(.a0, @intFromError(error.HelloRiscv64));
            try self.assembler.ret();
        }

        pub fn makeExecutable(self: *Self) !void {
            _ = try self.code_buffer.makeExecutable(&.{});
        }

        pub fn entrypoint(self: *const Self) chip8.Cpu.GuestFunction {
            return self.code_buffer.executable.entrypoint(chip8.Cpu.GuestFunction, self.trampolines.code_template.len);
        }

        pub fn deinit(self: *Self) !void {
            if (self.code_buffer != .writable) _ = try self.code_buffer.makeWritable();
            self.code_buffer.writable.deinit();
        }
    };
}
