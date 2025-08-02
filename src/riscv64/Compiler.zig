const std = @import("std");

const riscv64 = @import("../riscv64.zig");
const chip8 = @import("../chip8.zig");
const Context = chip8.Cpu.Context;
const code_buffer = @import("../code_buffer.zig");

const HostFunction = std.meta.DeclEnum(host_functions);

code: code_buffer.Any,
assembler: riscv64.Assembler,
trampolines: *const HostFunctionTrampolines,

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

fn riscvCheckRemaining() callconv(.naked) void {
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
        const host = comptime hostRegFromV(@intCast(vx));
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
        const host = comptime hostRegFromV(@intCast(vx));
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

const Compiler = @This();

const RegisterScope = struct {
    compiler: *Compiler,
    v_regs_saved: bool = false,
    i_saved: bool = false,
    temp_regs_used: u8 = 0,

    pub fn init(compiler: *Compiler) RegisterScope {
        return .{ .compiler = compiler };
    }

    pub fn tempReg(self: *RegisterScope) riscv64.Register {
        defer self.temp_regs_used += 1;
        return switch (self.temp_regs_used) {
            0 => .t0,
            1 => .t1,
            2 => .t2,
            else => unreachable, // handle this by spilling something to the stack
        };
    }

    pub fn saveVRegsForHostCall(self: *RegisterScope) !void {
        self.v_regs_saved = true;
        for (0..16) |vx| {
            const host = hostRegFromV(@intCast(vx));
            if (!host.isSaved()) {
                try self.compiler.assembler.sb(host, @intCast(vx + @offsetOf(Context, "v")), ctx_reg);
            }
        }
    }

    pub fn saveIForHostCall(self: *RegisterScope) !void {
        self.i_saved = true;
        try self.compiler.assembler.sd(i_reg, @intCast(@offsetOf(Context, "i")), ctx_reg);
    }

    pub fn restoreV(self: *RegisterScope) !void {
        if (self.v_regs_saved) {
            self.v_regs_saved = false;
            for (0..16) |vx| {
                const host = hostRegFromV(@intCast(vx));
                if (!host.isSaved()) {
                    try self.compiler.assembler.lbu(host, @intCast(vx + @offsetOf(Context, "v")), ctx_reg);
                }
            }
        }
    }

    pub fn restoreI(self: *RegisterScope) !void {
        if (self.i_saved) {
            self.i_saved = false;
            try self.compiler.assembler.ld(i_reg, @intCast(@offsetOf(Context, "i")), ctx_reg);
        }
    }

    pub fn isRestored(self: *const RegisterScope) bool {
        return !self.v_regs_saved and !self.i_saved;
    }
};

pub fn init(self: *Compiler, allocator: std.mem.Allocator, feature_set: std.Target.Cpu.Feature.Set) !void {
    self.* = .{
        .code = .{ .writable = .init(allocator) },
        .assembler = undefined,
        .trampolines = undefined,
    };
    self.assembler = .init(&self.code.writable.interface, feature_set);
    self.trampolines = if (self.assembler.hasCompressed())
        &riscv64_c_trampolines
    else
        &riscv64_trampolines;
    try self.code.writable.interface.writeAll(self.trampolines.code_template);
    self.trampolines.write(self.code.writable.list.items);
}

pub fn prologue(self: *Compiler) !void {
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

fn callHost(self: *Compiler, function: HostFunction) !void {
    const trampoline_code_offset: isize = @intCast(self.trampolines.codeOffset(function));
    const caller_offset: isize = @intCast(self.code.writable.list.items.len);
    return self.assembler.jal(.ra, @intCast(trampoline_code_offset - caller_offset));
}

fn hostRegFromV(guest_register: u4) riscv64.Register {
    return @enumFromInt(@as(u5, guest_register) + 16);
}

const ctx_reg = riscv64.Register.a0;
const i_reg = riscv64.Register.a1;

pub fn compile(self: *Compiler, instruction: chip8.Instruction) !void {
    const a = &self.assembler;
    try self.callHost(.check_remaining);
    var scope = RegisterScope.init(self);
    defer std.debug.assert(scope.isRestored());
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
            const offset_from_ctx = @as(u16, i_val) + @offsetOf(Context, "memory");
            if (std.math.cast(i12, offset_from_ctx)) |small_imm| {
                try a.addi(i_reg, ctx_reg, small_imm);
            } else {
                const tmp = scope.tempReg();
                try a.li(tmp, offset_from_ctx);
                try a.add(i_reg, ctx_reg, tmp);
            }
        },
        .random => |ins| {
            const dst_reg, const mask = ins;
            try scope.saveVRegsForHostCall();
            try scope.saveIForHostCall();
            try self.callHost(.random);
            try scope.restoreV();
            // this writes to the output V register but needs a1 to be the random result
            // instead of I. so it must overwrite the V registers that were saved across
            // the host call, but it must use a1 before it is restored to the value of I
            try a.andi(hostRegFromV(dst_reg), .a1, mask);
            try scope.restoreI();
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

            // compute the max I without wraparound:
            // if we load 2 registers, I can be at most FFD (load from [FFD] and [FFE], then leave set to [FFF])
            // so maximum I is FFF minus reg_count
            // add this value to the memory offset to get an offset from ctx
            // this value never fits in a small immediate :(
            //
            // li t0, (offset + FFF minus reg_count)
            // add t0, ctx, t0
            // bgtu i, t0, slow_path (aka bltu t0, i, slow_path)
            //
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

pub fn epilogue(self: *Compiler) !void {
    try self.assembler.ld(.ra, 0, .sp);
    try self.assembler.addi(.sp, .sp, 16);
    try self.assembler.li(.a0, @intFromError(error.HelloRiscv64));
    try self.assembler.ret();
}

pub fn makeExecutable(self: *Compiler) !void {
    _ = try self.code.makeExecutable(&.{});
}

pub fn entrypoint(self: *const Compiler) chip8.Cpu.GuestFunction {
    return self.code.executable.entrypoint(chip8.Cpu.GuestFunction, self.trampolines.code_template.len);
}

pub fn deinit(self: *Compiler) !void {
    if (self.code != .writable) _ = try self.code.makeWritable();
    self.code.writable.deinit();
}
