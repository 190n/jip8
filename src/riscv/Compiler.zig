const std = @import("std");

const riscv = @import("../riscv.zig");
const chip8 = @import("../chip8.zig");
const Context = chip8.Cpu.Context;
const code_buffer = @import("../code_buffer.zig");

const HostFunction = std.meta.DeclEnum(host_functions);

code: code_buffer.Any,
assembler: riscv.Assembler,
trampolines: *const HostFunctionTrampolines,

const host_functions = struct {
    pub const check_remaining = &riscvCheckRemaining;
    pub const random = switch (@import("builtin").cpu.arch) {
        .riscv32 => &randomImpl32,
        .riscv64 => &randomImpl,
        else => unreachable,
    };
    pub const load_slow = &loadSlow;
};

// load instruction where the I register could overflow
fn loadSlow(context: *Context, i: *const u8, reg_count: u8) callconv(.c) extern struct {
    context: *Context,
    i: *const u8,
} {
    // riscv32 blocked on 24669
    comptime std.debug.assert(@import("builtin").cpu.arch == .riscv64);

    var guest_i: u12 = @intCast(@intFromPtr(i) - @intFromPtr(&context.memory));
    for (0..reg_count, context.v[0..]) |_, *register| {
        register.* = context.memory[guest_i];
        guest_i +%= 1;
    }
    return .{ .context = context, .i = &context.memory[guest_i] };
}

fn randomImpl(context: *Context) callconv(.c) extern struct { a0: *Context, a1: u8 } {
    const cpu: *chip8.Cpu = @alignCast(@fieldParentPtr("context", context));
    return .{
        .a0 = context,
        .a1 = cpu.random.random().int(u8),
    };
}

// TODO change to always use randomImpl once https://github.com/ziglang/zig/pull/24669 lands in a tarball
fn randomImpl32(context: *Context) callconv(.c) u8 {
    const cpu: *chip8.Cpu = @alignCast(@fieldParentPtr("context", context));
    return cpu.random.random().int(u8);
}

fn riscvCheckRemaining() callconv(.naked) void {
    const store_instruction: []const u8 = comptime switch (@import("builtin").cpu.arch) {
        .riscv32 => "sw",
        .riscv64 => "sd",
        else => unreachable,
    };
    const load_instruction: []const u8 = comptime switch (@import("builtin").cpu.arch) {
        .riscv32 => "lw",
        .riscv64 => "ld",
        else => unreachable,
    };

    // TODO:
    // assemble this code into the JIT region, and use hostCall to call yield
    // so that the fast path is a jump nearby and only the uncommon case of
    // actually yielding has to read the address and indirect call
    asm volatile (std.fmt.comptimePrint(
            \\beqz s1, yield
            \\addi s1, s1, -1
            \\ret
            \\yield:
            \\addi sp, sp, -16
            \\{[store]s} ra, 0(sp)
        ,
            .{ .store = store_instruction },
        ));

    asm volatile (std.fmt.comptimePrint(
            "{[store]s} a1, {[offset]}(a0)",
            .{ .store = store_instruction, .offset = @offsetOf(Context, "i") },
        ));
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

    asm volatile (std.fmt.comptimePrint(
            "{[load]s} a1, {[offset]}(a0)",
            .{ .load = load_instruction, .offset = @offsetOf(Context, "i") },
        ));
    asm volatile (std.fmt.comptimePrint("lhu s1, {}(a0)", .{@offsetOf(Context, "instructions_remaining")}));
    inline for (0..16) |vx| {
        const host_reg = comptime hostRegFromV(@intCast(vx));
        asm volatile (std.fmt.comptimePrint(
                "lbu {s}, {}(a0)",
                .{ @tagName(host_reg), vx + @offsetOf(Context, "v") },
            ));
    }

    asm volatile (std.fmt.comptimePrint(
            \\{[load]s} ra, 0(sp)
            \\addi sp, sp, 16
            \\ret
        ,
            .{ .load = load_instruction },
        ));
}

const HostFunctionTrampolines = struct {
    /// Contains the actual code but null function pointers. The function pointers must be written
    /// here using `pointer_offsets` before the code can be called.
    code_template: []const u8,
    /// Tells the size of each function pointer stored at the beginning of code_template
    pointer_size: riscv.Bits,
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

fn makeRiscvTrampolines(
    comptime bits: riscv.Bits,
    comptime compressed: bool,
) !HostFunctionTrampolines {
    var code: [4096]u8 = undefined;
    var writer: std.io.Writer = .fixed(&code);
    var assembler = riscv.Assembler.init(
        &writer,
        std.Target.riscv.featureSet(switch (bits) {
            .@"32" => if (compressed)
                &.{ .i, .c }
            else
                &.{.i},
            .@"64" => if (compressed)
                &.{ .@"64bit", .i, .c }
            else
                &.{ .@"64bit", .i },
        }),
    );

    var pointer_offsets: std.EnumArray(HostFunction, isize) = undefined;

    for (@typeInfo(HostFunction).@"enum".fields) |field| {
        pointer_offsets.set(@enumFromInt(field.value), @intCast(writer.buffered().len));
        try writer.splatByteAll(0, bits.bytes());
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
        try assembler.load_register(.t0, @intCast(offset_to_function_pointer), .t0);
        // jump to the function
        try assembler.jr(.t0, 0);
    }

    const constant_code: [writer.buffered().len]u8 = code[0..writer.buffered().len].*;

    return .{
        .code_template = &constant_code,
        .pointer_size = bits,
        .first_code_offset = first_code_offset,
        .each_code_size = each_code_size.?,
    };
}

const riscv32_trampolines = makeRiscvTrampolines(.@"32", false) catch unreachable;
const riscv32_c_trampolines = makeRiscvTrampolines(.@"32", true) catch unreachable;
const riscv64_trampolines = makeRiscvTrampolines(.@"64", false) catch unreachable;
const riscv64_c_trampolines = makeRiscvTrampolines(.@"64", true) catch unreachable;

const Compiler = @This();

const RegisterScope = struct {
    a: *riscv.Assembler,
    /// bitmap
    v_regs_saved: u16 = 0,
    i_saved: bool = false,
    temp_regs_used: u8 = 0,

    pub fn init(compiler: *Compiler) RegisterScope {
        return .{ .a = &compiler.assembler };
    }

    pub fn tempReg(self: *RegisterScope) riscv.Register {
        defer self.temp_regs_used += 1;
        return switch (self.temp_regs_used) {
            0 => .t0,
            1 => .t1,
            2 => .t2,
            else => unreachable, // handle this by spilling something to the stack
        };
    }

    pub fn saveV(self: *RegisterScope, vx: u4) !void {
        try self.a.sb(hostRegFromV(vx), @as(i12, vx) + @offsetOf(Context, "v"), ctx_reg);
        self.v_regs_saved |= @as(u16, 1) << vx;
    }

    pub fn restoreV(self: *RegisterScope, vx: u4) !void {
        try self.a.lbu(hostRegFromV(vx), @as(i12, vx) + @offsetOf(Context, "v"), ctx_reg);
        // we allow restoring registers that were not saved before
        // to support a host call changing registers by setting them in memory instead of returning the new value
        self.v_regs_saved &= ~(@as(u16, 1) << vx);
    }

    pub fn saveVRegsForHostCall(self: *RegisterScope) !void {
        for (0..16) |vx| {
            const host = hostRegFromV(@intCast(vx));
            if (!host.isCalleeSaved()) {
                try self.saveV(@intCast(vx));
            }
        }
    }

    pub fn saveIForHostCall(self: *RegisterScope) !void {
        self.i_saved = true;
        try self.a.store_register(i_reg, @offsetOf(Context, "i"), ctx_reg);
    }

    pub fn restoreVRegsFromHostCall(self: *RegisterScope) !void {
        for (0..16) |vx| {
            const host = hostRegFromV(@intCast(vx));
            if (!host.isCalleeSaved()) {
                try self.restoreV(@intCast(vx));
            }
        }
    }

    pub fn restoreI(self: *RegisterScope) !void {
        if (self.i_saved) {
            self.i_saved = false;
            try self.a.load_register(i_reg, @offsetOf(Context, "i"), ctx_reg);
        }
    }

    pub fn isRestored(self: *const RegisterScope) bool {
        return self.v_regs_saved == 0 and !self.i_saved;
    }
};

pub fn init(self: *Compiler, allocator: std.mem.Allocator, feature_set: std.Target.Cpu.Feature.Set) !void {
    self.* = .{
        .code = .{ .writable = .init(allocator) },
        .assembler = undefined,
        .trampolines = undefined,
    };
    self.assembler = .init(&self.code.writable.interface, feature_set);
    self.trampolines = switch (self.assembler.features.bits) {
        .@"32" => if (self.assembler.hasCompressed()) &riscv32_c_trampolines else &riscv32_trampolines,
        .@"64" => if (self.assembler.hasCompressed()) &riscv64_c_trampolines else &riscv64_trampolines,
    };
    try self.code.writable.interface.writeAll(self.trampolines.code_template);
    self.trampolines.write(self.code.writable.list.items);
}

pub fn prologue(self: *Compiler) !void {
    const a = &self.assembler;
    try a.addi(.sp, .sp, -16);
    try a.store_register(.ra, 0, .sp);
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

fn hostRegFromV(guest_register: u4) riscv.Register {
    return @enumFromInt(@as(u5, guest_register) + 16);
}

const ctx_reg = riscv.Register.a0;
const i_reg = riscv.Register.a1;

const Marker = struct {
    compiler: *Compiler,
    offset: usize,

    /// Edit the branch instruction at the marker so its target will be the next instruction
    /// written to the compiler
    pub fn insertForwardBranchToNextInstruction(self: *const Marker) void {
        const ins: *align(2) riscv.Instruction = @alignCast(@ptrCast(&self.compiler.code.writable.list.items[self.offset]));
        const offset: i13 = @intCast(self.compiler.code.writable.list.items.len - self.offset);
        ins.assignB(offset);
    }

    /// Edit the jump instruction at the marker so its target will be the next instruction
    /// written to the compiler
    pub fn insertForwardJumpToNextInstruction(self: *const Marker) void {
        const ins: *align(2) riscv.Instruction = @alignCast(@ptrCast(&self.compiler.code.writable.list.items[self.offset]));
        const offset: i21 = @intCast(self.compiler.code.writable.list.items.len - self.offset);
        ins.assignJ(offset);
    }
};

fn markNextInstruction(self: *Compiler) Marker {
    return .{ .compiler = self, .offset = self.code.writable.list.items.len };
}

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
            switch (self.assembler.features.bits) {
                .@"32" => {
                    // 32-bit random impl does not return the context pointer, only the number
                    // so we need to save the context pointer somewhere
                    // TODO can probably pick a callee-save
                    try a.addi(.sp, .sp, -16);
                    try a.sw(ctx_reg, 0, .sp);
                    try self.callHost(.random);
                    const tmp_return_value = scope.tempReg();
                    // this dance assumes ctx is a0
                    comptime std.debug.assert(ctx_reg == .a0);
                    try a.mv(tmp_return_value, .a0);
                    try a.lw(ctx_reg, 0, .sp);
                    try a.addi(.sp, .sp, 16);
                    try scope.restoreVRegsFromHostCall();
                    try a.andi(hostRegFromV(dst_reg), tmp_return_value, mask);
                },
                .@"64" => {
                    try self.callHost(.random);
                    try scope.restoreVRegsFromHostCall();
                    // this writes to the output V register but needs a1 to be the random result
                    // instead of I. so it must overwrite the V registers that were saved across
                    // the host call, but it must use a1 before it is restored to the value of I
                    try a.andi(hostRegFromV(dst_reg), .a1, mask);
                },
            }
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
            const reg_count = @as(u8, up_to) + 1;
            // calculate the max pointer I can be to load all these registers
            // without wrapping around
            const max_i = scope.tempReg();
            try a.li(max_i, @as(i32, @offsetOf(Context, "memory")) + 0xfff - reg_count);
            try a.add(max_i, ctx_reg, max_i);
            const mark_branch_to_slow_path = self.markNextInstruction();
            // will be overwritten to branch to slow path
            try a.bgtu(i_reg, max_i, 0);

            // fast path, load everything in order
            for (0..reg_count) |vx_usize| {
                const vx: u4 = @intCast(vx_usize);
                try a.lbu(hostRegFromV(vx), vx, i_reg);
            }
            try a.addi(i_reg, i_reg, reg_count);
            const mark_jump_over_slow_path = self.markNextInstruction();
            // will be overwritten to jump over slow path
            try a.j(0);

            // slow path:
            mark_branch_to_slow_path.insertForwardBranchToNextInstruction();

            // save all V registers that are temporaries (so could be clobbered by the host call)
            // and that we're not about to load into (so we need to restore their values)
            for (reg_count..16) |vx_usize| {
                const vx: u4 = @intCast(vx_usize);
                const host_reg = hostRegFromV(vx);
                if (!host_reg.isCalleeSaved()) {
                    try scope.saveV(vx);
                }
            }
            // do the host call
            std.debug.assert(i_reg == .a1);
            try a.li(.a2, reg_count);
            try self.callHost(.load_slow);
            // host call can return its new I in a1 which is fine
            // restore V0-VX plus any temporaries from the first step
            for (0..16) |vx_usize| {
                const vx: u4 = @intCast(vx_usize);
                const host_reg = hostRegFromV(vx);
                if (vx <= up_to or !host_reg.isCalleeSaved()) {
                    try scope.restoreV(vx);
                }
            }

            // let the fast path jump here
            // TODO what if this is the last instruction?
            mark_jump_over_slow_path.insertForwardJumpToNextInstruction();
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
    try self.assembler.load_register(.ra, 0, .sp);
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
