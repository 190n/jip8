const std = @import("std");

const riscv = @import("../riscv.zig");
const chip8 = @import("../chip8.zig");
const Context = chip8.Cpu.Context;
const code_buffer = @import("../code_buffer.zig");

const HostFunction = std.meta.DeclEnum(host_functions);

code: code_buffer.Any,
assembler: riscv.Assembler,
trampolines: *const HostFunctionTrampolines,
entrypoint_offset: usize,
check_remaining: Marker,
/// Location of each compiled instruction
/// Indexed by guest address / 2
code_offsets: [2048]u32,
/// PC for next instruction that will be compiled
pc: u12,

const ctx_reg = riscv.Register.a0;
const i_reg = riscv.Register.a1;
const instructions_remaining_reg = riscv.Register.s1;
/// Link register for guest subroutine calls
const guest_ra = riscv.Register.t0;
const temp_regs = [_]riscv.Register{ .t1, .t2, .a2 };

const host_functions = struct {
    pub const random = &randomImpl;
    pub const yield = &chip8.Cpu.Context.yield;
    pub const snapshot = &snapshotImpl;
    pub const draw = &drawImpl;
};

fn randomImpl(context: *Context) callconv(.c) extern struct { a0: *Context, a1: u8 } {
    const cpu: *chip8.Cpu = @alignCast(@fieldParentPtr("context", context));
    return .{
        .a0 = context,
        .a1 = cpu.random.random().int(u8),
    };
}

fn snapshotImpl() callconv(.naked) void {
    if (chip8.Cpu.enable_snapshot) {
        const List = @FieldType(Context, "snapshots");
        const list_offset: isize = @offsetOf(Context, "snapshots");
        const load_word: []const u8 = switch (@import("builtin").cpu.arch) {
            .riscv32 => "lw",
            .riscv64 => "ld",
            else => unreachable,
        };
        const store_word: []const u8 = switch (@import("builtin").cpu.arch) {
            .riscv32 => "sw",
            .riscv64 => "sd",
            else => unreachable,
        };
        const next_offset = list_offset + @offsetOf(List, "next");
        const end_offset = list_offset + @offsetOf(List, "end");

        asm volatile (std.fmt.comptimePrint(
                // load next and end pointers to t1 and t2 (can't override t0 as it is the RA)
                \\{[load]s} t1, %[next_offset]({[ctx]t})
                \\{[load]s} t2, %[end_offset]({[ctx]t})
                // only execute normally if next < end, otherwise trap
                \\bltu t1, t2, 1f
                \\ebreak
                \\1:
            , .{ .load = load_word, .ctx = ctx_reg })
            :
            : [next_offset] "i" (next_offset),
              [end_offset] "i" (end_offset),
        );

        // store each V register in the snapshot
        inline for (0..16) |x| {
            const offset = @offsetOf(chip8.Cpu.Snapshot, "v") + x;
            asm volatile (std.fmt.comptimePrint(
                    \\sb x{[x_reg]d}, %[offset](t1)
                , .{ .x_reg = x + 16 })
                :
                : [offset] "i" (offset),
            );
        }

        asm volatile (std.fmt.comptimePrint(
                // set t2 to the offset of i past the context pointer
                \\sub t2, {[i]t}, {[ctx]t}
                // subtract the offset of the start of memory from the context
                \\addi t2, t2, %[negative_mem_offset]
                // store the computed offset in the snapshot
                \\sh t2, %[i_offset](t1)
                // store pc in the snapshot
                \\sh a2, %[pc_offset](t1)
                // increment the pointer where the next snapshot will be stored
                \\addi t1, t1, %[snapshot_size]
                // and store it in the cpu
                \\{[store]s} t1, %[next_offset]({[ctx]t})
                \\ret
            ,
                .{ .i = i_reg, .ctx = ctx_reg, .store = store_word },
            )
            :
            : [negative_mem_offset] "i" (@as(isize, -@offsetOf(Context, "memory"))),
              [i_offset] "i" (@offsetOf(chip8.Cpu.Snapshot, "i")),
              [pc_offset] "i" (@offsetOf(chip8.Cpu.Snapshot, "pc")),
              [snapshot_size] "i" (@sizeOf(chip8.Cpu.Snapshot)),
              [next_offset] "i" (next_offset),
        );
    } else {
        asm volatile ("ret");
    }
}

fn drawImpl(
    context: *Context,
    i: [*]const u8,
    x: u8,
    y: u8,
    rows: u8,
) callconv(.c) extern struct {
    context: *Context,
    i: [*]const u8,
} {
    // TODO bounds check (or in jit?)
    var intersect: u8 = 0;
    for (0..@min(rows, 32 - y)) |row| {
        const left = @bitReverse(i[row]) << @truncate(x % 8);
        const right: u8 = @truncate(@as(u16, @bitReverse(i[row]) >> @truncate(8 - (x % 8))));
        const index = (64 * (y + row) + x) / 8;
        intersect |= (context.screen[index] & left);
        context.screen[index] ^= left;
        if (x % 8 != 0 and x < 56) {
            intersect |= (context.screen[index + 1] & right);
            context.screen[index + 1] ^= right;
        }
    }
    context.v[0xf] = @intFromBool(intersect != 0);
    return .{ .context = context, .i = i };
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
        // get current PC in t1
        try assembler.auipc(.t1, 0);
        // load the function pointer into t1 using an offset from the PC
        try assembler.loadRegister(.t1, @intCast(offset_to_function_pointer), .t1);
        // jump to the function
        try assembler.jr(.t1, 0);
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
    ra_saved: bool = false,
    temp_regs_used: u8 = 0,

    pub fn init(compiler: *Compiler) RegisterScope {
        return .{ .a = &compiler.assembler };
    }

    pub fn tempReg(self: *RegisterScope) riscv.Register {
        defer self.temp_regs_used += 1;
        return temp_regs[self.temp_regs_used]; // handle overflow by spilling to the stack
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
        try self.a.storeRegister(i_reg, @offsetOf(Context, "i"), ctx_reg);
    }

    pub fn saveRaForHostCall(self: *RegisterScope) !void {
        self.ra_saved = true;
        try self.a.storeRegister(guest_ra, @offsetOf(Context, "guest_ra"), ctx_reg);
    }

    pub fn restoreVRegsFromHostCall(self: *RegisterScope) !void {
        // some instructions rely on host calls overwriting VF in the context
        comptime std.debug.assert(!hostRegFromV(0xf).isCalleeSaved());
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
            try self.a.loadRegister(i_reg, @offsetOf(Context, "i"), ctx_reg);
        }
    }

    pub fn restoreRa(self: *RegisterScope) !void {
        if (self.ra_saved) {
            self.ra_saved = false;
            try self.a.loadRegister(guest_ra, @offsetOf(Context, "guest_ra"), ctx_reg);
        }
    }

    pub fn isRestored(self: *const RegisterScope) bool {
        return self.v_regs_saved == 0 and !self.i_saved and !self.ra_saved;
    }
};

pub fn init(self: *Compiler, allocator: std.mem.Allocator, feature_set: std.Target.Cpu.Feature.Set) void {
    self.* = .{
        .code = .{ .writable = .init(allocator) },
        .assembler = undefined,
        .trampolines = undefined,
        .entrypoint_offset = 0,
        .check_remaining = undefined,
        .code_offsets = @splat(0),
        .pc = 0x200,
    };
    self.assembler = .init(&self.code.writable.interface, feature_set);
    self.trampolines = switch (self.assembler.features.bits) {
        .@"32" => if (self.assembler.hasCompressed()) &riscv32_c_trampolines else &riscv32_trampolines,
        .@"64" => if (self.assembler.hasCompressed()) &riscv64_c_trampolines else &riscv64_trampolines,
    };
}

pub fn prologue(self: *Compiler) !void {
    // insert trampolines to indirectly call host functions
    try self.code.writable.interface.writeAll(self.trampolines.code_template);
    self.trampolines.write(self.code.writable.list.items);

    // insert the fast path to check if we can keep going
    self.check_remaining = self.markNextInstruction();
    const a = &self.assembler;

    const branch_to_yield = self.markNextInstruction();
    try a.beq(instructions_remaining_reg, .zero, 0);
    try a.addi(instructions_remaining_reg, instructions_remaining_reg, -1);
    try a.ret();

    // yield:
    branch_to_yield.insertForwardBranchToNextInstruction(self);
    var scope = RegisterScope.init(self);
    try a.addi(.sp, .sp, -16);
    try a.storeRegister(.ra, 0, .sp);
    try a.storeRegister(guest_ra, 8, .sp);
    try scope.saveIForHostCall();
    try scope.saveVRegsForHostCall();
    try self.callHost(.yield);
    try scope.restoreVRegsFromHostCall();
    try scope.restoreI();
    try a.lhu(instructions_remaining_reg, @offsetOf(Context, "instructions_remaining"), ctx_reg);
    try a.addi(instructions_remaining_reg, instructions_remaining_reg, -1);
    try a.loadRegister(guest_ra, 8, .sp);
    try a.loadRegister(.ra, 0, .sp);
    try a.addi(.sp, .sp, 16);
    try a.ret();

    // below is where code starts running from
    self.entrypoint_offset = self.code.writable.list.items.len;
    try a.addi(.sp, .sp, -16);
    try a.storeRegister(.ra, 0, .sp);
    try a.lhu(instructions_remaining_reg, @offsetOf(Context, "instructions_remaining"), .a0);

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

const Marker = struct {
    offset: usize,

    /// Edit the branch instruction at the marker so its target will be the next instruction
    /// written to the compiler
    pub fn insertForwardBranchToNextInstruction(self: *const Marker, compiler: *Compiler) void {
        const ins: *align(2) riscv.Instruction = @ptrCast(@alignCast(&compiler.code.writable.list.items[self.offset]));
        const offset: i13 = @intCast(compiler.code.writable.list.items.len - self.offset);
        ins.b.setOffset(offset);
    }

    /// Edit the jump instruction at the marker so its target will be the next instruction
    /// written to the compiler
    pub fn insertForwardJumpToNextInstruction(self: *const Marker, compiler: *Compiler) void {
        const ins: *align(2) riscv.Instruction = @ptrCast(@alignCast(&compiler.code.writable.list.items[self.offset]));
        const offset: i21 = @intCast(compiler.code.writable.list.items.len - self.offset);
        ins.j.setOffset(offset);
    }
};

fn markNextInstruction(self: *Compiler) Marker {
    return .{ .offset = self.code.writable.list.items.len };
}

pub fn compile(self: *Compiler, instruction: chip8.Instruction) !void {
    const a = &self.assembler;
    var scope = RegisterScope.init(self);
    // make sure everything we save gets restored by the end
    defer std.debug.assert(scope.isRestored());
    self.code_offsets[self.pc] = @intCast(self.code.writable.list.items.len);
    try a.jal(.ra, @intCast(@as(isize, @intCast(self.check_remaining.offset)) - @as(isize, @intCast(self.code.writable.list.items.len))));
    if (chip8.Cpu.enable_snapshot) {
        try a.li(.a2, self.pc);
        try self.callHost(.snapshot);
    }
    defer self.pc += 2;
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
            try scope.saveRaForHostCall();
            try self.callHost(.random);
            try scope.restoreVRegsFromHostCall();
            try scope.restoreRa();
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
            const reg_count = @as(u8, up_to) + 1;
            // calculate the max pointer I can be to load all these registers
            // without wrapping around
            const max_i = scope.tempReg();
            try a.li(max_i, @as(i32, @offsetOf(Context, "memory")) + 0xfff - reg_count);
            try a.add(max_i, ctx_reg, max_i);
            const mark_branch_to_trap = self.markNextInstruction();
            // will be overwritten to branch to trap
            try a.bgtu(i_reg, max_i, 0);

            // load everything in order
            for (0..reg_count) |vx_usize| {
                const vx: u4 = @intCast(vx_usize);
                try a.lbu(hostRegFromV(vx), vx, i_reg);
            }
            try a.addi(i_reg, i_reg, reg_count);
            const mark_jump_over_trap = self.markNextInstruction();
            // will be overwritten to jump over trap
            try a.j(0);

            mark_branch_to_trap.insertForwardBranchToNextInstruction(self);
            try a.ebreak();

            // let the normal path jump here
            // TODO what if this is the last instruction?
            mark_jump_over_trap.insertForwardJumpToNextInstruction(self);
        },
        .jump, .call => |target| {
            if (instruction.decode() == .jump) {
                if (target > self.pc) {
                    // emit custom instruction encoding containing the jump target
                    // which will later be fixed up into a real jump
                    try a.writer.writeInt(
                        u32,
                        @bitCast(riscv.Instruction{ .custom_forward_chip8_jump = .{
                            .target = target,
                            .kind = .jump,
                        } }),
                        .little,
                    );
                } else {
                    const dest: isize = @intCast(self.code_offsets[target]);
                    const offset = dest - @as(isize, @intCast(self.code.writable.list.items.len));
                    try a.j(@intCast(offset));
                }
            } else {
                // caller-side stack maintenance since we can't know which instructions are call targets
                try a.addi(.sp, .sp, -16);
                try a.storeRegister(.ra, 0, .sp);
                try a.storeRegister(guest_ra, 8, .sp);
                if (target > self.pc) {
                    // emit custom instruction encoding containing the jump target
                    // which will later be fixed up into a real jump
                    try a.writer.writeInt(
                        u32,
                        @bitCast(riscv.Instruction{ .custom_forward_chip8_jump = .{
                            .target = target,
                            .kind = .call,
                        } }),
                        .little,
                    );
                } else {
                    const dest: isize = @intCast(self.code_offsets[target]);
                    const offset = dest - @as(isize, @intCast(self.code.writable.list.items.len));
                    try a.jal(guest_ra, @intCast(offset));
                }
                try a.loadRegister(.ra, 0, .sp);
                try a.loadRegister(guest_ra, 8, .sp);
                try a.addi(.sp, .sp, 16);
            }
        },
        .ret => {
            try a.jr(guest_ra, 0);
        },
        .skip_if_equal => |ins| {
            const vx, const imm = ins;
            const compare_against: riscv.Register = if (imm == 0) .zero else reg: {
                const temp = scope.tempReg();
                try a.li(temp, imm);
                break :reg temp;
            };
            try a.writer.writeInt(
                u32,
                @bitCast(riscv.Instruction{ .custom_forward_chip8_jump = .{
                    .target = self.pc + 4,
                    .kind = .beq,
                    .rs1 = hostRegFromV(vx),
                    .rs2 = compare_against,
                } }),
                .little,
            );
        },
        .skip_if_not_equal => |ins| {
            const vx, const imm = ins;
            const compare_against: riscv.Register = if (imm == 0) .zero else reg: {
                const temp = scope.tempReg();
                try a.li(temp, imm);
                break :reg temp;
            };
            try a.writer.writeInt(
                u32,
                @bitCast(riscv.Instruction{ .custom_forward_chip8_jump = .{
                    .target = self.pc + 4,
                    .kind = .bne,
                    .rs1 = hostRegFromV(vx),
                    .rs2 = compare_against,
                } }),
                .little,
            );
        },
        .draw => |ins| {
            const vx, const vy, const rows = ins;
            // load coordinates and row count as function arguments
            // coordinates wrap around to screen space
            try a.andi(.a2, hostRegFromV(vx), 0x3f);
            try a.andi(.a3, hostRegFromV(vy), 0x1f);
            try a.li(.a4, rows);
            try scope.saveVRegsForHostCall();
            try scope.saveRaForHostCall();
            try self.callHost(.draw);
            try scope.restoreRa();
            try scope.restoreVRegsFromHostCall();
        },
        .read_dt => |vx| {
            try a.lbu(hostRegFromV(vx), @offsetOf(Context, "dt"), ctx_reg);
        },
        .set_dt => |vx| {
            try a.sb(hostRegFromV(vx), @offsetOf(Context, "dt"), ctx_reg);
        },
        .sub_registers_reverse => |ins| {
            const vx, const vy = ins;
            // set result and then set VF
            try a.sub(hostRegFromV(vx), hostRegFromV(vy), hostRegFromV(vx));
            // set VF to whether borrow occurred
            try a.slti(hostRegFromV(0xf), hostRegFromV(vx), 0);
            // wrap the output to 8-bit range, unless the output is VF in which case
            // we just set it to only 0 or 1
            if (vx != 0xf) {
                try a.andi(hostRegFromV(vx), hostRegFromV(vx), 0xff);
            }
            // flip VF so that it is 0 for borrow instead of 1 for borrow
            try a.xori(hostRegFromV(0xf), hostRegFromV(0xf), 1);
        },
        .increment_i => |vx| {
            // TODO bounds check
            try a.add(i_reg, i_reg, hostRegFromV(vx));
        },
        .clear => {
            const counter = scope.tempReg();
            const pointer = scope.tempReg();
            try a.li(counter, @divExact(@sizeOf(@FieldType(Context, "screen")), @as(u16, a.features.bits.bytes())));
            try a.addi(pointer, ctx_reg, @offsetOf(Context, "screen"));
            const loop = self.markNextInstruction();
            try a.storeRegister(.zero, 0, pointer);
            try a.addi(counter, counter, -1);
            try a.addi(pointer, pointer, a.features.bits.bytes());
            try a.bne(counter, .zero, @intCast(@as(isize, @intCast(loop.offset)) - @as(isize, @intCast(self.code.writable.list.items.len))));
        },

        .invalid => |opcode| {
            try a.li(temp_regs[0], @intFromEnum(opcode));
            try a.ebreak();
        },

        .skip_if_registers_equal,
        .bitwise_or,
        .bitwise_and,
        .bitwise_xor,
        .add_registers,
        .sub_registers,
        .shift_right,
        .shift_left,
        .skip_if_registers_not_equal,
        .jump_v0,
        .skip_if_pressed,
        .skip_if_not_pressed,
        .wait_for_key,
        .set_st,
        .set_i_to_font,
        .store_bcd,
        => {
            std.log.scoped(.compiler).warn("unimplemented chip-8 instruction: {x:0>4} ({s})", .{ @intFromEnum(instruction), @tagName(instruction.decode()) });
        },
    }
}

pub fn epilogue(self: *Compiler) !void {
    try self.assembler.loadRegister(.ra, 0, .sp);
    try self.assembler.addi(.sp, .sp, 16);
    try self.assembler.li(.a0, @intFromError(error.Overrun));
    try self.assembler.ret();
}

pub fn makeExecutable(self: *Compiler) !void {
    var iter: riscv.InstructionIterator = .init(self.code.writable.list.items);
    while (iter.next()) |ins_ptr| {
        if (ins_ptr == .rv and ins_ptr.rv.custom_forward_chip8_jump.opcode == .custom_forward_chip8_jump) {
            const ins = ins_ptr.rv.custom_forward_chip8_jump;
            const origin = @as([*]u8, @ptrCast(ins_ptr.rv)) - self.code.writable.list.items.ptr;
            const host_target = self.code_offsets[ins.target];
            ins_ptr.rv.* = switch (ins.kind) {
                .jump, .call => .makeJ(
                    .jal,
                    if (ins.kind == .jump) .zero else guest_ra,
                    @intCast(host_target - origin),
                ),
                .beq, .bne => .makeB(
                    .branch,
                    if (ins.kind == .beq) .beq else .bne,
                    ins.rs1,
                    ins.rs2,
                    @intCast(host_target - origin),
                ),
            };
        }
    }
    _ = try self.code.makeExecutable(&.{});
}

pub fn entrypoint(self: *const Compiler) chip8.Cpu.GuestFunction {
    return self.code.executable.entrypoint(chip8.Cpu.GuestFunction, self.entrypoint_offset);
}

pub fn deinit(self: *Compiler) !void {
    if (self.code != .writable) _ = try self.code.makeWritable();
    self.code.writable.deinit();
}

test "run one instruction at a time" {
    const t = std.testing;
    const allocator = t.allocator;
    const stack = try allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        4 << 10,
    );
    defer allocator.free(stack);
    var compiler: Compiler = undefined;
    compiler.init(allocator, @import("builtin").cpu.features);
    defer compiler.deinit() catch unreachable;
    try compiler.prologue();

    for (0..16) |i| {
        try compiler.compile(@enumFromInt(0x6000 | (i << 8) | (i << 4) | i));
    }
    try compiler.compile(@enumFromInt(0x1220));
    try compiler.epilogue();
    try compiler.makeExecutable();
    var snapshots: [17]chip8.Cpu.Snapshot = undefined;
    var cpu = chip8.Cpu.init(stack, compiler.entrypoint(), 0, &snapshots);
    for (0..17) |i| {
        try t.expectEqual(i, cpu.context.snapshots.slice().len);
        cpu.run(1) catch unreachable;
        const written_snapshots = cpu.context.snapshots.slice();
        try t.expectEqual(i + 1, written_snapshots.len);
        const last = written_snapshots[i];
        try t.expectEqual(0, last.i);
        try t.expectEqual(0x200 + 2 * i, last.pc);
        for (last.v, 0..) |vx, j| {
            // last is state before the ith instruction was executed
            try t.expectEqual(if (j < i) 17 * j else 0, vx);
        }
    }
}

test "run many instructions" {
    const t = std.testing;
    const allocator = t.allocator;
    const stack = try allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        4 << 10,
    );
    defer allocator.free(stack);
    var compiler: Compiler = undefined;
    compiler.init(allocator, @import("builtin").cpu.features);
    defer compiler.deinit() catch unreachable;
    try compiler.prologue();

    for (0..16) |i| {
        try compiler.compile(@enumFromInt(0x6000 | (i << 8) | (i << 4) | i));
    }
    try compiler.compile(@enumFromInt(0x1220));
    try compiler.epilogue();
    try compiler.makeExecutable();
    var snapshots: [17]chip8.Cpu.Snapshot = undefined;
    var cpu = chip8.Cpu.init(stack, compiler.entrypoint(), 0, &snapshots);
    try cpu.run(17);
    const written_snapshots = cpu.context.snapshots.slice();
    try t.expectEqual(17, written_snapshots.len);
    for (written_snapshots, 0..) |s, i| {
        try t.expectEqual(0, s.i);
        try t.expectEqual(0x200 + 2 * i, s.pc);
        for (s.v, 0..) |vx, j| {
            try t.expectEqual(if (j < i) 17 * j else 0, vx);
        }
    }
}

test "backwards jump" {
    const t = std.testing;
    const allocator = t.allocator;
    const stack = try allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        4 << 10,
    );
    defer allocator.free(stack);
    var compiler: Compiler = undefined;
    compiler.init(allocator, @import("builtin").cpu.features);
    defer compiler.deinit() catch unreachable;
    try compiler.prologue();

    try compiler.compile(@enumFromInt(0x7002));
    try compiler.compile(@enumFromInt(0x1200));

    try compiler.epilogue();
    try compiler.makeExecutable();
    var snapshots: [10]chip8.Cpu.Snapshot = undefined;
    var cpu = chip8.Cpu.init(stack, compiler.entrypoint(), 0, &snapshots);
    try cpu.run(10);
    for (snapshots, 0..) |s, i| {
        try t.expectEqual(0, s.i);
        try t.expectEqual(2 * ((i + 1) / 2), s.v[0]);
        try t.expectEqual(@as(u12, if (i % 2 == 0) 0x200 else 0x202), s.pc);
    }
}

test "forwards jump" {
    const t = std.testing;
    const allocator = t.allocator;
    const stack = try allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        4 << 10,
    );
    defer allocator.free(stack);
    var compiler: Compiler = undefined;
    compiler.init(allocator, @import("builtin").cpu.features);
    defer compiler.deinit() catch unreachable;
    try compiler.prologue();

    try compiler.compile(@enumFromInt(0x1208));
    for (0..3) |_| {
        try compiler.compile(@enumFromInt(0x0000));
    }
    try compiler.compile(@enumFromInt(0x1208));

    try compiler.epilogue();
    try compiler.makeExecutable();
    var snapshots: [2]chip8.Cpu.Snapshot = undefined;
    var cpu = chip8.Cpu.init(stack, compiler.entrypoint(), 0, &snapshots);
    try cpu.run(2);
    try t.expectEqual(snapshots[0].pc, 0x200);
    try t.expectEqual(snapshots[1].pc, 0x208);
}

test "call" {
    const t = std.testing;
    const allocator = t.allocator;
    const stack = try allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        4 << 10,
    );
    defer allocator.free(stack);
    var compiler: Compiler = undefined;
    compiler.init(allocator, @import("builtin").cpu.features);
    defer compiler.deinit() catch unreachable;
    try compiler.prologue();

    try compiler.compile(@enumFromInt(0x1206));
    try compiler.compile(@enumFromInt(0x6001));
    try compiler.compile(@enumFromInt(0x00ee));
    try compiler.compile(@enumFromInt(0x2202));
    try compiler.compile(@enumFromInt(0x1208));

    try compiler.epilogue();
    try compiler.makeExecutable();
    var snapshots: [5]chip8.Cpu.Snapshot = undefined;
    var cpu = chip8.Cpu.init(stack, compiler.entrypoint(), 0, &snapshots);
    try cpu.run(5);
    try t.expectEqual(0x200, snapshots[0].pc);
    try t.expectEqual(0x206, snapshots[1].pc);
    try t.expectEqual(0x202, snapshots[2].pc);
    try t.expectEqual(0x204, snapshots[3].pc);
    try t.expectEqual(1, snapshots[3].v[0]);
    try t.expectEqual(0x208, snapshots[4].pc);
}

test "loop" {
    const t = std.testing;
    const allocator = t.allocator;
    const stack = try allocator.alignedAlloc(
        u8,
        .fromByteUnits(std.heap.page_size_min),
        4 << 10,
    );
    defer allocator.free(stack);
    var compiler: Compiler = undefined;
    compiler.init(allocator, @import("builtin").cpu.features);
    defer compiler.deinit() catch unreachable;
    try compiler.prologue();

    try compiler.compile(@enumFromInt(0x6005));
    try compiler.compile(@enumFromInt(0x7103));
    try compiler.compile(@enumFromInt(0x70ff));
    try compiler.compile(@enumFromInt(0x3000));
    try compiler.compile(@enumFromInt(0x1202));
    try compiler.compile(@enumFromInt(0x120a));

    try compiler.epilogue();
    try compiler.makeExecutable();
    var snapshots: [21]chip8.Cpu.Snapshot = undefined;
    var cpu = chip8.Cpu.init(stack, compiler.entrypoint(), 0, &snapshots);
    try cpu.run(21);
    for (1..4) |i| {
        try t.expectEqual(0x206, snapshots[4 * i + 3].pc);
        // did not skip
        try t.expectEqual(0x208, snapshots[4 * i + 4].pc);
    }
    try t.expectEqual(0x206, snapshots[19].pc);
    // skipped
    try t.expectEqual(0x20a, snapshots[20].pc);
    try t.expectEqual(15, snapshots[20].v[1]);
}
