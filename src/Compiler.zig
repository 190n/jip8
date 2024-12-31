const std = @import("std");

const riscv64 = @import("./riscv64.zig");
const x86_64 = @import("./x86_64.zig");
const chip8 = @import("./chip8.zig");

pub const Isa = enum {
    x86_64,
    riscv64,
};

fn debugV0AndV1(_: u64, _: u64, _: u64, _: u64, _: u64, _: u64, a6: u64, a7: u64) callconv(.c) void {
    std.log.scoped(.callback).info("v0/a6 = {}, v1/a7 = {}", .{ a6, a7 });
}

const HostFunction = std.meta.DeclEnum(host_functions);

const host_functions = struct {
    pub const yield: *align(@alignOf(fn () callconv(.c) void)) const anyopaque = &chip8.Cpu.Context.yield;
    pub const debug_v0_and_v1: *align(@alignOf(fn () callconv(.c) void)) const anyopaque = &debugV0AndV1;
};

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
    var assembler = riscv64.Assembler.initBuffer(
        &code,
        std.Target.riscv.featureSet(if (compressed)
            &.{ .@"64bit", .i, .c }
        else
            &.{ .@"64bit", .i }),
    );

    var markers: std.EnumArray(HostFunction, riscv64.Assembler.Marker) = undefined;

    for (@typeInfo(HostFunction).@"enum".fields) |field| {
        markers.set(@enumFromInt(field.value), assembler.mark());
        try assembler.insertBytes(&[1]u8{0} ** 8);
    }

    const first_code_offset = assembler.offsetNextInstruction();
    var each_code_size: ?usize = null;

    for (@typeInfo(HostFunction).@"enum".fields) |field| {
        const marker = markers.get(@enumFromInt(field.value));
        const offset_before = assembler.offsetNextInstruction();
        defer {
            const size = assembler.offsetNextInstruction() - offset_before;
            if (each_code_size) |known_size| {
                std.debug.assert(size == known_size);
            } else {
                each_code_size = size;
            }
        }

        const offset_to_function_pointer = assembler.distanceNextInstructionTo(marker);
        // get current PC in t0
        try assembler.auipc(.t0, 0);
        // load the function pointer into t0 using an offset from the PC
        try assembler.ld(.t0, @intCast(offset_to_function_pointer), .t0);
        // jump to the function
        try assembler.jr(.t0, 0);
    }

    const constant_code: [assembler.slice().len]u8 = assembler.slice()[0..assembler.slice().len].*;

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
        assembler: Assembler,
        trampolines: *const HostFunctionTrampolines,

        const Self = @This();
        const Assembler = switch (isa) {
            .x86_64 => x86_64.Assembler,
            .riscv64 => riscv64.Assembler,
        };

        pub fn init(allocator: std.mem.Allocator, feature_set: std.Target.Cpu.Feature.Set) !Self {
            var assembler = Assembler.init(allocator, feature_set);
            const trampolines = if (assembler.hasCompressed())
                &riscv64_c_trampolines
            else
                &riscv64_trampolines;
            try assembler.insertBytes(trampolines.code_template);
            trampolines.write(assembler.slice());

            return .{
                .assembler = assembler,
                .trampolines = trampolines,
            };
        }

        pub fn prologue(self: *Self) !void {
            try self.assembler.addi(.sp, .sp, -16);
            try self.assembler.sd(.ra, 0, .sp);
            try self.assembler.sd(.s0, 8, .sp);
            try self.assembler.addi(.s0, .a0, 0);
        }

        fn callHost(self: *Self, function: HostFunction) !void {
            const trampoline_code_offset: isize = @intCast(self.trampolines.codeOffset(function));
            const caller_offset: isize = @intCast(self.assembler.offsetNextInstruction());
            return self.assembler.jal(.ra, @intCast(trampoline_code_offset - caller_offset));
        }

        fn registerFor(self: *const Self, guest_register: u4) riscv64.Register {
            _ = self;
            return @enumFromInt(@as(u5, guest_register) + 16);
        }

        pub fn compile(self: *Self, instruction: chip8.Instruction) !void {
            const decoded = instruction.decode();
            switch (decoded) {
                .set_register => |ins| {
                    const vx, const nn = ins;
                    try self.assembler.li(self.registerFor(vx), nn);
                },
                .add_immediate => |ins| {
                    // TODO mask
                    const vx, const nn = ins;
                    try self.assembler.addi(self.registerFor(vx), self.registerFor(vx), nn);
                    // andi vx, vx, 0xff
                },
                .set_register_to_register => |ins| {
                    const vx, const vy = ins;
                    try self.assembler.addi(self.registerFor(vx), self.registerFor(vy), 0);
                },
                .invalid => try self.assembler.ebreak(),
                .clear, .ret, .jump, .call, .skip_if_equal, .skip_if_not_equal, .skip_if_registers_equal, .bitwise_or, .bitwise_and, .bitwise_xor, .add_registers, .sub_registers, .shift_right, .sub_registers_reverse, .shift_left, .skip_if_registers_not_equal, .set_i, .jump_v0, .random, .draw, .skip_if_pressed, .skip_if_not_pressed, .read_dt, .wait_for_key, .set_dt, .set_st, .increment_i, .set_i_to_font, .store_bcd, .store, .load => {
                    std.log.scoped(.compiler).warn("unimplemented chip-8 instruction: {x:0>4} ({s})", .{ @intFromEnum(instruction), @tagName(decoded) });
                },
            }
        }

        pub fn debug(self: *Self) !void {
            try self.callHost(.debug_v0_and_v1);
        }

        pub fn epilogue(self: *Self) !void {
            try self.assembler.ld(.ra, 0, .sp);
            try self.assembler.ld(.s0, 8, .sp);
            try self.assembler.addi(.sp, .sp, 16);
            try self.assembler.li(.a0, @intFromError(error.HelloRiscv64));
            try self.assembler.ret();
        }

        pub fn makeExecutable(self: *Self) !void {
            return self.assembler.makeExecutable();
        }

        pub fn entrypoint(self: *const Self) chip8.Cpu.GuestFunction {
            return self.assembler.entrypoint(chip8.Cpu.GuestFunction, self.trampolines.code_template.len);
        }

        pub fn deinit(self: *Self) void {
            self.assembler.deinit();
        }
    };
}
