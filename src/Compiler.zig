const std = @import("std");

const riscv64 = @import("./riscv64.zig");
const x86_64 = @import("./x86_64.zig");
const chip8 = @import("./chip8.zig");

pub const Isa = enum {
    x86_64,
    riscv64,
};

fn log(a: u32) callconv(.c) void {
    std.log.scoped(.runtime).info("a = {}", .{a});
}

const HostFunction = enum {
    log,
    yield,
};

const HostFunctionTrampolines = struct {
    /// Contains the actual code but null function pointers. The function pointers must be written
    /// here using `pointer_offsets` before the code can be called.
    code_template: []const u8,
    /// Tells the size of each function pointer stored at the beginning of code_template
    pointer_size: usize,
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
        inline for (@typeInfo(HostFunction).@"enum".fields) |field| {
            std.mem.writeInt(
                u64,
                buf[8 * field.value ..][0..8],
                @intFromPtr(host_functions.get(@enumFromInt(field.value))),
                .little,
            );
        }
    }
};

const host_functions = std.EnumArray(HostFunction, *const anyopaque).init(.{
    .log = &log,
    .yield = &chip8.Cpu.Context.yield,
});

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

    var markers = std.EnumArray(HostFunction, riscv64.Assembler.Marker).initFill(@enumFromInt(std.math.maxInt(usize)));

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
        .pointer_size = 8,
        .first_code_offset = first_code_offset,
        .each_code_size = each_code_size.?,
    };
}

const riscv64_trampolines = makeRiscv64Trampolines(false) catch unreachable;
const riscv64_c_trampolines = makeRiscv64Trampolines(true) catch unreachable;

pub fn Compiler(comptime isa: Isa) type {
    return struct {
        assembler: Assembler,
        allocator: std.mem.Allocator,
        trampolines: *const HostFunctionTrampolines,

        const Self = @This();
        const Assembler = switch (isa) {
            .x86_64 => x86_64.Assembler,
            .riscv64 => riscv64.Assembler,
        };

        pub fn init(allocator: std.mem.Allocator, feature_set: std.Target.Cpu.Feature.Set) !Self {
            var assembler = Assembler.init(allocator, feature_set);
            const trampolines = if (assembler.features.zca)
                &riscv64_c_trampolines
            else
                &riscv64_trampolines;
            try assembler.insertBytes(trampolines.code_template);
            trampolines.write(assembler.slice());

            return .{
                .assembler = assembler,
                .allocator = allocator,
                .trampolines = trampolines,
            };
        }

        pub fn prologue(self: *Self) !void {
            try self.assembler.addi(.sp, .sp, -32);
            try self.assembler.sd(.ra, 0, .sp);
            try self.assembler.sd(.s0, 8, .sp);
            try self.assembler.sd(.s1, 16, .sp);
            try self.assembler.addi(.s1, .a0, 0);
        }

        fn callHost(self: *Self, function: HostFunction) !void {
            const trampoline_code_offset: isize = @intCast(self.trampolines.codeOffset(function));
            const caller_offset: isize = @intCast(self.assembler.offsetNextInstruction());
            return self.assembler.jal(.ra, @intCast(trampoline_code_offset - caller_offset));
        }

        pub fn genSomeCode(self: *Self) !void {
            for (0..3) |_| {
                try self.assembler.addi(.s0, .s0, 1);
                try self.assembler.addi(.a0, .s0, 0);
                try self.callHost(.log);
                try self.assembler.addi(.a0, .s1, 0);
                try self.callHost(.yield);
            }
        }

        pub fn epilogue(self: *Self) !void {
            try self.assembler.ld(.ra, 0, .sp);
            try self.assembler.ld(.s0, 8, .sp);
            try self.assembler.ld(.s1, 16, .sp);
            try self.assembler.addi(.sp, .sp, 32);
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
