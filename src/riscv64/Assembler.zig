const std = @import("std");

const Assembler = @This();

const riscv64 = @import("../riscv64.zig");
const Register = riscv64.Register;
const Instruction = riscv64.Instruction;

const GenericAssembler = @import("../Assembler.zig");

inner: GenericAssembler,

pub fn init(allocator: std.mem.Allocator) Assembler {
    return .{ .inner = GenericAssembler.init(allocator) };
}

pub fn deinit(self: *Assembler) void {
    self.inner.deinit();
}

pub fn makeExecutable(self: *Assembler) !void {
    // TODO compressed ebreak
    const ebreak: u32 = 0x00100073;
    try self.inner.makeExecutable(std.mem.asBytes(&std.mem.nativeToLittle(u32, ebreak)));
}

fn emit(self: *Assembler, instruction: Instruction) !void {
    try instruction.any().writeTo(self.inner.writer());
}

pub fn addi(self: *Assembler, rd: Register, rs1: Register, value: i12) !void {
    try self.emit(Instruction{ .i = .{
        .opcode = 0b0010011,
        .funct3 = 0b000,
        .rd = rd,
        .rs1 = rs1,
        .imm = @bitCast(value),
    } });
}

pub fn li(self: *Assembler, rd: Register, value: i64) !void {
    // TODO support more complex cases
    const imm: i12 = @intCast(value);
    try self.addi(rd, .zero, imm);
}

pub fn jalr(self: *Assembler, rd: Register, rs1: Register, offset: i12) !void {
    try self.emit(Instruction{ .i = .{
        .opcode = 0b1100111,
        .funct3 = 0b000,
        .rd = rd,
        .rs1 = rs1,
        .imm = @bitCast(offset),
    } });
}

pub fn ret(self: *Assembler) !void {
    try self.jalr(.zero, .ra, 0);
}
