const std = @import("std");

const Assembler = @This();

code: Buffer,
state: enum { writable, executable } = .writable,

const assert = std.debug.assert;
const page_size = std.mem.page_size;
pub const Buffer = std.ArrayListAligned(u8, page_size);

pub fn init(allocator: std.mem.Allocator) Assembler {
    return .{ .code = Buffer.init(allocator) };
}

pub fn deinit(self: *Assembler) void {
    if (self.state == .executable) {
        if (std.posix.mprotect(self.code.items, std.posix.PROT.READ | std.posix.PROT.WRITE)) {
            self.code.deinit();
        } else |_| {}
    } else self.code.deinit();
    self.* = undefined;
}

pub fn writer(self: *Assembler) Buffer.Writer {
    assert(self.state == .writable);
    return self.code.writer();
}

pub fn makeExecutable(self: *Assembler, trap_bytes: []const u8) !void {
    var bytes_to_add = std.mem.alignForward(usize, self.code.items.len, page_size) - self.code.items.len;
    assert(bytes_to_add % trap_bytes.len == 0);
    try self.code.ensureTotalCapacityPrecise(self.code.items.len + bytes_to_add);

    while (bytes_to_add > 0) : (bytes_to_add -= trap_bytes.len) {
        self.code.appendSliceAssumeCapacity(trap_bytes);
    }

    self.code.shrinkAndFree(self.code.items.len);
    try std.posix.mprotect(self.code.items, std.posix.PROT.READ | std.posix.PROT.EXEC);
    self.state = .executable;
}
