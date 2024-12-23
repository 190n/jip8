const std = @import("std");
const Allocator = std.mem.Allocator;

const Assembler = @This();

const CodeArrayList = std.ArrayListAlignedUnmanaged(u8, page_size);

/// capacity must be kept page-aligned
code: CodeArrayList = .{},
state: enum { writable, executable } = .writable,
allocator: Allocator,

const assert = std.debug.assert;
const page_size = std.mem.page_size;

pub fn init(allocator: Allocator) Assembler {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Assembler) void {
    if (self.state == .executable) {
        // try to make it writable because allocator will set the freed memory to 0xAA in safe
        // builds
        if (std.posix.mprotect(self.code.allocatedSlice(), std.posix.PROT.READ | std.posix.PROT.WRITE)) {
            self.code.deinit(self.allocator);
        } else |_| {}
    } else self.code.deinit(self.allocator);
    self.* = undefined;
}

pub const Writer = std.io.Writer(*Assembler, Allocator.Error, writeFn);

pub fn writeFn(self: *Assembler, bytes: []const u8) Allocator.Error!usize {
    const new_capacity = std.mem.alignForward(
        usize,
        self.code.capacity + bytes.len,
        page_size,
    );
    try self.code.ensureTotalCapacityPrecise(self.allocator, new_capacity);
    self.code.appendSliceAssumeCapacity(bytes);
    return bytes.len;
}

pub fn writer(self: *Assembler) Writer {
    assert(self.state == .writable);
    return Writer{ .context = self };
}

/// Makes the code read-only and executable. Fills unused capacity with copies of trap_bytes, which
/// should be the machine code for a debug breakpoint/trap (x86_64 int3, RISC-V ebreak, etc).
pub fn makeExecutable(self: *Assembler, trap_bytes: []const u8) !void {
    assert(self.state == .writable);
    // fill the spare capacity of the buffer with trap instructions
    var window = std.mem.window(
        u8,
        self.code.allocatedSlice()[self.code.items.len..],
        trap_bytes.len,
        trap_bytes.len,
    );
    while (window.next()) |chunk| {
        // safety: chunk is a slice of self.code.allocatedSlice(), which is mutable
        @memcpy(@constCast(chunk), trap_bytes);
    }

    try std.posix.mprotect(self.code.allocatedSlice(), std.posix.PROT.READ | std.posix.PROT.EXEC);
    self.state = .executable;
}

pub fn entrypoint(self: *const Assembler, comptime T: type, offset: usize) T {
    assert(self.state == .executable);
    return @alignCast(@ptrCast(&self.code.items[offset]));
}
