/// A growable buffer of bytes which can be written to. Nearly identical to ArrayList(u8),
/// except the capacity and pointer are kept page-aligned.
pub const Writable = struct {
    list: std.ArrayListAlignedUnmanaged(u8, .fromByteUnits(std.heap.page_size_min)),
    allocator: std.mem.Allocator,
    interface: std.io.Writer = .{
        .vtable = &.{ .drain = drain },
        // writes go to memory so buffering is unnecessary
        .buffer = &.{},
        .end = 0,
    },

    pub fn init(allocator: std.mem.Allocator) Writable {
        return .{ .list = .empty, .allocator = allocator };
    }

    /// Makes the code read-only and executable. If trap_bytes is not empty, it is repeated to fill
    /// any unused capacity.
    pub fn toExecutable(self: Writable, trap_bytes: []const u8) !Executable {
        std.debug.assert(std.mem.isAligned(self.list.capacity, std.heap.page_size_min));
        if (trap_bytes.len > 0) {
            var window = std.mem.window(
                u8,
                self.list.unusedCapacitySlice(),
                trap_bytes.len,
                trap_bytes.len,
            );
            while (window.next()) |chunk| {
                // safety: chunk is a slice of self.list.unusedCapacitySlice(), which is mutable
                @memcpy(@constCast(chunk), trap_bytes);
            }
        }

        try std.posix.mprotect(self.list.allocatedSlice(), std.posix.PROT.READ | std.posix.PROT.EXEC);
        return .{
            .code = self.list.items,
            .allocator = self.allocator,
            .capacity = self.list.capacity,
        };
    }

    pub fn deinit(self: *Writable) void {
        self.list.deinit(self.allocator);
        self.* = undefined;
    }

    fn drain(w: *std.io.Writer, data: []const []const u8, splat: usize) std.io.Writer.Error!usize {
        const self: *Writable = @fieldParentPtr("interface", w);
        const new_bytes = blk: {
            var sum: usize = 0;
            for (data[0 .. data.len - 1]) |chunk| sum += chunk.len;
            break :blk sum;
        } +
            splat * data[data.len - 1].len;
        const min_capacity = self.list.items.len + new_bytes;
        const new_capacity = std.mem.alignForward(usize, min_capacity, std.heap.page_size_min);
        self.list.ensureTotalCapacityPrecise(self.allocator, new_capacity) catch return error.WriteFailed;
        for (data[0 .. data.len - 1]) |chunk| self.list.appendSliceAssumeCapacity(chunk);
        for (0..splat) |_| self.list.appendSliceAssumeCapacity(data[data.len - 1]);
        return new_bytes;
    }
};

/// A fixed buffer of bytes known to point to executable memory.
pub const Executable = struct {
    code: []align(std.heap.page_size_min) const u8,
    capacity: usize,
    /// Allocator responsible for code.ptr[0..capacity]
    allocator: std.mem.Allocator,

    /// Converts this into a writable code buffer, making the memory read-write and
    /// non-executable again
    pub fn toWritable(self: Executable) !Writable {
        const allocated_slice: []align(std.heap.page_size_min) u8 = @constCast(self.code.ptr[0..self.capacity]);
        try std.posix.mprotect(allocated_slice, std.posix.PROT.READ | std.posix.PROT.WRITE);
        return .{
            .list = .{
                .items = @constCast(self.code),
                .capacity = self.capacity,
            },
            .allocator = self.allocator,
        };
    }

    pub fn entrypoint(self: *const Executable, comptime T: type, offset: usize) T {
        comptime std.debug.assert(@typeInfo(@typeInfo(T).pointer.child) == .@"fn");
        return @alignCast(@ptrCast(&self.code[offset]));
    }
};

/// Wrapper for holding either a Writable or an Executable and easily changing which is active.
pub const Any = union(enum) {
    writable: Writable,
    executable: Executable,

    pub fn makeExecutable(self: *Any, trap_bytes: []const u8) !*Executable {
        const writable = self.writable;
        self.* = .{ .executable = try writable.toExecutable(trap_bytes) };
        return &self.executable;
    }

    pub fn makeWritable(self: *Any) !*Writable {
        const executable = self.executable;
        self.* = .{ .writable = try executable.toWritable() };
        return &self.writable;
    }
};

const std = @import("std");
