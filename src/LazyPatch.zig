//! A lazy patch is a description of an edit to be made to machine code, which requires information
//! that was not yet available when the lazy patch was created.
//!
//! For instance, a CHIP-8 jump instruction may target an instruction after the jump. The size of
//! the host machine code between the jump and its target will not be known when the jump is first
//! seen, as it depends on what those instructions are. Therefore a lazy patch should be created
//! instructing the assembler to fill in code to jump to the right location, once that location is
//! known.

/// Function to call to generate code once the target location is known
implementation: *const fn (
    /// An ISA-specific assembler initialized such that writing will replace code at the
    /// correct location
    assembler: *anyopaque,
    /// Location of the code being patched
    src: Source,
    /// Context that was given when this patch was created
    context: PatchData,
) void,
/// Number of bytes that should be patched
size: usize,
/// Extra data for the implementation function to use
data: PatchData align(@alignOf(usize)),

/// Each kind of lazy patch will assign its own meaning to these bits using a packed struct.
pub const PatchData = @Type(.{ .int = .{
    .bits = 2 * @bitSizeOf(usize),
    .signedness = .unsigned,
} });

pub const Source = struct {
    /// Absolute memory location of the code being patched
    address: usize,
    /// Offset of the code being patched relative to the start of the assembler's buffer
    offset: usize,
};
