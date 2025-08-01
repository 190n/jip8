pub const Instruction = enum(u16) {
    _,

    pub fn decode(self: Instruction) Decoded {
        const int = @intFromEnum(self);
        const nibbles = [4]u4{
            @truncate(int >> 12),
            @truncate(int >> 8),
            @truncate(int >> 4),
            @truncate(int >> 0),
        };
        const low8: u8 = @truncate(int);
        // set up different format variants
        const xy = [2]u4{ nibbles[1], nibbles[2] };
        const xnn = .{ nibbles[1], low8 };
        const nnn: u12 = @truncate(int);
        const xyn = [3]u4{ nibbles[1], nibbles[2], nibbles[3] };
        const x = nibbles[1];

        const invalid = Decoded{ .invalid = self };

        return switch (nibbles[0]) {
            0x0 => switch (low8) {
                0xe0 => .{ .clear = {} },
                0xee => .{ .ret = {} },
                else => invalid,
            },
            0x1 => .{ .jump = nnn },
            0x2 => .{ .call = nnn },
            0x3 => .{ .skip_if_equal = xnn },
            0x4 => .{ .skip_if_not_equal = xnn },
            0x5 => switch (nibbles[3]) {
                0x0 => .{ .skip_if_registers_equal = xy },
                else => invalid,
            },
            0x6 => .{ .set_register = xnn },
            0x7 => .{ .add_immediate = xnn },
            0x8 => switch (nibbles[3]) {
                0x0 => .{ .set_register_to_register = xy },
                0x1 => .{ .bitwise_or = xy },
                0x2 => .{ .bitwise_and = xy },
                0x3 => .{ .bitwise_xor = xy },
                0x4 => .{ .add_registers = xy },
                0x5 => .{ .sub_registers = xy },
                0x6 => .{ .shift_right = xy },
                0x7 => .{ .sub_registers_reverse = xy },
                0xe => .{ .shift_left = xy },
                else => invalid,
            },
            0x9 => switch (nibbles[3]) {
                0x0 => .{ .skip_if_registers_not_equal = xy },
                else => invalid,
            },
            0xa => .{ .set_i = nnn },
            0xb => .{ .jump_v0 = nnn },
            0xc => .{ .random = xnn },
            0xd => .{ .draw = xyn },
            0xe => switch (low8) {
                0x9e => .{ .skip_if_pressed = x },
                0xa1 => .{ .skip_if_not_pressed = x },
                else => invalid,
            },
            0xf => switch (low8) {
                0x07 => .{ .read_dt = x },
                0x0a => .{ .wait_for_key = x },
                0x15 => .{ .set_dt = x },
                0x18 => .{ .set_st = x },
                0x1e => .{ .increment_i = x },
                0x29 => .{ .set_i_to_font = x },
                0x33 => .{ .store_bcd = x },
                0x55 => .{ .store = x },
                0x65 => .{ .load = x },
                else => invalid,
            },
        };
    }

    pub const formats = struct {
        /// reg, reg
        pub const Xy = [2]u4;
        /// reg, imm8
        pub const Xnn = struct { u4, u8 };
        /// imm12
        pub const Nnn = u12;
        /// reg, reg, imm4
        pub const Xyn = [3]u4;
        /// reg
        pub const X = u4;
        pub const None = void;
    };

    pub const Decoded = union(enum) {
        /// 00E0: clear the screen
        clear: formats.None,
        /// 00EE: return
        ret: formats.None,
        /// 1NNN: jump to NNN
        jump: formats.Nnn,
        /// 2NNN: call subroutine at NNN
        call: formats.Nnn,
        /// 3XNN: skip next instruction if VX == NN
        skip_if_equal: formats.Xnn,
        /// 4XNN: skip next instruction if VX != NN
        skip_if_not_equal: formats.Xnn,
        /// 5XY0: skip next instruction if VX == VY
        skip_if_registers_equal: formats.Xy,
        /// 6XNN: set VX to NN
        set_register: formats.Xnn,
        /// 7XNN: add NN to VX without carry
        add_immediate: formats.Xnn,
        /// 8XY0: set VX to VY
        set_register_to_register: formats.Xy,
        /// 8XY1: set VX to VX | VY
        bitwise_or: formats.Xy,
        /// 8XY2: set VX to VX & VY
        bitwise_and: formats.Xy,
        /// 8XY3: set VX to VX ^ VY
        bitwise_xor: formats.Xy,
        /// 8XY4: set VX to VX + VY; set VF to 1 if carry occurred, 0 otherwise
        add_registers: formats.Xy,
        /// 8XY5: set VX to VX - VY; set VF to 0 if borrow occurred, 1 otherwise
        sub_registers: formats.Xy,
        /// 8XY6: set VX to VY >> 1, set VF to the former least significant bit of VY
        shift_right: formats.Xy,
        /// 8XY7: set VX to VY - VX; set VF to 0 if borrow occurred, 1 otherwise
        sub_registers_reverse: formats.Xy,
        /// 8XYE: set VX to VY << 1, set VF to the former most significant bit of VY
        shift_left: formats.Xy,
        /// 9XY0: skip next instruction if VX != VY
        skip_if_registers_not_equal: formats.Xy,
        /// ANNN: set I to NNN
        set_i: formats.Nnn,
        /// BNNN: jump to NNN + V0
        jump_v0: formats.Nnn,
        /// CXNN: set VX to rand() & NN
        random: formats.Xnn,
        /// DXYN: draw an 8xN sprite from memory starting at I at (VX, VY); set VF to 1 if any pixel was
        /// turned off, 0 otherwise
        draw: formats.Xyn,
        /// EX9E: skip next instruction if the key in VX is pressed
        skip_if_pressed: formats.X,
        /// EXA1: skip next instruction if the key in VX is not pressed
        skip_if_not_pressed: formats.X,
        /// FX07: store the value of the delay timer in VX
        read_dt: formats.X,
        /// FX0A: wait until any key is pressed, then store the key that was pressed in VX
        wait_for_key: formats.X,
        /// FX15: set the delay timer to the value of VX
        set_dt: formats.X,
        /// FX18: set the sound timer to the value of VX
        set_st: formats.X,
        /// FX1E: increment I by the value of VX
        increment_i: formats.X,
        /// FX29: set I to the address of the sprite for the digit in VX
        set_i_to_font: formats.X,
        /// FX33: store the binary-coded decimal version of the value of VX in I, I + 1, and I + 2
        store_bcd: formats.X,
        /// FX55: store registers [V0, VX] in memory starting at I; set I to I + X + 1
        store: formats.X,
        /// FX65: load values from memory starting at I into registers [V0, VX]; set I to I + X + 1
        load: formats.X,

        /// Holds the complete opcode, for debugging
        invalid: Instruction,
    };
};
