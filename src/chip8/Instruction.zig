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
        clear: formats.None,
        ret: formats.None,
        jump: formats.Nnn,
        call: formats.Nnn,
        skip_if_equal: formats.Xnn,
        skip_if_not_equal: formats.Xnn,
        skip_if_registers_equal: formats.Xy,
        set_register: formats.Xnn,
        add_immediate: formats.Xnn,
        set_register_to_register: formats.Xy,
        bitwise_or: formats.Xy,
        bitwise_and: formats.Xy,
        bitwise_xor: formats.Xy,
        add_registers: formats.Xy,
        sub_registers: formats.Xy,
        shift_right: formats.Xy,
        sub_registers_reverse: formats.Xy,
        shift_left: formats.Xy,
        skip_if_registers_not_equal: formats.Xy,
        set_i: formats.Nnn,
        jump_v0: formats.Nnn,
        random: formats.Xnn,
        draw: formats.Xyn,
        skip_if_pressed: formats.X,
        skip_if_not_pressed: formats.X,
        read_dt: formats.X,
        wait_for_key: formats.X,
        set_dt: formats.X,
        set_st: formats.X,
        increment_i: formats.X,
        set_i_to_font: formats.X,
        store_bcd: formats.X,
        store: formats.X,
        load: formats.X,

        /// Holds the complete opcode, for debugging
        invalid: Instruction,
    };
};
