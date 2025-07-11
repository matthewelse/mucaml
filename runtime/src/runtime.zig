const config = @import("config");
const std = @import("std");

const device =
    if (std.mem.eql(u8, config.mu_target, "rp2350"))
        @import("./rpi.zig")
    else if (std.mem.eql(u8, config.mu_target, "stm32f100rb"))
        @import("./stm32.zig")
    else
        struct {
            pub fn init() void {}

            pub fn loop() void {}

            export fn mucaml_print(x: i32) void {
                const out_file = std.io.getStdOut();
                (out_file.writer().print("{d}", .{x})) catch {};
            }
        };

extern fn mucaml_main(i32) u32;

export fn string_length(_: i32) i32 {
    return 0;
}

pub fn main() void {
    device.init();

    _ = mucaml_main(0);

    device.loop();
}
