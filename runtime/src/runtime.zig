const config = @import("config");
const std = @import("std");

const device =
    if (std.mem.eql(u8, config.mu_target, "rp2350"))
        @import("./rpi.zig")
    else
        struct {
            pub fn init() void {}

            pub fn loop() void {}
        };

extern fn mucaml_main(i32) u32;

pub fn main() !void {
    device.init();

    _ = mucaml_main(0);

    device.loop();
}
