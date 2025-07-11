const config = @import("config");
const std = @import("std");
const microzig = @import("microzig");

const device =
    if (std.mem.eql(u8, config.mu_target, "rp2350"))
        @import("./rpi.zig")
    else if (std.mem.eql(u8, config.mu_target, "stm32f100rb"))
        @import("./stm32.zig")
    else
        @compileError("micro library is only for embedded targets");

extern fn mucaml_main(i32) u32;

pub fn main() void {
    device.init();

    _ = mucaml_main(0);

    device.loop();
}
