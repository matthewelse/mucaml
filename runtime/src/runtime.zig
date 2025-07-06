const config = @import("config");
const std = @import("std");
const microzig = @import("microzig");
const hal = microzig.hal;
const time = hal.time;

const device =
    if (@hasDecl(config, "rpi"))
        @import("./rpi.zig")
    else
        struct {
            pub fn init() void {}

            pub fn loop() void {}
        };

export fn mucaml_sleep(ms: u32) void {
    time.sleep_ms(ms);
}

extern fn mucaml_main(i32) u32;

pub fn main() !void {
    device.init();

    _ = mucaml_main(0);

    device.loop();
}
