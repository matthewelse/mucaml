const std = @import("std");
const microzig = @import("microzig");
const rp2xxx = microzig.hal;
const time = rp2xxx.time;

const pin_config = rp2xxx.pins.GlobalConfiguration{
    .GPIO7 = .{
        .name = "led",
        .direction = .out,
    },
};

const pins = pin_config.pins();

extern fn mucaml_main(i32) u32;

export fn mucaml_pin(_: i32) *const microzig.hal.gpio.Pin {
    return &pins.led;
}

export fn mucaml_pin_toggle(pin: *const microzig.hal.gpio.Pin) void {
    pin.toggle();
}

export fn mucaml_sleep(ms: u32) void {
    time.sleep_ms(ms);
}

pub fn main() !void {
    pin_config.apply();

    _ = mucaml_main(0);

    while (true) {
        pins.led.toggle();
        time.sleep_ms(100);
    }
}
