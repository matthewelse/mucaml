const config = @import("config");
const std = @import("std");
const microzig = @import("microzig");
const hal = microzig.hal;
const time = hal.time;

pub const pin_config =
    hal.pins.GlobalConfiguration{
        .GPIO7 = .{
            .name = "led",
            .direction = .out,
        },
    };

pub const pins = pin_config.pins();

export fn mucaml_pin(_: i32) *const microzig.hal.gpio.Pin {
    return &pins.led;
}

export fn mucaml_pin_toggle(pin: *const microzig.hal.gpio.Pin) void {
    pin.toggle();
}

export fn mucaml_sleep(ms: u32) void {
    time.sleep_ms(ms);
}

pub fn init() void {
    pin_config.apply();
}

pub fn loop() void {
    while (true) {
        pins.led.toggle();
        time.sleep_ms(100);
    }
}
