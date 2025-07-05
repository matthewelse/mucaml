const microzig = @import("microzig");
const nrf = microzig.hal;
const gpio = nrf.gpio;

pub const led_active_state = 0;
pub const button_active_state = 0;
pub const button_pull = .pullup;

pub const led1 = gpio.num(0, 8);
pub const led_r = led1;
pub const led2 = gpio.num(1, 9);
pub const led_g = led2;
pub const led3 = gpio.num(0, 12);
pub const led_b = led3;
pub const leds: []const gpio.Pin = &.{ led1, led2, led3 };

pub const buttons: []const gpio.Pin = &.{ button1, button2 };
pub const button1 = gpio.num(1, 6);
pub const button2 = gpio.num(0, 18); // Reset, has external pullup

pub fn init() void {
    for (leds) |led| {
        led.set_direction(.out);
        led.put(1);
    }

    button1.set_pull(.up);
    for (buttons) |button|
        button.set_direction(.in);
}
