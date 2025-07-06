const config = @import("config");
const std = @import("std");
const microzig = @import("microzig");
const hal = microzig.hal;
const time = hal.time;
const semihosting = microzig.core.experimental.ARM_semihosting;

export fn mucaml_print(x: i32) i32 {
    semihosting.Debug.print("mucaml_print {d}\n", .{x});
    return 0;
}

export fn mucaml_exit(x: usize) void {
    semihosting.Debug.print("mucaml_exit {d}\n", .{x});
    semihosting.Debug.exit(x);
}

pub fn init() void {}

pub fn loop() void {}
