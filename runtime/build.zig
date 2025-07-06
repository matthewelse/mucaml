const std = @import("std");
const microzig = @import("microzig");

const MicroBuild = microzig.MicroBuild(.{
    .rp2xxx = true,
});

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const mz_dep = b.dependency("microzig", .{});
    const mb = MicroBuild.init(b, mz_dep) orelse return;

    const raspberrypi = mb.ports.rp2xxx.boards.raspberrypi;
    const target = raspberrypi.pico2_arm;

    const options = b.addOptions();
    options.addOption(bool, "rpi", true);

    const firmware = mb.add_library(.{
        .name = "mucaml_runtime",
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/runtime.zig"),
    });
    firmware.add_options("config", options);

    mb.install_static_lib(firmware);
}
