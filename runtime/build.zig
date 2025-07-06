const std = @import("std");
const microzig = @import("microzig");

const MicroBuild = microzig.MicroBuild(.{ .rp2xxx = true, .stm32 = true });

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const mz_dep = b.dependency("microzig", .{});
    const mb = MicroBuild.init(b, mz_dep) orelse return;

    const targets = [_]Target{
        .{ .target = mb.ports.rp2xxx.boards.raspberrypi.pico2_arm, .name = "rp2350" },
        .{ .target = mb.ports.stm32.chips.STM32F100RB, .name = "STM32F100RB" },
    };

    for (targets) |target| {
        const mz_target = target.target;

        const options = b.addOptions();
        options.addOption([]const u8, "mu_target", target.name);

        const firmware = mb.add_library(.{
            .name = "mucaml_runtime",
            .target = mz_target,
            .optimize = optimize,
            .root_source_file = b.path("src/runtime.zig"),
        });
        firmware.add_options("config", options);

        mb.install_static_lib(firmware);
    }
}

const Target = struct {
    target: *const microzig.Target,
    name: []const u8,
};
