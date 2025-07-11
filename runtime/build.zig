const std = @import("std");
const microzig = @import("microzig");

const MicroBuild = microzig.MicroBuild(.{ .rp2xxx = true, .stm32 = true });

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const mz_dep = b.dependency("microzig", .{});

    const mb = MicroBuild.init(b, mz_dep) orelse return;

    const rp2350 = TargetKind{ .microzig = mb.ports.rp2xxx.boards.raspberrypi.pico2_arm };
    const stm32 = TargetKind{ .microzig = mb.ports.stm32.chips.STM32F100RB };
    const native = .native;

    const all_targets = [_]Target{
        .{ .target = rp2350, .name = "rp2350" },
        .{ .target = stm32, .name = "stm32f100rb" },
        .{ .target = native, .name = "native" },
    };

    const embedded_targets = [_]Target{
        .{ .target = rp2350, .name = "rp2350" },
        .{ .target = stm32, .name = "stm32f100rb" },
    };

    // Build runtime library for all targets
    build_runtime_library(b, &all_targets);

    // Build micro library for embedded targets only
    build_micro_library(b, &embedded_targets, optimize, mb);
}

fn build_runtime_library(b: *std.Build, targets: []const Target) void {
    const library_name = "mucaml_runtime";
    const root_source_file = "src/runtime.zig";

    for (targets) |mu_target| {
        const target_kind = mu_target.target;

        const options = b.addOptions();
        options.addOption([]const u8, "mu_target", mu_target.name);

        switch (target_kind) {
            .microzig => |mz_target| {
                const target = mz_target.zig_target;
                const root_module = b.createModule(.{ .root_source_file = b.path(root_source_file), .target = b.resolveTargetQuery(target) });
                root_module.addOptions("config", options);

                const lib = b.addLibrary(.{
                    .name = library_name,
                    .root_module = root_module,
                });

                const dest_dir = std.fmt.allocPrint(b.allocator, "lib/{s}", .{mu_target.name}) catch |err| {
                    std.debug.panic("Failed to allocate dest_dir: {any}", .{err});
                };

                const artifact = b.addInstallArtifact(lib, .{ .dest_dir = .{ .override = .{ .custom = dest_dir } } });
                b.getInstallStep().dependOn(&artifact.step);
            },
            .native => {
                const target = b.standardTargetOptions(.{});
                const root_module = b.createModule(.{ .root_source_file = b.path(root_source_file), .target = target });
                root_module.addOptions("config", options);

                const lib = b.addLibrary(.{
                    .name = library_name,
                    .root_module = root_module,
                });

                const artifact = b.addInstallArtifact(lib, .{ .dest_dir = .{ .override = .{ .custom = "lib/native" } } });
                b.getInstallStep().dependOn(&artifact.step);
            },
        }
    }
}

fn build_micro_library(b: *std.Build, targets: []const Target, optimize: std.builtin.OptimizeMode, mb: *MicroBuild) void {
    const library_name = "mucaml_micro";
    const root_source_file = "src/micro.zig";

    for (targets) |mu_target| {
        const target_kind = mu_target.target;

        const options = b.addOptions();
        options.addOption([]const u8, "mu_target", mu_target.name);

        switch (target_kind) {
            .microzig => |mz_target| {
                const firmware = mb.add_library(.{
                    .name = library_name,
                    .target = mz_target,
                    .optimize = optimize,
                    .root_source_file = b.path(root_source_file),
                });
                firmware.add_options("config", options);

                const dest_dir = std.fmt.allocPrint(b.allocator, "lib/{s}", .{mu_target.name}) catch |err| {
                    std.debug.panic("Failed to allocate dest_dir: {any}", .{err});
                };

                mb.install_static_lib(firmware, dest_dir);
            },
            .native => unreachable, // micro library is only for embedded targets
        }
    }
}

const TargetKind = union(enum) { microzig: *const microzig.Target, native };

const Target = struct {
    target: TargetKind,
    name: []const u8,
};
