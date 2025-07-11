const config = @import("config");
const std = @import("std");

// Core runtime functions that are available on all targets
extern fn mucaml_main(i32) u32;

fn strlen(s: [*]const u8) usize {
    var length: usize = 0;
    var ptr = s;

    while (ptr[0] != 0) {
        ptr += 1;
        length += 1;
    }

    return length;
}

comptime {
    if (std.mem.eql(u8, config.mu_target, "native")) {
        @export(&mucaml_print_endline, .{ .name = "mucaml_print_endline", .linkage = .strong });
    }
}

fn mucaml_print_endline(x: [*]const u8) callconv(.C) i32 {
    const out_file = std.io.getStdOut();
    const len = strlen(x);
    (out_file.writer().print("{s}\n", .{x[0..len]})) catch {};
    return 0;
}
