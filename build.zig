const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lox_exe = b.addExecutable(.{
        .name = "lox",
        .root_module = b.createModule(.{
            .root_source_file = b.path("lox/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    b.installArtifact(lox_exe);

    const ox_exe = b.addExecutable(.{
        .name = "ox",
        .root_module = b.createModule(.{
            .root_source_file = b.path("ox/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    b.installArtifact(ox_exe);

    const lox_run_cmd = b.addRunArtifact(lox_exe);
    lox_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        lox_run_cmd.addArgs(args);
    }
    const lox_run_step = b.step("lox", "Run the Lox interpreter (Use -h to view more options)");
    lox_run_step.dependOn(&lox_run_cmd.step);

    const ox_run_cmd = b.addRunArtifact(ox_exe);
    ox_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        ox_run_cmd.addArgs(args);
    }
    const ox_run_step = b.step("ox", "Run the Ox bytecode VM (Use -h to view more options)");
    ox_run_step.dependOn(&ox_run_cmd.step);
}
