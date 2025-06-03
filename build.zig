const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const pog_exe = b.addExecutable(.{
        .name = "pog",
        .root_module = b.createModule(.{
            .root_source_file = b.path("pog/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    b.installArtifact(pog_exe);

    const og_exe = b.addExecutable(.{
        .name = "og",
        .root_module = b.createModule(.{
            .root_source_file = b.path("og/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    b.installArtifact(og_exe);

    const pog_run_cmd = b.addRunArtifact(pog_exe);
    pog_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        pog_run_cmd.addArgs(args);
    }
    const pog_run_step = b.step("pog", "Run the Pog language interpreter (Use -h to view more options)");
    pog_run_step.dependOn(&pog_run_cmd.step);

    const og_run_cmd = b.addRunArtifact(og_exe);
    og_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        og_run_cmd.addArgs(args);
    }
    const og_run_step = b.step("og", "Run the Og bytecode VM (Use -h to view more options)");
    og_run_step.dependOn(&og_run_cmd.step);
}
