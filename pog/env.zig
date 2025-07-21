const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const Value = @import("value.zig").Value;
const PogFunction = @import("callable.zig").PogFunction;
const RuntimeError = @import("interpreter.zig").RuntimeError;

const Environment = @This();

const EnvValue = struct {
    name: []const u8,
    value: Value,
};

enclosing: ?*Environment,
values: std.ArrayList(EnvValue),

pub fn init(allocator: Allocator, enclosing: ?*Environment) *Environment {
    const env = allocator.create(Environment) catch unreachable;
    env.*.enclosing = enclosing;
    env.*.values = std.ArrayList(EnvValue).init(allocator);
    return env;
}

pub fn deinit(self: *Environment) void {
    self.values.deinit();
}

pub fn define(self: *Environment, name: []const u8, value: Value) !void {
    if (self.alreadyDefined(name)) {
        return RuntimeError.AlreadyDefinedIdentifier;
    }
    try self.values.append(.{ .name = name, .value = value });
}

pub fn assign(self: *Environment, name: []const u8, value: Value) !void {
    if (!self.alreadyDefined(name)) {
        if (self.enclosing) |enclosing| {
            return try enclosing.*.assign(name, value);
        } else {
            // NOTE(yemon): Already at the top most (global) scope
            return RuntimeError.UndefinedIdentifier;
        }
    }
    self.values.append(.{ .name = name, .value = value }) 
        catch return RuntimeError.DefinitionFailed;
}

pub fn getValue(self: *const Environment, name: []const u8) !Value {
    var value: ?Value = null;
    for (self.values.items) |item| {
        if (std.mem.eql(u8, item.name, name)) {
            value = item.value;
        }
    }

    return if (value) |it| it else {
        if (self.enclosing) |enclosing| {
            return enclosing.*.getValue(name);
        } else {
            return RuntimeError.UndefinedIdentifier;
        }
    };
}

fn alreadyDefined(self: *const Environment, name: []const u8) bool {
    for (self.values.items) |item| {
        if (std.mem.eql(u8, item.name, name)) {
            return true;
        }
    }
    return false;
}

pub fn display(self: *const Environment) void {
    debug.print("[", .{});
    for (self.values.items) |item| {
        debug.print("{s}=", .{ item.name });
        item.value.display(true);
        debug.print(" | ", .{});
    }
    debug.print("]", .{});

    if (self.enclosing) |enclosing| {
        debug.print(" ", .{});
        enclosing.*.display();
    } else {
        debug.print("\n", .{});
    }
}

