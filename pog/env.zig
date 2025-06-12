const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const Value = @import("value.zig").Value;
const LoxFunction = @import("lox_callable.zig").LoxFunction;
const RuntimeError = @import("interpreter.zig").RuntimeError;

const Environment = @This();

enclosing: ?*Environment,
values: std.StringHashMap(Value),

pub fn init(allocator: Allocator, enclosing: ?*Environment) *Environment {
    const env = allocator.create(Environment) catch unreachable;
    env.*.enclosing = enclosing;
    env.*.values = std.StringHashMap(Value).init(allocator);
    return env;
}

pub fn define(self: *Environment, name: []const u8, value: Value) !void {
    if (self.alreadyDefined(name)) {
        return RuntimeError.AlreadyDefinedIdentifier;
    }
    try self.values.put(name, value);
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
    self.values.put(name, value) catch return RuntimeError.DefinitionFailed;
}

pub fn getValue(self: *const Environment, name: []const u8) !Value {
    if (self.values.get(name)) |value| {
        return value;
    } else {
        if (self.enclosing) |it| {
            return it.*.getValue(name);
        } else {
            // NOTE(yemon): Already at the top most (global) scope
            return RuntimeError.UndefinedIdentifier;
        }
    }
}

fn alreadyDefined(self: *const Environment, name: []const u8) bool {
    if (self.values.get(name)) |_| {
        return true;
    } else {
        return false;
    }
}

pub fn display(self: *const Environment) void {
    debug.print("[Env: ", .{});
    var iter = self.values.iterator();
    while (iter.next()) |entry| {
        const key: []const u8 = entry.key_ptr.*;
        const value: Value = entry.value_ptr.*;
        debug.print("{s}=", .{ key });
        value.display(true);
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
