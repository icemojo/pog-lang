const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

pub fn LoxCallable(comptime ArgumentType: type, comptime ReturnType: type) type {
    return struct {
        const Self = @This();

        name: []const u8,
        func: CallableFunc, 
        args: ?std.ArrayList(ArgumentType),

        const CallableFunc = *const fn (allocator: Allocator, args: ?std.ArrayList(ArgumentType)) ?ReturnType;

        pub fn init(name: []const u8, func: CallableFunc, args: ?std.ArrayList(ArgumentType)) Self {
            return Self{
                .name = name,
                .func = func,
                .args = args,
            };
        }

        pub fn arity(self: *const Self) usize {
            return if (self.args) |args| args.items.len else 0;
        }

        pub fn call(self: *const Self, allocator: Allocator) ?ReturnType {
            return self.func(allocator, self.args);
        }
    };
}
