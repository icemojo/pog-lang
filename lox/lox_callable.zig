const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const Value = @import("interpreter.zig").Value;
const Interpreter = @import("interpreter.zig");

//pub fn LoxCallable(comptime SourceType: type) type {
pub fn LoxCallable() type {
    return struct {
        const Self = @This();

        name: []const u8,
        allocator: Allocator,

        pub fn init(allocator: Allocator, name: []const u8) Self {
            return Self{
                .name = name,
                .allocator = allocator,
            };
        }

        pub fn arity(self: *const Self) u32 {
            _ = self;
            return 0;
        }

        pub fn call(self: *Self, allocator: Allocator, interpreter: *Interpreter, arguments: ?*std.ArrayList(Value)) Value {
            _ = self;
            _ = allocator;
            _ = interpreter;
            _ = arguments;
            debug.print("The function is being called now!!\n", .{});
            return Value{ .nil = true };
        }

        pub fn toString(self: *Self, allocator: Allocator) []const u8 {
            const str = std.fmt.allocPrint(allocator, "<func {s}(...)>", .{ self.name });
            return str;
        }
    };
}
