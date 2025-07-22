const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const callable = @import("callable.zig");
const PogFunction = callable.PogFunction;
const PogObject = callable.PogObject;

pub const ArithmeticOp = enum {
    substract,
    divide,
    multiply,
    addition,

    pub fn display(self: ArithmeticOp) void {
        switch (self) {
            .substract => debug.print("-=", .{}),
            .divide => debug.print("/=", .{}),
            .multiply => debug.print("*=", .{}),
            .addition => debug.print("+=", .{}),
        }
    }

    pub fn toString(self: ArithmeticOp) []const u8 {
        return switch (self) {
            .substract => "substraction assignment",
            .divide => "division assignment",
            .multiply => "multiplication assignment",
            .addition => "addition assignment",
        };
    }
};

const ComparisonOp = enum {
    lesser,
    lesser_equal,
    greater,
    greater_equal,
};

const ValueError = error {
    InvalidValueComparison,
    InvalidArithmeticOperand,
    InvalidComparisonOperand,
};

pub const Value = union(enum) {
    integer: i64,
    double: f64,
    string: []u8,
    boolean: bool,
    nil: bool,
    function: PogFunction,
    object: PogObject,

    pub fn isTruthy(self: Value) bool {
        switch (self) {
            .nil => return false,
            .boolean => |it| return it,
            else => return true,
        }
    }

    pub fn isEqual(self: Value, other: Value) ValueError!bool {
        switch (self) {
            .integer => |left| {
                if (other.getInteger()) |right| {
                    return left == right;
                } else {
                    return ValueError.InvalidValueComparison;
                }
            },
            .double => |left| {
                if (other.getDouble()) |right| {
                    return left == right;
                } else {
                    return ValueError.InvalidValueComparison;
                }
            },
            .string => |left| {
                if (other.getString()) |right| {
                    return std.mem.eql(u8, left, right);
                } else {
                    return ValueError.InvalidValueComparison;
                }
            },
            .boolean => |left| {
                if (other.getBoolean()) |right| {
                    return left == right;
                } else {
                    return ValueError.InvalidValueComparison;
                }
            },
            .nil => {
                if (other.isNil()) {
                    return true;
                } else {
                    return ValueError.InvalidValueComparison;
                }
            },
            .function => {
                return ValueError.InvalidValueComparison;
            },
            .object => {
                return ValueError.InvalidValueComparison;
            },
        }
    }

    pub fn isNumber(self: Value) bool {
        const int_value = self.getInteger();
        const double_value = self.getDouble();
        return int_value != null or double_value != null;
    }

    fn getInteger(self: Value) ?i64 {
        switch (self) {
            .integer => |it| return it,
            else => return null,
        }
    }

    fn getDouble(self: Value) ?f64 {
        switch (self) {
            .double => |it| return it,
            else => return null,
        }
    }

    fn getString(self: Value) ?[]u8 {
        switch (self) {
            .string => |it| return it,
            else => return null,
        }
    }

    fn getBoolean(self: Value) ?bool {
        switch (self) {
            .boolean => |it| return it,
            else => return null,
        }
    }

    pub fn isNil(self: Value) bool {
        switch (self) {
            .nil => return true,
            else => return false,
        }
    }

    pub fn doArithmetic(self: Value, rhs: Value, op: ArithmeticOp) ValueError!Value {
        // NOTE(yemon): I know 'force coercing' i64 into f64 is kinda dangerous
        // but I'm just getting this up and running for the time being.
        switch (self) {
            .integer => |left_int| {
                switch (rhs) {
                    .integer => |right_int| {
                        switch (op) {
                            .substract => return Value{ .integer = left_int - right_int, },
                            .divide => return Value{ .integer = @divFloor(left_int, right_int) },
                            .multiply => return Value{ .integer = left_int * right_int },
                            .addition => return Value{ .integer = left_int + right_int },
                        }
                    },
                    .double => |right_double| {
                        const left_value: f64 = @floatFromInt(left_int);
                        switch (op) {
                            .substract => return Value{ .double = left_value - right_double },
                            .divide => return Value{ .double = left_value / right_double },
                            .multiply => return Value{ .double = left_value * right_double },
                            .addition => return Value{ .double = left_value + right_double },
                        }
                    },
                    else => {
                        return ValueError.InvalidArithmeticOperand;
                    }
                }
            },
            
            .double => |left_double| {
                switch (rhs) {
                    .integer => |right_int| {
                        const right_value: f64 = @floatFromInt(right_int);
                        switch (op) {
                            .substract => return Value{ .double = left_double - right_value },
                            .divide => return Value{ .double = left_double / right_value },
                            .multiply => return Value{ .double = left_double * right_value },
                            .addition => return Value{ .double = left_double + right_value },
                        }
                    },
                    .double => |right_double| {
                        switch (op) {
                            .substract => return Value{ .double = left_double - right_double },
                            .divide => return Value{ .double = left_double / right_double },
                            .multiply => return Value{ .double = left_double * right_double },
                            .addition => return Value{ .double = left_double + right_double },
                        }
                    },
                    else => {
                        return ValueError.InvalidArithmeticOperand;
                    }
                }
            },

            else => {
                return ValueError.InvalidArithmeticOperand;
            }
        }
    }

    pub fn doComparison(self: Value, rhs: Value, op: ComparisonOp) ValueError!Value {
        // NOTE(yemon): force coercing the i64 into f64 here agin, just for simplicity's sake
        switch (self) {
            .integer => |left_int| {
                switch (rhs) {
                    .integer => |right_int| {
                        switch (op) {
                            .lesser => return Value{ .boolean = left_int < right_int },
                            .lesser_equal => return Value{ .boolean = left_int <= right_int },
                            .greater => return Value{ .boolean = left_int > right_int },
                            .greater_equal => return Value{ .boolean = left_int >= right_int },
                        }
                    },
                    .double => |right_double| {
                        const left_double: f64 = @floatFromInt(left_int);
                        switch (op) {
                            .lesser => return Value{ .boolean = left_double < right_double },
                            .lesser_equal => return Value{ .boolean = left_double <= right_double },
                            .greater => return Value{ .boolean = left_double > right_double },
                            .greater_equal => return Value{ .boolean = left_double >= right_double },
                        }
                    },
                    else => {
                        return ValueError.InvalidComparisonOperand;
                    }
                }
            },

            .double => |left_double| {
                switch (rhs) {
                    .integer => |right_int| {
                        const right_double: f64 = @floatFromInt(right_int);
                        switch (op) {
                            .lesser => return Value{ .boolean = left_double < right_double },
                            .lesser_equal => return Value{ .boolean = left_double <= right_double },
                            .greater => return Value{ .boolean = left_double > right_double },
                            .greater_equal => return Value{ .boolean = left_double >= right_double },
                        }
                    },
                    .double => |right_double| {
                        switch (op) {
                            .lesser => return Value{ .boolean = left_double < right_double },
                            .lesser_equal => return Value{ .boolean = left_double <= right_double },
                            .greater => return Value{ .boolean = left_double > right_double },
                            .greater_equal => return Value{ .boolean = left_double >= right_double },
                        }
                    },
                    else => {
                        return ValueError.InvalidComparisonOperand;
                    }
                }
            },

            else => {
                return ValueError.InvalidComparisonOperand;
            }
        }
    }

    pub fn toString(self: Value, allocator: Allocator, comptime string_quoted: bool) []const u8 {
        switch (self) {
            .integer => |it| {
                return std.fmt.allocPrint(allocator, "{}", .{ it }) catch "";
            },
            .double => |it| {
                return std.fmt.allocPrint(allocator, "{d}", .{ it }) catch "";
            },
            .string => |it| {
                return std.fmt.allocPrint(allocator, 
                    if (string_quoted) "\"{s}\"" else "{s}", .{ it }
                ) catch "";
            },
            .boolean => |it| {
                return std.fmt.allocPrint(allocator, "{}", .{ it }) catch "";
            },
            .nil => {
                return "nil";
            },
            .function => |it| {
                return it.toString(allocator);
            },
            .object => |it| {
                return it.toString(allocator);
            },
        }
    }

    pub fn display(self: Value, comptime string_quoted: bool) void {
        switch (self) {
            .integer => |it| {
                debug.print("{}", .{ it });
            },
            .double => |it| {
                debug.print("{d}", .{ it });
            },
            .string => |it| {
                debug.print(if (string_quoted) "\"{s}\"" else "{s}", .{ it });
            },
            .boolean => |it| {
                debug.print("{}", .{ it });
            },
            .nil => {
                debug.print("nil", .{});
            },
            .function => |it| {
                it.display();
            },
            .object => |it| {
                it.display();
            },
        }
    }

    pub fn getTypeName(self: Value) []const u8 {
        return @tagName(self);
    }
};

