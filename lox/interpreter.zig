const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const ast = @import("ast.zig");

const ArithmeticOp = enum {
    substract,
    divide,
    multiply,
    addition,
};

const ValueError = error {
    InvalidValueComparison,
};

pub const Value = union(enum) {
    integer: i64,
    double: f64,
    string: []u8,
    boolean: bool,
    nil: bool,
    // object: ??

    fn isTruthy(self: Value) bool {
        switch (self) {
            .nil => return false,
            .boolean => |it| return it,
            else => return true,
        }
    }

    fn isEqual(self: Value, other: Value) ValueError!bool {
        switch (self) {
            .integer => |left| {
                if (other.getInteger()) |right| {
                    return left == right;
                } else {
                    return error.InvalidValueComparison;
                }
            },
            .double => |left| {
                if (other.getDouble()) |right| {
                    return left == right;
                } else {
                    return error.InvalidValueComparison;
                }
            },
            .string => |left| {
                if (other.getString()) |right| {
                    return std.mem.eql(u8, left, right);
                } else {
                    return error.InvalidValueComparison;
                }
            },
            .boolean => |left| {
                if (other.getBoolean()) |right| {
                    return left == right;
                } else {
                    return error.InvalidValueComparison;
                }
            },
            .nil => {
                if (other.isNil()) {
                    return true;
                } else {
                    return error.InvalidValueComparison;
                }
            },
        }
    }

    fn isNumber(self: Value) bool {
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

    fn isNil(self: Value) bool {
        switch (self) {
            .nil => return true,
            else => return false,
        }
    }


    fn doArithmetic(self: Value, rhs: Value, op: ArithmeticOp) EvaluationError!Value {
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
                        return error.InvalidArithmeticOperand;
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
                        return error.InvalidArithmeticOperand;
                    }
                }
            },

            else => {
                return error.InvalidArithmeticOperand;
            }
        }
    }

    pub fn toString(self: Value, allocator: Allocator) []const u8 {
        switch (self) {
            .integer => |it| {
                return std.fmt.allocPrint(allocator, "{}", .{ it }) catch "";
            },
            .double => |it| {
                return std.fmt.allocPrint(allocator, "{d}", .{ it }) catch "";
            },
            .string => |it| {
                return std.fmt.allocPrint(allocator, "\"{s}\"", .{ it }) catch "";
            },
            .boolean => |it| {
                return std.fmt.allocPrint(allocator, "{}", .{ it }) catch "";
            },
            .nil => {
                return "nil";
            },
        }
    }
};

fn concatStrings(allocator: Allocator, string1: []const u8, string2: []const u8) !Value {
    const target_string = try std.fmt.allocPrint(allocator, "{s}{s}", .{ string1, string2 });
    return Value{ .string = target_string };
}

const EvaluationError = error {
    UnknownBinaryOperation,
    InvalidArithmeticOperand,
    InvalidStringOperand,
    InvalidValueTypeToNegate,
    StringConcatFailed,
    NotDoneYet,

    OutOfMemory,
};

// NOTE(yemon): Runtime errors should probably return, attached with a proper message
// since they should probably be presented and visible to the user.
const RuntimeError = error {
    InvalidUnaryOperand,
    InvalidBinaryOperands,
    UninitializedVariable,
    UndefinedVariable,
};

pub const Interpreter = struct {
    debug_print: bool,
    debug_env: bool,
    env: Environment,

    pub fn init(allocator: Allocator) Interpreter {
        return .{
            .debug_print = false,
            .debug_env = false,
            .env = Environment.init(allocator),
        };
    }

    fn initBuiltins(self: *Interpreter) void {
        _ = self;
        // TODO(yemon): `__env__`, `__name__` so on and so forth...
    }
    
    pub fn executeAll(self: *Interpreter, allocator: Allocator, statements: std.ArrayList(*ast.Stmt)) (EvaluationError || RuntimeError)!void {
        for (statements.items) |stmt| {
            try self.evaluateStatement(allocator, stmt);
            debugPrint(self, "{} items in environment.\n", .{ self.env.values.count() });
            if (self.debug_env) {
                // debug.print("{}\n", .{ self.env });
                self.env.display(allocator);
            }
        }
    }

    fn evaluateStatement(self: *Interpreter, allocator: Allocator, stmt: *const ast.Stmt) (EvaluationError || RuntimeError)!void {
        switch (stmt.*) {
            .variable => |variable| {
                debugPrint(self, "Evaluating variable statement...\n", .{});
                if (variable.initializer) |initializer| {
                    const value = try evaluate(self, allocator, initializer);
                    try self.env.define(variable.name, value);
                } else {
                    try self.env.define(variable.name, Value{ .nil = true });
                }
            },

            .print => |print| {
                debugPrint(self, "Evaluating print statement...\n", .{});
                const value = try evaluate(self, allocator, print.expr);
                debug.print("{s}\n", .{ value.toString(allocator) });
            },
            .expr => |expr| {
                debugPrint(self, "Evaluating expression...\n", .{});
                _ = try evaluate(self, allocator, expr.expr);
            },
        }
    }
};

fn evaluate(self: *const Interpreter, allocator: Allocator, expr: *const ast.Expr) (EvaluationError || RuntimeError)!Value {
    switch (expr.*) {
        .binary => |binary| {
            return try evaluateBinaryExpr(self, allocator, &binary);
        },
        .unary => |unary| {
            return try evaluateUnaryExpr(self, allocator, &unary);
        },
        .grouping => |group| {
            return try evaluate(self, allocator, group.inner);
        },
        .literal => |literal| {
            return literal.evaluate(allocator);
        },

        // NOTE(yemon): Not sure how to do with error handling here,
        // this switch branch know everything that could go wrong with the variable, 
        // but since the function itself is error-returned, the branch cannot choose
        // to return nothing if the error is already handled locally.
        // Other info related to the error (like 'lexeme') cannot be passed back
        // to the caller.
        .variable => |variable| {
            const name = variable.lexeme.?;
            return self.env.get(name) catch |err| switch (err) {
                RuntimeError.UndefinedVariable => {
                    debug.print("Undefined variable '{s}'.\n", .{ name });
                    return Value{ .nil = true };
                },
                else => return err,
            };
        },
    }
}

fn evaluateBinaryExpr(self: *const Interpreter, allocator: Allocator, binary: *const ast.BinaryExpr) (EvaluationError || RuntimeError)!Value {
    const left_value = try evaluate(self, allocator, binary.left);
    const right_value = try evaluate(self, allocator, binary.right);

    switch (binary.optr.token_type) { 
        .Minus => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return error.InvalidBinaryOperands;
            }
            return try left_value.doArithmetic(right_value, .substract);
        },
        .Slash => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return error.InvalidBinaryOperands;
            }
            return try left_value.doArithmetic(right_value, .divide);
        },
        .Star => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return error.InvalidBinaryOperands;
            }
            return try left_value.doArithmetic(right_value, .multiply);
        },

        .Plus => {
            switch (left_value) {
                .integer, .double => {
                    if (!left_value.isNumber() or !right_value.isNumber()) {
                        return error.InvalidBinaryOperands;
                    }
                    return try left_value.doArithmetic(right_value, .addition);
                },
                .string => |left_string| {
                    switch (right_value) {
                        .string => |right_string| {
                            return concatStrings(allocator, left_string, right_string) catch {
                                return error.StringConcatFailed;
                            };
                        },
                        else => {
                            return error.InvalidStringOperand;
                        }
                    }
                },
                else => {
                    return error.InvalidArithmeticOperand;
                }
            }
        },

        .BangEqual => {
            const is_equal = left_value.isEqual(right_value) catch false;
            return Value{ .boolean = !is_equal };
        },
        .EqualEqual => {
            const is_equal = left_value.isEqual(right_value) catch false;
            return Value{ .boolean = is_equal };
        },

        else => {
            return error.UnknownBinaryOperation;
        }
    }
}

fn evaluateUnaryExpr(self: *const Interpreter, allocator: Allocator, unary: *const ast.UnaryExpr) (EvaluationError || RuntimeError)!Value {
    const value = try evaluate(self, allocator, unary.right);

    switch (unary.optr.token_type) {
        .Minus => {
            if (!value.isNumber()) {
                return error.InvalidUnaryOperand;
            }

            // NOTE(yemon): For this kind of situations, I'd really like to define 
            // my own casting operator (similar to @as) like:
            //     @unionCast(i64, Value, value),
            // where the operator returns only if one of the fields of the second
            // argument, the union type, holds the valid target data type given as the
            // first argument. Otherwise, it'll throw an error.
            // I don't need as far as type inferrance, explicit casting/function calling
            // is fine. Basic idea could be to use some sort of reflection on the union
            // type's fields to see whether the two types are the same.
            switch (value) {
                .integer => |it| {
                    return Value{ .integer = -it };
                },
                .double => |it| {
                    return Value{ .double = -it };
                },
                else => {
                    return error.InvalidValueTypeToNegate;
                }
            }
        },

        .Bang => {
            return Value{ .boolean = !value.isTruthy() };
        },

        else => {
            return value;
        }
    }
}

const Environment = struct {
    values: std.StringHashMap(Value),

    fn init(allocator: Allocator) Environment {
        return .{
            .values = std.StringHashMap(Value).init(allocator),
        };
    }

    fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
    }

    fn get(self: *const Environment, name: []const u8) !Value {
        if (self.values.get(name)) |value| {
            return value;
        } else {
            return RuntimeError.UndefinedVariable;
        }
    }

    fn display(self: *Environment, allocator: Allocator) void {
        debug.print("[Env: ", .{});
        var iter = self.values.iterator();
        while (iter.next()) |entry| {
            const key = entry.key_ptr.*;
            const value = entry.value_ptr.*;
            debug.print("{s}={s} | ", .{ key, value.toString(allocator) });
        }
        debug.print("]\n", .{});
    }
};

fn debugPrint(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
    if (!self.debug_print) {
        return;
    }
    debug.print(fmt, args);
}
