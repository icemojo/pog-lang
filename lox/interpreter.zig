const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const ast = @import("ast.zig");

const ArithmeticOp = enum {
    substract,
    divide,
    multiply,
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

    fn doArithmetic(self: Value, rhs: Value, op: ArithmeticOp) !Value {
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
                        }
                    },
                    .double => |right_double| {
                        const right_value: i64 = @intFromFloat(right_double);
                        switch (op) {
                            .substract => return Value{ .integer = left_int - right_value },
                            .divide => return Value{ .integer = @divFloor(left_int, right_value) },
                            .multiply => return Value{ .integer = left_int * right_value },
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
                        }
                    },
                    .double => |right_double| {
                        switch (op) {
                            .substract => return Value{ .double = left_double - right_double },
                            .divide => return Value{ .double = left_double / right_double },
                            .multiply => return Value{ .double = left_double * right_double },
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
};

const EvaluationError = error {
    InvalidArithmeticOperand,
    InvalidValueTypeToNegate,
    NotDoneYet,

    OutOfMemory,
};

pub fn evaluate(expr: *const ast.Expr, allocator: Allocator) EvaluationError!Value {
    switch (expr.*) {
        .binary => |binary| {
            return try evaluateBinaryExpr(&binary, allocator);
        },
        .unary => |unary| {
            return try evaluateUnaryExpr(&unary, allocator);
        },
        .grouping => |group| {
            return try evaluate(group.inner, allocator);
        },
        .literal => |literal| {
            return try literal.evaluate(allocator);
        },
    }
}

fn evaluateBinaryExpr(binary: *const ast.BinaryExpr, allocator: Allocator) EvaluationError!Value {
    const left_value = try evaluate(binary.left, allocator);
    const right_value = try evaluate(binary.right, allocator);

    switch (binary.optr.token_type) { 
        .Minus => {
            return try left_value.doArithmetic(right_value, .substract);
        },
        .Slash => {
            return try left_value.doArithmetic(right_value, .divide);
        },
        .Star => {
            return try left_value.doArithmetic(right_value, .multiply);
        },

        else => {
            return error.NotDoneYet;
        }
    }
}

fn evaluateUnaryExpr(unary: *const ast.UnaryExpr, allocator: Allocator) !Value {
    const value = try evaluate(unary.right, allocator);

    switch (unary.optr.token_type) {
        .Minus => {
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