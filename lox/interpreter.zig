const std = @import("std");
const debug = @import("std").debug;
const log = @import("std").log;
const Allocator = @import("std").mem.Allocator;

const report = @import("report.zig");
const ast = @import("ast.zig");
const LoxFunction = @import("lox_callable.zig").LoxFunction;

const ArithmeticOp = enum {
    substract,
    divide,
    multiply,
    addition,
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

    fn doArithmetic(self: Value, rhs: Value, op: ArithmeticOp) ValueError!Value {
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

    fn getTypeName(self: Value) []const u8 {
        return switch (self) {
            .integer => "integer",
            .double => "double",
            .string => "string",
            .boolean => "boolean",
            .nil => "nil",
        };
    }
};

fn concatStrings(allocator: Allocator, string1: []u8, string2: []u8) Value {
    const target_string = std.fmt.allocPrint(allocator, "{s}{s}", .{ string1, string2 })
        catch string1[0..];
    return Value{ .string = target_string };
}

// NOTE(yemon): Runtime errors should probably return, attached with a proper message
// since they should probably be presented and visible to the user.
const RuntimeError = error {
    UndefinedIdentifier,
    AlreadyDefinedVariable,
    AlreadyDefinedFunction,
};

const Self = @This();

debug_print: bool,
debug_env: bool,
global_env: *Environment,
env: *Environment,
caller_depth: i32,
current_depth: i32,

pub fn init(allocator: Allocator) Self {
    const global_env = Environment.init(allocator, null);
    var interpreter = Self{
        .debug_print = false,
        .debug_env = false,
        .global_env = global_env,
        .env = global_env,
        .caller_depth = -1,
        .current_depth = -1,
    };
    interpreter.initBuiltins(allocator) catch unreachable;
    return interpreter;
}

// TODO(yemon): `__env__`, `__name__`, `print()`, `clock()` so on and so forth...
fn initBuiltins(self: *Self, allocator: Allocator) !void {
    _ = self;
    _ = allocator;
    // `clock_func` should return this when called:
    // `(double)System.currentTimeMillis() / 1000.0;` with arity 0
    // const clock_func = LoxCallable().init(allocator, "clock");
    // try self.global_env.defineFunction("clock", clock_func);
}

fn evaluateStatement(
    self: *Self, allocator: Allocator, stmt: *const ast.Stmt
) EvaluateResult {
    switch (stmt.*) {
        .expr_stmt => |expr| {
            self.debugPrint("Evaluating expression statement...\n", .{});
            return self.evaluate(allocator, expr.expr);
        },

        .block_stmt => |block| {
            self.debugPrint("Evaluating a block statement (depth: {})...\n", .{ 
                self.current_depth 
            });
            const block_eval = self.executeBlock(allocator, block.statements);

            self.debugPrint("  Block got '{s}' result. (depth: {})\n", .{ 
                block_eval.getTypeName(), self.current_depth
            });

            return block_eval;
        },

        .print_stmt => |print| {
            self.debugPrint("Evaluating print statement...\n", .{});
            const eval = self.evaluate(allocator, print.expr);
            const value = eval.getExprValue();
            debug.print("{s}\n", .{ value.toString(allocator) });
            return .{ .no_return = true };
        },

        .if_stmt => |if_stmt| {
            self.debugPrint("Evaluating if statement...\n", .{});
            const eval = self.evaluate(allocator, if_stmt.condition);
            const condition = eval.getExprValue();

            var if_eval_result: EvaluateResult = undefined;
            if (condition.isTruthy()) {
                self.debugPrint("TRUE... \n", .{});
                if_eval_result = self.evaluateStatement(allocator, if_stmt.then_branch);
            } else {
                self.debugPrint("  FALSE... \n", .{});
                if (if_stmt.else_branch) |else_branch| {
                    self.debugPrint("  else branch...\n", .{});
                    if_eval_result = self.evaluateStatement(allocator, else_branch);
                } else {
                    if_eval_result = .{ .no_return = true };
                }
            }
            return if_eval_result;
        },

        .while_stmt => |while_stmt| {
            self.debugPrint("Evaluating while statement block...\n", .{});
            var eval = self.evaluate(allocator, while_stmt.condition);
            var condition = eval.getExprValue();

            const loop_eval_result: EvaluateResult = loop: while (condition.isTruthy()) {
                const eval_result = self.evaluateStatement(allocator, while_stmt.body);
                eval = self.evaluate(allocator, while_stmt.condition);
                condition = eval.getExprValue();

                // NOTE(yemon): there's no early `break` from the loops... yet
                // so only the function return can break the loop body early entirely
                switch (eval_result) {
                    .func_return => |func_return| {
                        break :loop .{ .func_return = func_return };
                    },
                    .error_return => {
                        break .{ .error_return = true };
                    },
                    .expr_value, .no_return => {
                        continue :loop;
                    }
                }
            } else .{ .no_return = true };
            return loop_eval_result;
        },

        .variable_declare_stmt => |variable_declare| {
            self.debugPrint("Evaluating variable declaration statement...\n", .{});
            if (variable_declare.initializer) |initializer| {
                const eval = self.evaluate(allocator, initializer);
                const value = eval.getExprValue();
                self.env.*.define(variable_declare.name, value) catch {
                    report.runtimeError("Variable declaration failed for unknown reason.");
                };
            } else {
                self.env.*.define(variable_declare.name, Value{ .nil = true }) catch {
                    report.runtimeError("Variable declaration failed for unknown reason.");
                };
            }
            return .{ .no_return = true };
        },

        .func_declare_stmt => |func_declare| {
            var name: []const u8 = undefined;
            if (func_declare.name.lexeme) |lexeme| {
                name = lexeme;
            } else {
                report.runtimeError("Unable to declare a function without proper identifier name.");
                return .{ .error_return = true };
            }

            self.debugPrint("Evaluating the function declaration statement '{s}'...\n", .{ name });

            const function = LoxFunction.init(&func_declare);
            self.env.*.defineFunction(name, function) catch {
                report.runtimeError("Function declaration failed for unknown reason.");
            };
            if (self.debug_env) {
                self.env.*.display(allocator);
            }
            return .{ .no_return = true };
        },

        .return_stmt => |return_stmt| {
            self.debugPrint("Evaluating a return statement... caller_depth: {}\n", .{
                self.caller_depth
            });
            if (return_stmt.expr) |expr| {
                const return_eval = self.evaluate(allocator, expr);
                const value = return_eval.getExprValue();
                return .{ 
                    .func_return = .{
                        .value = value,
                        .caller_depth = self.caller_depth,
                    },
                };
            } else {
                return .{
                    .func_return = .{
                        .value = Value{ .nil = true },
                        .caller_depth = self.caller_depth,
                    },
                };
            }
        },
    }
}

// `expr_value` should be handled in place at the caller
// `func_return` and/or `error_return` indicates the control flow between the statements
//  - `func_return` should be used to decide whether the enclosing block should continue
//     or break out. Generally, only the `return_stmt` variant should produce this.
//  - `error_return` should end the evaluation and execution altogether
// `no_return` will just let the evaluations continue to the next statement
pub const EvaluateResult = union(enum) {
    expr_value: Value,
    func_return: FuncReturn,
    error_return: bool,  // NOTE(yemon): should this carry additional info, like an err msg?
    no_return: bool,

    fn getExprValue(self: EvaluateResult) Value {
        switch (self) {
            .expr_value => |value| {
                return value;
            },
            else => {
                return Value{ .nil = true };
            }
        }
    }

    pub fn getIfFuncReturn(self: EvaluateResult) ?FuncReturn {
        switch (self) {
            .func_return => |it| {
                return it;
            },
            else => {
                return null;
            }
        }
    }

    fn isErrorReturn(self: EvaluateResult) bool {
        return switch (self) {
            .error_return => true,
            else => false,
        };
    }

    fn getTypeName(self: EvaluateResult) []const u8 {
        return switch (self) {
            .expr_value => "expr_value",
            .func_return => "func_return",
            .error_return => "error_return",
            .no_return => "no_return",
        };
    }
};

const FuncReturn = struct {
    value: Value,
    caller_depth: i32,
};

// TODO(yemon): Reporting errors in place is easy enough to implement. But I don't think
// it's going to be really useful in big code files if the "stack trace" (or at least
// the token where the error occured) cannot be described when error occured.
fn evaluate(
    self: *Self, allocator: Allocator, expr: *const ast.Expr
) EvaluateResult {
    switch (expr.*) {
        .assign => |assignment| {
            self.debugPrint("  Evaluating assignment expression...\n", .{});
            self.evaluateAssignmentExpr(allocator, &assignment) catch {
                report.runtimeErrorAlloc(allocator,
                    "Unable to assign value to a variable '{s}'.", .{
                        assignment.name
                    });
                return .{ .error_return = true };
            };
            return .{ .no_return = true };
        },

        .binary => |binary| {
            self.debugPrint("  Evaluating binary expression...\n", .{});
            const value = self.evaluateBinaryExpr(allocator, &binary);
            return .{
                .expr_value = value,
            };
        },

        .logical => |logical| {
            self.debugPrint("  Evaluating logical expression...\n", .{});
            const value = self.evaluateLogicalExpr(allocator, &logical);
            return .{
                .expr_value = value,
            };
        },

        .unary => |unary| {
            self.debugPrint("  Evaluating unary expression...\n", .{});
            const value = self.evaluateUnaryExpr(allocator, &unary);
            return .{
                .expr_value = value,
            };
        },

        .grouping => |group| {
            self.debugPrint("  Evaluating grouping expression...\n", .{});
            return self.evaluate(allocator, group.inner);
        },

        .literal => |literal| {
            self.debugPrint("  Evaluating literal expression...\n", .{});
            const value = literal.evaluate(allocator);
            return .{
                .expr_value = value,
            };
        },

        .variable => |variable| {
            const name = variable.lexeme.?;
            self.debugPrint("  Evaluating variable expression '{s}'...\n", .{ name });

            const env_value = self.env.*.getValue(name) catch {
                report.runtimeErrorAlloc(allocator, "Undefined variable '{s}'.", .{ name });
                return .{
                    .expr_value = Value{ .nil = true },
                };
            };

            switch (env_value) {
                .value => |value| {
                    self.debugPrint("  Env Value: {s}\n", .{ value.toString(allocator) });
                    return .{
                        .expr_value = value,
                    };
                },
                else => {
                    report.runtimeError("Invalid identifier access.");
                    // NOTE(yemon): not sure about this yet!
                    return .{ .no_return = true };
                }
            }
        },

        .func_call => |func_call| {
            self.debugPrint("  Evaluating a function call expression :: ", .{});
            if (self.debug_print) {
                func_call.display(false);
                debug.print("\n", .{});
            }

            self.debugPrint("  Function callee expr: ", .{});
            if (self.debug_print) {
                func_call.callee.*.display(true);
            }

            self.caller_depth = self.current_depth;
            const func_eval_result = self.evaluateFunctionCallExpr(allocator, &func_call);
            self.debugPrint("Finished function call expression :: ", .{});
            if (self.debug_print) {
                func_call.display(false);
                debug.print("\n", .{});
            }

            return func_eval_result;
        },
    }
}

fn evaluateAssignmentExpr(
    self: *Self, allocator: Allocator, 
    assignment: *const ast.AssignmentExpr
) !void {
    const eval = self.evaluate(allocator, assignment.value);
    const value = eval.getExprValue();

    try self.env.*.assign(assignment.name, value);
}

fn evaluateBinaryExpr(
    self: *Self, allocator: Allocator, 
    binary: *const ast.BinaryExpr
) Value {
    const left_eval = self.evaluate(allocator, binary.left);
    const left_value = left_eval.getExprValue();

    const right_eval = self.evaluate(allocator, binary.right);
    const right_value = right_eval.getExprValue();

    switch (binary.optr.token_type) { 
        .Minus => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                report.arithmeticError(allocator, "Substraction has to be between numbers.", 
                    left_value.getTypeName(), right_value.getTypeName()
                );
                return Value{ .nil = true };
            }

            const result = left_value.doArithmetic(right_value, .substract)
                catch {
                    report.arithmeticError(allocator, "Unable to do substraction.", 
                        left_value.getTypeName(), right_value.getTypeName()
                    );
                    return Value{ .nil = true };
                };
            return result;
        },

        .Slash => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                report.arithmeticError(allocator, "Division has to be between numbers.", 
                    left_value.getTypeName(), right_value.getTypeName()
                );
                return Value{ .nil = true };
            }

            const result = left_value.doArithmetic(right_value, .divide)
                catch {
                    report.arithmeticError(allocator, "Unable to do division.",
                        left_value.getTypeName(), right_value.getTypeName()
                    );
                    return Value{ .nil = true };
                };
            return result;
        },

        .Star => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                report.arithmeticError(allocator, "Multiplication has to be between numbers.", 
                    left_value.getTypeName(), right_value.getTypeName()
                );
                return Value{ .nil = true };
            }

            const result = left_value.doArithmetic(right_value, .multiply)
                catch {
                    report.arithmeticError(allocator, "Unable to do multiplication.",
                        left_value.getTypeName(), right_value.getTypeName()
                    );
                    return Value{ .nil = true };
                };
            return result;
        },

        .Plus => {
            switch (left_value) {
                .integer, .double => {
                    if (!left_value.isNumber() or !right_value.isNumber()) {
                        report.arithmeticError(allocator, "Addition has to be between numbers.", 
                            left_value.getTypeName(), right_value.getTypeName()
                        );
                        return Value{ .nil = true };
                    }

                    const result = left_value.doArithmetic(right_value, .addition)
                        catch {
                            report.arithmeticError(allocator, "Unable to do addition.",
                                left_value.getTypeName(), right_value.getTypeName()
                            );
                            return Value{ .nil = true };
                        };
                    return result;
                },
                .string => |left_string| {
                    switch (right_value) {
                        .string => |right_string| {
                            return concatStrings(allocator, left_string, right_string);
                        },
                        else => {
                            report.runtimeError("A string can only be concated with " ++
                                "another string."
                            );
                            return Value{ .string = left_string };
                        }
                    }
                },
                else => {
                    report.runtimeError("Invalid operand types to do an addition.");
                    return Value{ .nil = true };
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

        .Less => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                report.comparisonError(allocator, 
                    left_value.getTypeName(), right_value.getTypeName()
                );
                return Value{ .nil = true };
            }

            return left_value.doComparison(right_value, .lesser) 
                catch Value{ .boolean = false };
        },
        .LessEqual => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                report.comparisonError(allocator, 
                    left_value.getTypeName(), right_value.getTypeName()
                );
                return Value{ .nil = true };
            }

            return left_value.doComparison(right_value, .lesser_equal)
                catch Value{ .boolean = false };
        },

        .Greater => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                report.comparisonError(allocator, 
                    left_value.getTypeName(), right_value.getTypeName()
                );
                return Value{ .nil = true };
            }

            return left_value.doComparison(right_value, .greater) 
                catch Value{ .boolean = false };
        },
        .GreaterEqual => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                report.comparisonError(allocator, 
                    left_value.getTypeName(), right_value.getTypeName()
                );
                return Value{ .nil = true };
            }

            return left_value.doComparison(right_value, .greater_equal)
                catch Value{ .boolean = false };
        },

        else => {
            report.runtimeError("Unknown binary operation.");
            return Value{ .nil = true };
        }
    }
}

fn evaluateLogicalExpr(
    self: *Self, allocator: Allocator, 
    logical: *const ast.LogicalExpr
) Value {
    const left_eval = self.evaluate(allocator, logical.left);
    const left_value = left_eval.getExprValue();

    if (logical.optr.token_type == .Or) {
        if (left_value.isTruthy()) {
            return left_value;
        }
    } else {
        if (!left_value.isTruthy()) {
            return left_value;
        }
    }

    const right_eval = self.evaluate(allocator, logical.right);
    return right_eval.getExprValue();
}

fn evaluateUnaryExpr(
    self: *Self, allocator: Allocator, 
    unary: *const ast.UnaryExpr
) Value {
    const eval = evaluate(self, allocator, unary.right);
    const value = eval.getExprValue();

    switch (unary.optr.token_type) {
        .Minus => {
            if (!value.isNumber()) {
                report.unaryError(allocator, value.getTypeName());
                return value;
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
                    report.unaryError(allocator, value.getTypeName());
                    return value;
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

fn evaluateFunctionCallExpr(
    self: *Self, allocator: Allocator, 
    func_call: *const ast.FunctionCallExpr,
) EvaluateResult {
    const callee: ast.Expr = func_call.callee.*;
    switch (callee) {
        .variable => |variable| {
            const name = variable.lexeme orelse unreachable;
            const env_value = self.env.*.getValue(name) catch {
                report.runtimeErrorAlloc(allocator, "Undefined function '{s}'.", .{ name });
                return .{ .error_return = true };
            };

            switch (env_value) {
                .function => |lox_function| {
                    const evaluated_args = self.evaluateFunctionArguments(
                        allocator, func_call
                    );
                    defer {
                        // NOTE(yemon): Probably won't need this if the interpreter
                        // is using an arena.
                        if (evaluated_args) |args| args.deinit();
                    }

                    const args_count = if (evaluated_args) |args| 
                        @as(usize, args.items.len) else 0;
                    self.debugPrint("  Function arity: {d}, args_count: {d}\n", .{ 
                        lox_function.arity(), args_count 
                    });
                    if (lox_function.arity() != args_count) {
                        report.runtimeErrorAlloc(allocator,
                            "Expecting {} arguments in the function call, received {}.", .{
                                lox_function.arity(), args_count
                            });
                        return .{ .error_return = true };
                    }

                    const func_eval_result = lox_function.call(
                        allocator, self, evaluated_args
                    );
                    return func_eval_result;
                },
                .value => |value| {
                    report.runtimeErrorAlloc(allocator, 
                        "The value '{s}' is not callable as a function.", .{
                            value.toString(allocator)
                        });
                    return .{ .error_return = true };
                }
            }
        },

        else => {
            report.runtimeErrorAlloc(allocator, 
                "Unable to resolve the '{s}' expression as a function call.", .{
                    callee.getTypeName()
                });
            return .{ .error_return = true };
        }
    }
}

fn evaluateFunctionArguments(
    self: *Self, allocator: Allocator, 
    func_call: *const ast.FunctionCallExpr
) ?std.ArrayList(Value) {
    self.debugPrint("  Evaluating function arguments...\n", .{});
    if (func_call.arguments) |args| {
        var evaluated_args = std.ArrayList(Value).init(allocator);
        for (args.items) |arg| {
            const eval = self.evaluate(allocator, arg);
            const arg_value = eval.getExprValue();
            if (arg_value.isNil()) {
                continue;
            }

            self.debugPrint("   -> {s}\n", .{ arg_value.toString(allocator) });
            evaluated_args.append(arg_value) catch unreachable;
        }
        return evaluated_args;
    } else {
        return null;
    }
}

pub fn executeBlock(
    self: *Self, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt)
) EvaluateResult {
    self.current_depth += 1;
    const parent_env = self.env;
    const block_env = Environment.init(allocator, self.env);
    defer allocator.destroy(block_env);

    self.debugPrint("  >> executeBlock():\n", .{});
    self.env = block_env;

    var block_eval: EvaluateResult = undefined;
    control: for (statements.items) |stmt| {
        block_eval = self.evaluateStatement(allocator, stmt);

        switch (block_eval) {
            .func_return => |func_return| {
                self.debugPrint("  >> Statement received a 'func_return', " ++ 
                    "likely produced by a `return_stmt`. " ++
                    "(Return value: {s}, caller_depth: {})\n", .{
                        func_return.value.toString(allocator), func_return.caller_depth
                    });
                break :control;
            },
            .error_return => {
                self.debugPrint("  >> Statement evaluation returned with an error. " ++ 
                    "This should disrupt the entire execution altogether.\n", .{});
                break :control;
            },
            .no_return, .expr_value => {
                continue :control;
            },
        }
    }
    self.env = parent_env;

    self.debugPrint("  >> caller_depth: {}, current_depth: {}\n", .{ 
        self.caller_depth, self.current_depth
    });
    self.debugPrint("  >> executeBlock() done!\n", .{});

    if (self.debug_env) {
        self.env.*.display(allocator);
    }

    self.current_depth -= 1;
    return block_eval;
}

pub fn executeBlockEnv(
    self: *Self, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt), 
    with_env: *Environment
) EvaluateResult {
    self.current_depth += 1;
    const parent_env = self.env;

    self.debugPrint("  >> executeBlockEnv():\n", .{});
    self.env = with_env;

    var block_eval: EvaluateResult = undefined;
    control: for (statements.items) |stmt| {
        block_eval = self.evaluateStatement(allocator, stmt);

        switch (block_eval) {
            .func_return => |func_return| {
                self.debugPrint("  >> Statement received a `func_return`, " ++ 
                    "likely produced by a `return_stmt`. " ++
                    "(Return value: {s}, caller_depth: {})\n", .{
                        func_return.value.toString(allocator), func_return.caller_depth
                    });
                break :control;
            },
            .error_return => {
                self.debugPrint("  >> Statement evaluation returned with an error. " ++ 
                    "This should disrupt the entire execution altogether.", .{});
                break :control;
            },
            .no_return, .expr_value => {
                continue :control;
            }
        }
    }
    self.env = parent_env;
    
    self.debugPrint("  >> caller_depth: {}, current_depth: {}\n", .{ 
        self.caller_depth, self.current_depth
    });
    self.debugPrint("  >> executeBlockEnv() done!\n", .{});

    if (self.debug_env) {
        self.env.*.display(allocator);
    }

    self.current_depth -= 1;
    return block_eval;
}

pub const EnvValue = union(enum) {
    value: Value,
    function: LoxFunction,

    fn toString(self: EnvValue, allocator: Allocator) []const u8 {
        switch (self) {
            .value => |value| {
                return value.toString(allocator);
            },
            .function => |function| {
                return function.toString(allocator);
            },
        }
    }
};

// It's kinda rare in for language to have both the variables and function declarations
// to NOT live in the same namespace. (e.g., Common Lisp). 
// If the functions need to be "first-class citizens", they both have to exist 
// in the same namespace.
pub const Environment = struct {
    enclosing: ?*Environment,
    values: std.StringHashMap(EnvValue),

    pub fn init(allocator: Allocator, enclosing: ?*Environment) *Environment {
        const env = allocator.create(Environment) catch unreachable;
        env.*.enclosing = enclosing;
        env.*.values = std.StringHashMap(EnvValue).init(allocator);
        return env;
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        if (self.alreadyDefined(name)) {
            return RuntimeError.AlreadyDefinedVariable;
        }
        try self.values.put(name, EnvValue{
            .value = value,
        });
    }

    fn defineFunction(self: *Environment, name: []const u8, function: LoxFunction) !void {
        if (self.alreadyDefined(name)) {
            return RuntimeError.AlreadyDefinedFunction;
        }
        try self.values.put(name, EnvValue{
            .function = function,
        });
    }

    fn assign(self: *Environment, name: []const u8, value: Value) !void {
        if (!self.alreadyDefined(name)) {
            if (self.enclosing) |enclosing| {
                return try enclosing.*.assign(name, value);
            } else {
                // NOTE(yemon): Already at the top most (global) scope
                return RuntimeError.UndefinedIdentifier;
            }
        }
        try self.values.put(name, EnvValue{ 
            .value = value,
        });
    }

    fn getValue(self: *const Environment, name: []const u8) !EnvValue {
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

    fn display(self: *const Environment, allocator: Allocator) void {
        debug.print("[Env: ", .{});
        var iter = self.values.iterator();
        while (iter.next()) |entry| {
            const key = entry.key_ptr.*;
            const value = entry.value_ptr.*;
            debug.print("{s}={s} | ", .{ key, value.toString(allocator) });
        }
        debug.print("]", .{});
        if (self.enclosing) |enclosing| {
            debug.print(" ", .{});
            enclosing.*.display(allocator);
        } else {
            debug.print(" [-]\n", .{});
        }
    }
};

fn debugPrint(self: *const Self, comptime fmt_msg: []const u8, args: anytype) void {
    if (!self.debug_print) {
        return;
    }
    debug.print("[DEPTH {}]   ", .{ self.current_depth });
    debug.print(fmt_msg, args);
}
