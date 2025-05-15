const std = @import("std");
const debug = @import("std").debug;
const log = @import("std").log;
const Allocator = @import("std").mem.Allocator;

const report = @import("report.zig");
const ast = @import("ast.zig");
const Value = @import("value.zig").Value;
const LoxFunction = @import("lox_callable.zig").LoxFunction;

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
    DefinitionFailed,
    EvaluationFailed,
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
    _ = allocator;
    // `clock_func` should return this when called:
    // `(double)System.currentTimeMillis() / 1000.0;` with arity 0
    // const clock_func = LoxCallable().init(allocator, "clock");
    // try self.global_env.defineFunction("clock", clock_func);

    const test_name: []u8 = @constCast("test.lox");
    try self.global_env.*.define("__name__", Value{ .string = test_name });
}

fn evaluateStatement(
    self: *Self, allocator: Allocator, stmt: *const ast.Stmt
) EvaluateResult {
    switch (stmt.*) {
        .expr_stmt => |expr_stmt| {
            self.debugPrint("Evaluating expression statement...\n", .{});
            return self.evaluate(allocator, expr_stmt.expr);
        },

        .block_stmt => |block_stmt| {
            self.debugPrint("Evaluating a block statement (depth: {})...\n", .{ 
                self.current_depth 
            });
            const block_eval = self.executeBlock(allocator, block_stmt.statements);

            self.debugPrint("  Block got '{s}' result. (depth: {})\n", .{ 
                block_eval.getTypeName(), self.current_depth
            });

            return block_eval;
        },

        .print_stmt => |print_stmt| {
            self.debugPrint("Evaluating print statement...\n", .{});
            const eval = self.evaluate(allocator, print_stmt.expr);
            if (eval.isErrorReturn()) {
                return eval;
            }
            const value = eval.getExprOrFuncReturnValue();
            debug.print("{s}\n", .{ value.toString(allocator, false) });
            return .{ .no_return = true };
        },

        .compound_stmt => |compound_stmt| {
            self.debugPrint("Evaluating the compound statement...\n", .{});

            const name = compound_stmt.identifier;
            const value = self.env.*.getValue(name) catch {
                report.runtimeErrorAlloc(
                    allocator, 
                    "Invalid identifier '{s}' for the compound statement target.", .{ name }
                );
                return .{ .error_return = true };
            };

            const allow_arithmetic = allow: switch (value) {
                .integer, .double => break :allow true,
                else => break :allow false,
            };
            if (!allow_arithmetic) {
                report.runtimeErrorAlloc(allocator, 
                    "Unable to do an arithmetic operation on '{s}'.", .{ @tagName(value) }
                );
                return .{ .error_return = true };
            }

            const right_eval = self.evaluate(allocator, compound_stmt.expr);
            if (right_eval.isErrorReturn()) {
                return right_eval;
            }

            const right_value = right_eval.getExprOrFuncReturnValue();
            const op_msg = std.fmt.allocPrint(allocator, "Unable to do {s}.", .{ 
                    compound_stmt.optr.toString()
                }) catch {
                    return .{ .error_return = true };
                };
            defer allocator.free(op_msg);

            const result = value.doArithmetic(right_value, compound_stmt.optr) catch {
                report.runtimeErrorAlloc(allocator, "Unable to do {s}.", .{
                    compound_stmt.optr.toString()
                });
                return .{ .error_return = true };
            };

            self.env.*.assign(name, result) catch {
                return .{ .error_return = true };
            };
            return .{ .no_return = true };
        },

        .if_stmt => |if_stmt| {
            self.debugPrint("Evaluating if statement...\n", .{});
            const eval = self.evaluate(allocator, if_stmt.condition);
            if (eval.isErrorReturn()) {
                return eval;
            }
            const condition = eval.getExprOrFuncReturnValue();

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
            if (eval.isErrorReturn()) {
                return eval;
            }
            var condition = eval.getExprOrFuncReturnValue();

            const loop_eval_result: EvaluateResult = loop: while (condition.isTruthy()) {
                const eval_result = self.evaluateStatement(allocator, while_stmt.body);
                eval = self.evaluate(allocator, while_stmt.condition);
                if (eval.isErrorReturn()) {
                    return eval;
                }
                condition = eval.getExprOrFuncReturnValue();

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

        .variable_declare_stmt => |variable_declare_stmt| {
            self.debugPrint("Evaluating variable declaration statement...\n", .{});
            const name = variable_declare_stmt.name;
            if (variable_declare_stmt.initializer) |initializer| {
                const eval = self.evaluate(allocator, initializer);
                if (eval.isErrorReturn()) {
                    return eval;
                }

                const value: Value = eval: switch (eval) {
                    .expr_value => |expr_value| {
                        break :eval expr_value;
                    },
                    .func_return => |func_return| {
                        break :eval func_return.value;
                    },
                    else => {
                        break :eval .{ .nil = true };
                    },
                };

                self.env.*.define(name, value) catch |err| {
                    switch (err) {
                        RuntimeError.AlreadyDefinedVariable => {
                            report.runtimeErrorAlloc(allocator, 
                                "Variable identifier of name '{s}' already exists.", .{ name }
                            );
                        },
                        else => {
                            report.runtimeError("Variable declaration failed for unknown reason.");
                        },
                    }
                    return .{ .error_return = true };
                };
            } else {
                self.env.*.define(name, Value{ .nil = true }) catch |err| {
                    switch (err) {
                        RuntimeError.AlreadyDefinedVariable => {
                            report.runtimeErrorAlloc(allocator, 
                                "Variable identifier of name '{s}' already exists.", .{ name }
                            );
                        },
                        else => {
                            report.runtimeError("Variable declaration failed for unknown reason.");
                        },
                    }
                    return .{ .error_return = true };
                };
            }
            return .{ .no_return = true };
        },

        .func_declare_stmt => |func_declare_stmt| {
            var name: []const u8 = undefined;
            if (func_declare_stmt.name.lexeme) |lexeme| {
                name = lexeme;
            } else {
                report.runtimeError("Unable to declare a function without proper identifier name.");
                return .{ .error_return = true };
            }

            self.debugPrint("Evaluating the function declaration statement '{s}'...\n", .{ name });

            const function = LoxFunction.init(func_declare_stmt);
            self.env.*.defineFunction(name, function) catch {
                report.runtimeError("Function declaration failed for unknown reason.");
            };
            return .{ .no_return = true };
        },

        .return_stmt => |return_stmt| {
            self.debugPrint("Evaluating a return statement... caller_depth: {}\n", .{
                self.caller_depth
            });
            if (return_stmt.expr) |expr| {
                const return_eval = self.evaluate(allocator, expr);
                if (return_eval.isErrorReturn()) {
                    return return_eval;
                }
                const value = return_eval.getExprOrFuncReturnValue();
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

    fn getExprOrFuncReturnValue(self: EvaluateResult) Value {
        switch (self) {
            .expr_value => |value| {
                return value;
            },
            .func_return => |func_return| {
                return func_return.value;
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
            self.evaluateAssignmentExpr(allocator, &assignment) catch |err| switch (err) {
                RuntimeError.EvaluationFailed => {
                    return .{ .error_return = true };
                },
                else => {
                    report.runtimeErrorAlloc(allocator,
                        "Unable to assign value to an undefined variable '{s}'.", .{
                            assignment.identifier
                        });
                    return .{ .error_return = true };
                },
            };
            return .{ .no_return = true };
        },

        .binary => |binary| {
            self.debugPrint("  Evaluating binary expression...\n", .{});
            const value = self.evaluateBinaryExpr(allocator, &binary) catch {
                return .{ .error_return = true };
            };
            return .{ .expr_value = value };
        },

        .logical => |logical| {
            self.debugPrint("  Evaluating logical expression...\n", .{});
            const value = self.evaluateLogicalExpr(allocator, &logical) catch {
                return .{ .error_return = true };
            };
            return .{ .expr_value = value };
        },

        .unary => |unary| {
            self.debugPrint("  Evaluating unary expression...\n", .{});
            const value = self.evaluateUnaryExpr(allocator, &unary) catch {
                return .{ .error_return = true };
            };
            return .{ .expr_value = value };
        },

        .grouping => |group| {
            self.debugPrint("  Evaluating grouping expression...\n", .{});
            return self.evaluate(allocator, group.inner);
        },

        .literal => |literal| {
            self.debugPrint("  Evaluating literal expression...\n", .{});
            const value = literal.evaluate(allocator);
            return .{ .expr_value = value };
        },

        .variable => |variable| {
            const name = variable.lexeme.?;
            self.debugPrint("  Evaluating variable expression '{s}'...\n", .{ name });

            const value = self.env.*.getValue(name) catch {
                report.runtimeErrorAlloc(allocator, "Undefined variable '{s}'.", .{ name });
                return .{ .error_return = true };
            };

            switch (value) {
                .function => {
                    // NOTE(yemon): A 'function' won't be accessible like a variable
                    // for now. Might change later on when they're being treated as 1st class.
                    report.runtimeError("Invalid identifier access.");
                    return .{ .no_return = true };
                },
                else => {
                    return .{ .expr_value = value };
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
            self.debugPrint("  func_eval_result: {s}\n", .{ @tagName(func_eval_result) });

            return func_eval_result;
        },
    }
}

fn evaluateAssignmentExpr(
    self: *Self, allocator: Allocator, 
    assignment: *const ast.AssignmentExpr
) RuntimeError!void {
    const eval = self.evaluate(allocator, assignment.value);
    if (eval.isErrorReturn()) {
        self.debugPrint("  Assignment expression evaluation resulted in error.\n", .{});
        return RuntimeError.EvaluationFailed;
    }

    const value = eval.getExprOrFuncReturnValue();
    try self.env.*.assign(assignment.identifier, value);
}

fn evaluateBinaryExpr(
    self: *Self, allocator: Allocator, 
    binary: *const ast.BinaryExpr
) RuntimeError!Value {
    const left_eval = self.evaluate(allocator, binary.left);
    const right_eval = self.evaluate(allocator, binary.right);
    if (left_eval.isErrorReturn() or right_eval.isErrorReturn()) {
        return RuntimeError.EvaluationFailed;
    }

    const left_value = left_eval.getExprOrFuncReturnValue();
    const right_value = right_eval.getExprOrFuncReturnValue();

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
                            const right_string = right_value.toString(allocator, false);
                            return concatStrings(allocator, left_string, @constCast(right_string));
                        }
                    }
                },
                else => {
                    report.runtimeError("Invalid operand types to do an addition.");
                    return Value{ .nil = true };
                }
            }
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
) RuntimeError!Value {
    const left_eval = self.evaluate(allocator, logical.left);
    if (left_eval.isErrorReturn()) {
        return RuntimeError.EvaluationFailed;
    }
    const left_value = left_eval.getExprOrFuncReturnValue();

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
    if (right_eval.isErrorReturn()) {
        return RuntimeError.EvaluationFailed;
    }
    return right_eval.getExprOrFuncReturnValue();
}

fn evaluateUnaryExpr(
    self: *Self, allocator: Allocator, 
    unary: *const ast.UnaryExpr
) RuntimeError!Value {
    const eval = self.evaluate(allocator, unary.right);
    if (eval.isErrorReturn()) {
        return RuntimeError.EvaluationFailed;
    }
    const value = eval.getExprOrFuncReturnValue();

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
            const value = self.env.*.getValue(name) catch {
                report.runtimeErrorAlloc(allocator, "Undefined function '{s}'.", .{ name });
                return .{ .error_return = true };
            };

            switch (value) {
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
                else => {
                    report.runtimeErrorAlloc(allocator, 
                        "The value of type '{s}' is not callable as a function.", .{
                            @tagName(value)
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
            const arg_value = eval.getExprOrFuncReturnValue();
            if (arg_value.isNil()) {
                continue;
            }

            self.debugPrint("   -> {s}\n", .{ arg_value.toString(allocator, true) });
            evaluated_args.append(arg_value) catch unreachable;
        }
        return evaluated_args;
    } else {
        return null;
    }
}

pub fn executeRepl(
    self: *Self, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt)
) void {
    self.current_depth += 1;
    defer self.current_depth -= 1;

    var eval: EvaluateResult = undefined;
    control: for (statements.items, 0..) |stmt, idx| {
        eval = self.evaluateStatement(allocator, stmt);

        if (idx == 0 and !eval.isErrorReturn()) {
            const result = eval.getExprOrFuncReturnValue();
            debug.print("{s}\n", .{ result.toString(allocator, false) });
        }

        if (self.debug_env) {
            self.env.*.display();
        }

        // NOTE(yemon): not doing anything right now on eval result variants
        // if the statements are being evaluated through REPL; will probably
        // change later on...
        continue :control;
    }
}

pub fn executeBlock(
    self: *Self, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt)
) EvaluateResult {
    self.current_depth += 1;
    const parent_env = self.env;
    const block_env = Environment.init(allocator, self.env);
    self.env = block_env;

    defer {
        self.env = parent_env;
        allocator.destroy(block_env);
        self.current_depth -= 1;
    }

    var block_eval: EvaluateResult = undefined;
    control: for (statements.items) |stmt| {
        block_eval = self.evaluateStatement(allocator, stmt);

        if (self.debug_env) {
            self.env.*.display();
        }

        switch (block_eval) {
            .func_return => |func_return| {
                self.debugPrint("  >> Statement received a 'func_return', " ++ 
                    "likely produced by a `return_stmt`. " ++
                    "(Return value: {s}, caller_depth: {})\n", .{
                        func_return.value.toString(allocator, true), 
                        func_return.caller_depth
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

    return block_eval;
}

pub fn executeBlockEnv(
    self: *Self, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt), 
    with_env: *Environment
) EvaluateResult {
    self.current_depth += 1;
    const parent_env = self.env;
    self.env = with_env;

    defer {
        self.env = parent_env;
        self.current_depth -= 1;
    }

    var block_eval: EvaluateResult = undefined;
    control: for (statements.items) |stmt| {
        block_eval = self.evaluateStatement(allocator, stmt);

        if (self.debug_env) {
            self.env.*.display();
        }

        switch (block_eval) {
            .func_return => |func_return| {
                self.debugPrint("  >> Statement received a `func_return`, " ++ 
                    "likely produced by a `return_stmt`. " ++
                    "(Return value: {s}, caller_depth: {})\n", .{
                        func_return.value.toString(allocator, true), 
                        func_return.caller_depth
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
            }
        }
    }
    
    return block_eval;
}

// It's kinda rare in for language to have both the variables and function declarations
// to NOT live in the same namespace. (e.g., Common Lisp). 
// If the functions need to be "first-class citizens", they both have to exist 
// in the same namespace.
pub const Environment = struct {
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
            return RuntimeError.AlreadyDefinedVariable;
        }
        try self.values.put(name, value);
    }

    fn defineFunction(self: *Environment, name: []const u8, function: LoxFunction) !void {
        try self.values.put(name, Value{
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
        self.values.put(name, value) catch return RuntimeError.DefinitionFailed;
    }

    fn getValue(self: *const Environment, name: []const u8) !Value {
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
};

fn debugPrint(self: *const Self, comptime fmt_msg: []const u8, args: anytype) void {
    if (!self.debug_print) {
        return;
    }
    debug.print("[DEPTH {}]   ", .{ self.current_depth });
    debug.print(fmt_msg, args);
}
