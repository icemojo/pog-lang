const std = @import("std");
const debug = @import("std").debug;
const log = @import("std").log;
const Allocator = @import("std").mem.Allocator;

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

    pub fn doComparison(self: Value, rhs: Value, op: ComparisonOp) EvaluationError!Value {
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
                        return error.InvalidComparisonOperand;
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
                        return error.InvalidComparisonOperand;
                    }
                }
            },

            else => {
                return error.InvalidComparisonOperand;
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
    InvalidAssignmentOperation,
    InvalidBinaryOperands,
    InvalidArithmeticOperand,
    InvalidComparisonOperand,
    InvalidStringOperand,
    InvalidValueTypeToNegate,
    StringConcatFailed,
    InvalidVariableAccess,
    InvalidFunctionDeclaration,
    InvalidFunctionCall,
    InvalidFunctionReturn,
    NotDoneYet,

    OutOfMemory,
};

// NOTE(yemon): Runtime errors should probably return, attached with a proper message
// since they should probably be presented and visible to the user.
const RuntimeError = error {
    InvalidUnaryOperand,
    InvalidBinaryOperands,
    UninitializedVariable,
    UndefinedIdentifier,
    UndefinedFunction,
    AlreadyDefinedVariable,
    AlreadyDefinedFunction,
    FunctionArityMismatch,
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

pub fn _executeAll(
    self: *Self, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt)
) (EvaluationError || RuntimeError)!?ControlFlow {
    var control_flow: ?ControlFlow = null;

    // for (statements.items) |stmt| {
    var i: usize = 0;
    while (i < statements.items.len) : (i += 1)  {
        const stmt = statements.items[i];

        control_flow = null;
        // const control_flow: ?ControlFlow = try self.evaluateStatement(allocator, stmt);
        control_flow = try self.evaluateStatement(allocator, stmt);
        if (self.debug_env) {
            self.env.*.display(allocator);
        }

        if (control_flow != null) {
            // self.debugPrint("<<< Statement done with function RETURN {s}\n", .{ 
                // flow.return_value.toString(allocator),
            // });
            break; // :block_scope;
        } else {
            // self.debugPrint("<<< Control should NOT break.\n", .{});
            // self.debugPrint("<<< Statement has NO RETURN.\n", .{});
            continue; //:block_scope;
        }
    }
    if (control_flow) |flow| {
        self.debugPrint("<<< executeAll()  : done WITH return value {s}\n", .{ 
            flow.return_value.toString(allocator) 
        });
    } else {
        self.debugPrint("<<< executeAll()  : done WITHOUT control flow.\n", .{});
    }

    return control_flow;
}

pub const ControlFlow = struct {
    return_value: Value,
};

// const ReturnType = enum {
//     Block,
//     Function,
// };

//const ReturnValue = Value; // struct {
//     type: ReturnType,
//     value: Value,
// };

fn evaluateStatement(
    self: *Self, allocator: Allocator, 
    stmt: *const ast.Stmt
) (EvaluationError || RuntimeError)!EvaluateResult {
    // self.debugPrint("Current env inner return (bef stmt eval): ", .{});
    // if (self.env.*.inner_return) |ret_value| {
        // debug.print("{s}\n", .{ ret_value.toString(allocator) });
        // return ControlFlow{
        //     .return_value = ret_value,
        // };
    // } else {
        // debug.print("NONE\n", .{});
    // }

    switch (stmt.*) {
        .expr => |expr| {
            self.debugPrint("Evaluating expression...\n", .{});
            _ = try self.evaluate(allocator, expr.expr);
            return .{ .no_return = true };
        },

        .block => |block| {
            self.debugPrint("Evaluating a block...\n", .{});
            // const return_value = try self.executeBlock(allocator, block.statements);
            // return return_value;
            // const control_flow = try self.executeBlock(allocator, block.statements);
            // return control_flow;
            const block_eval = try self.executeBlock(allocator, block.statements);
            if (block_eval.getIfFuncReturn()) |func_return| {
                _ = func_return;
                // TODO: break out of the call stack
                self.debugPrint("  Block got function return. Should break out...\n", .{});
                return block_eval;
            } else {
                return .{ .no_return = true };
            }

            // if (control_flow) |flow| {
            //     self.debugPrint("Block evaluation done WITH control flow {s}\n", .{ flow.return_value.toString(allocator) });
            // } else {
            //     self.debugPrint("Block evaluation done WITHOUT any control flow\n", .{});
            // }

            // return control_flow;
        },

        .print => |print| {
            self.debugPrint("Evaluating print statement...\n", .{});
            const eval = try self.evaluate(allocator, print.expr);
            const value = eval.expr_value;
            debug.print("{s}\n", .{ value.toString(allocator) });
            return .{ .no_return = true };
        },

        .if_stmt => |if_stmt| {
            self.debugPrint("Evaluating if statement...\n", .{});
            const eval = try self.evaluate(allocator, if_stmt.condition);
            // TODO(yemon): should I asset the non `expr_value` types here?
            const condition = eval.expr_value;
            if (condition.isTruthy()) {
                self.debugPrint("  TRUE... \n", .{});
                _ = try self.evaluateStatement(allocator, if_stmt.then_branch);
            } else {
                self.debugPrint("  FALSE... \n", .{});
                if (if_stmt.else_branch) |else_branch| {
                    self.debugPrint("  else branch...\n", .{});
                    _ = try self.evaluateStatement(allocator, else_branch);
                }
            }
            return .{ .no_return = true };
        },

        .while_stmt => |while_stmt| {
            self.debugPrint("Evaluating while statement block...\n", .{});
            var eval = try self.evaluate(allocator, while_stmt.condition);
            // TODO(yemon): should I asset the non `expr_value` types here?
            var condition = eval.expr_value;
            while (condition.isTruthy()) {
                _ = try self.evaluateStatement(allocator, while_stmt.body);
                eval = try self.evaluate(allocator, while_stmt.condition);
                condition = eval.expr_value;
            }
            return .{ .no_return = true };
        },

        .variable_declare_stmt => |variable_declare| {
            self.debugPrint("Evaluating variable declaration statement...\n", .{});
            if (variable_declare.initializer) |initializer| {
                const eval = try self.evaluate(allocator, initializer);
                const value = eval.expr_value;
                try self.env.*.define(variable_declare.name, value);
            } else {
                try self.env.*.define(variable_declare.name, Value{ .nil = true });
            }
            return .{ .no_return = true };
        },

        .func_declare_stmt => |func_declare| {
            var name: []const u8 = undefined;
            if (func_declare.name.lexeme) |lexeme| {
                name = lexeme;
            } else {
                return EvaluationError.InvalidFunctionDeclaration;
            }
            self.debugPrint("Evaluating the function declaration statement '{s}'...\n", .{ name });

            const function = LoxFunction.init(&func_declare);
            try self.env.*.defineFunction(name, function);
            return .{ .no_return = true };
        },

        .return_stmt => |return_stmt| {
            self.debugPrint("Evaluating a return statement... caller_depth: {}\n", .{
                self.caller_depth
            });
            if (return_stmt.expr) |expr| {
                const return_eval = try self.evaluate(allocator, expr);
                const value = return_eval.expr_value;
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

pub const EvaluateResult = union(enum) {
    expr_value: Value,
    func_return: FuncReturn,
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

    fn getIfFuncReturn(self: EvaluateResult) ?FuncReturn {
        switch (self) {
            .func_return => |it| {
                return it;
            },
            else => {
                return null;
            }
        }
    }
};

const FuncReturn = struct {
    value: Value,
    caller_depth: i32,
};

fn evaluate(
    self: *Self, allocator: Allocator, 
    expr: *const ast.Expr
) (EvaluationError || RuntimeError)!EvaluateResult {
    switch (expr.*) {
        .assign => |assignment| {
            self.debugPrint("  Evaluating assignment expression...\n", .{});
            try self.evaluateAssignmentExpr(allocator, &assignment);
            return .{ .no_return = true };
        },

        .binary => |binary| {
            self.debugPrint("  Evaluating binary expression...\n", .{});
            const value = try self.evaluateBinaryExpr(allocator, &binary);
            return .{
                .expr_value = value,
            };
        },

        .logical => |logical| {
            self.debugPrint("  Evaluating logical expression...\n", .{});
            const value = try self.evaluateLogicalExpr(allocator, &logical);
            return .{
                .expr_value = value,
            };
        },

        .unary => |unary| {
            self.debugPrint("  Evaluating unary expression...\n", .{});
            const value = try self.evaluateUnaryExpr(allocator, &unary);
            return .{
                .expr_value = value,
            };
        },

        .grouping => |group| {
            self.debugPrint("  Evaluating grouping expression...\n", .{});
            return try self.evaluate(allocator, group.inner);
        },

        .literal => |literal| {
            self.debugPrint("  Evaluating literal expression...\n", .{});
            const value = literal.evaluate(allocator);
            return .{
                .expr_value = value,
            };
        },

        // NOTE(yemon): Not sure how to do with error handling here,
        // this switch branch know everything that could go wrong with the variable, 
        // but since the function itself is error-returned, the branch cannot choose
        // to return nothing if the error is already handled locally.
        // Other info related to the error (like 'lexeme') cannot be passed back
        // to the caller.
        .variable => |variable| {
            self.debugPrint("  Evaluating variable expression...\n", .{});
            const name = variable.lexeme.?;

            const env_value = self.env.*.getValue(name) catch |err| switch (err) {
                RuntimeError.UndefinedIdentifier => {
                    log.err("Undefined variable '{s}'.", .{ name });
                    return .{
                        .expr_value = Value{ .nil = true },
                    };
                },
                else => {
                    log.err("Unknown error when trying to access variable '{s}': {}", .{ 
                        name, err 
                    });
                    return err;
                },
            };

            switch (env_value) {
                .value => |value| {
                    self.debugPrint("  Env Value: {s}\n", .{ value.toString(allocator) });
                    return .{
                        .expr_value = value,
                    };
                },
                else => {
                    return EvaluationError.InvalidVariableAccess;
                }
            }
        },

        .func_call => |func_call| {
            self.debugPrint("  Evaluating a function call expression :: ", .{});
            if (self.debug_print) {
                func_call.display(false);
                debug.print("\n", .{});
                // self.debugPrint("\n", .{});
            }

            self.debugPrint("  Function callee expr: ", .{});
            if (self.debug_print) {
                func_call.callee.*.display(true);
            }

            self.caller_depth = self.current_depth;
            const func_eval_result: EvaluateResult = try self.evaluateFunctionCallExpr(
                allocator, &func_call
            );
            self.debugPrint("Finished function call expression :: \n", .{});
            if (self.debug_print) {
                func_call.display(false);
                debug.print("\n", .{});
            }

            const func_return = func_eval_result.func_return;
            // if (func_return) |func_return| {
            if (self.caller_depth == func_return.caller_depth) {
                // break out of all evaluation recursive calls
            } else {
                //
            }
            // } else {
            //     return EvaluationError.InvalidFunctionReturn;
            // }

            return func_eval_result;

            // const control_flow = try self.evaluateFunctionCallExpr(allocator, &func_call);
            // self.debugPrint("Function call expression done :: ", .{});
            // if (control_flow) |flow| {
            //     if (self.debug_print) {
            //         func_call.display(false);
            //         self.debugPrint("WITH control flow {s}\n", .{ 
            //             flow.return_value.toString(allocator) 
            //         });
            //     }
            //     const return_value: Value = flow.return_value;
            //     return return_value;
            // } else {
            //     self.debugPrint("WITHOUT control flow.\n", .{});
            //     return Value{ .nil = true };
            // }
        },
    }
}

fn evaluateAssignmentExpr(
    self: *Self, allocator: Allocator, 
    assignment: *const ast.AssignmentExpr
) (EvaluationError || RuntimeError)!void {
    const eval = try self.evaluate(allocator, assignment.value);
    const value = eval.getExprValue();

    try self.env.*.assign(assignment.name, value);
}

fn evaluateBinaryExpr(
    self: *Self, allocator: Allocator, 
    binary: *const ast.BinaryExpr
) (EvaluationError || RuntimeError)!Value {
    const left_eval = try self.evaluate(allocator, binary.left);
    const left_value = left_eval.getExprValue();

    const right_eval = try self.evaluate(allocator, binary.right);
    const right_value = right_eval.getExprValue();

    switch (binary.optr.token_type) { 
        .Minus => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return EvaluationError.InvalidBinaryOperands;
            }
            return try left_value.doArithmetic(right_value, .substract);
        },

        .Slash => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return EvaluationError.InvalidBinaryOperands;
            }
            return try left_value.doArithmetic(right_value, .divide);
        },

        .Star => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return EvaluationError.InvalidBinaryOperands;
            }
            return try left_value.doArithmetic(right_value, .multiply);
        },

        .Plus => {
            switch (left_value) {
                .integer, .double => {
                    if (!left_value.isNumber() or !right_value.isNumber()) {
                        return EvaluationError.InvalidBinaryOperands;
                    }
                    return try left_value.doArithmetic(right_value, .addition);
                },
                .string => |left_string| {
                    switch (right_value) {
                        .string => |right_string| {
                            return concatStrings(allocator, left_string, right_string) catch {
                                return EvaluationError.StringConcatFailed;
                            };
                        },
                        else => {
                            return EvaluationError.InvalidStringOperand;
                        }
                    }
                },
                else => {
                    return EvaluationError.InvalidArithmeticOperand;
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
                return EvaluationError.InvalidBinaryOperands;
            }
            const is_less = left_value.doComparison(right_value, .lesser) 
                catch Value{ .boolean = false };
            return is_less;
        },
        .LessEqual => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return EvaluationError.InvalidBinaryOperands;
            }
            const is_less_equal = left_value.doComparison(right_value, .lesser_equal)
                catch Value{ .boolean = false };
            return is_less_equal;
        },

        .Greater => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return EvaluationError.InvalidBinaryOperands;
            }
            const is_greater = left_value.doComparison(right_value, .greater) 
                catch Value{ .boolean = false };
            return is_greater;
        },
        .GreaterEqual => {
            if (!left_value.isNumber() or !right_value.isNumber()) {
                return EvaluationError.InvalidBinaryOperands;
            }
            const is_greater_equal = left_value.doComparison(right_value, .greater_equal)
                catch Value{ .boolean = false };
            return is_greater_equal;
        },

        else => {
            return EvaluationError.UnknownBinaryOperation;
        }
    }
}

fn evaluateLogicalExpr(
    self: *Self, allocator: Allocator, 
    logical: *const ast.LogicalExpr
) (EvaluationError || RuntimeError)!Value {
    const left_eval = try self.evaluate(allocator, logical.left);
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

    const right_eval = try self.evaluate(allocator, logical.right);
    return right_eval.getExprValue();
}

fn evaluateUnaryExpr(
    self: *Self, allocator: Allocator, 
    unary: *const ast.UnaryExpr
) (EvaluationError || RuntimeError)!Value {
    const eval = try evaluate(self, allocator, unary.right);
    const value = eval.getExprValue();

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

// NOTE(yemon): 
//   1) free functions can be called
//   2) class 'member functions' can be called in scope of its instance
//   3) 'class definitions' can be called to construct a new instance
fn evaluateFunctionCallExpr(
    self: *Self, allocator: Allocator, 
    func_call: *const ast.FunctionCallExpr,
) (EvaluationError || RuntimeError)!EvaluateResult {
    const callee: ast.Expr = func_call.callee.*;
    switch (callee) {
        .variable => |variable| {
            const name = variable.lexeme orelse unreachable;
            const env_value = self.env.*.getValue(name) catch |err| switch (err) {
                RuntimeError.UndefinedIdentifier => {
                    log.err("Undefined function '{s}'", .{ name });
                    return RuntimeError.UndefinedFunction;
                },
                else => {
                    return err;
                }
            };

            switch (env_value) {
                .function => |lox_function| {
                    const evaluated_args = try self.evaluateFunctionArguments(
                        allocator, func_call
                    );
                    const args_count = if (evaluated_args) |args| 
                        @as(usize, args.items.len) else 0;
                    self.debugPrint("  Function arity: {}, args_count: {}\n", .{ 
                        lox_function.arity(), args_count 
                    });
                    if (lox_function.arity() != args_count) {
                        return RuntimeError.FunctionArityMismatch;
                    }

                    // self.debugPrint("  Triggering LoxFunction call() with {} arguments.\n", .{
                    //     if (evaluated_args) |args| args.items.len else 0
                    // });
                    // const control_flow = lox_function.call(allocator, self, evaluated_args);
                    // self.debugPrint("  LoxFunction call() result: \n", .{});
                    // if (control_flow) |flow| {
                        // self.debugPrint("{s}", .{ flow.return_value.toString(allocator) });
                    // }
                    // return control_flow;

                    const func_eval_result: EvaluateResult = lox_function.call(
                        allocator, self, evaluated_args
                    );
                    return func_eval_result;
                },
                else => {
                    return EvaluationError.InvalidFunctionCall;
                }
            }
        },

        else => {
            return EvaluationError.InvalidFunctionCall;
        }
    }
}

fn evaluateFunctionArguments(
    self: *Self, allocator: Allocator, 
    func_call: *const ast.FunctionCallExpr
) (EvaluationError || RuntimeError)!?std.ArrayList(Value) {
    self.debugPrint("  Evaluating function arguments...\n", .{});
    var evaluated_args = std.ArrayList(Value).init(allocator);
    if (func_call.arguments) |args| {
        for (args.items) |arg| {
            const eval = try self.evaluate(allocator, arg);
            const arg_value = eval.expr_value;
            self.debugPrint("  -> {s}\n", .{ arg_value.toString(allocator) });
            evaluated_args.append(arg_value) catch unreachable;
        }
    } else {
        return null;
    }

    return evaluated_args;
}

pub fn executeBlock(
    self: *Self, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt)
) !EvaluateResult {
    self.current_depth += 1;
    // if (self.env.*.inner_return) |inner_return| {
        // self.debugPrint(">>> Inner block returned a value: {s}. Need to early break!\n", .{ 
            // inner_return.toString(allocator) 
        // });
        // return ControlFlow{
        //     .return_value = inner_return,
        // };
    // } else {
        // self.debugPrint(">>> Inner block returned NOTHING!\n", .{});
    // }

    const parent_env = self.env;
    const block_env = Environment.init(allocator, self.env);
    defer allocator.destroy(block_env);

    self.debugPrint("  >> executeBlock():\n", .{});
    self.env = block_env;
    // const control_flow = try self.executeAll(allocator, statements);
    // const control_flow: ?ControlFlow = control: for (statements.items) |stmt| {
    var eval_result: EvaluateResult = undefined;
    control: for (statements.items) |stmt| {
        eval_result = try self.evaluateStatement(allocator, stmt);

        switch (eval_result) {
            .func_return => |func_return| {
                self.debugPrint("  >> Need to return the function call. " ++ 
                    "(Return value: {s}, caller_depth: {})\n", .{
                        func_return.value.toString(allocator), func_return.caller_depth
                    });
                break :control;
            },
            else => {
                continue :control;
            }
        }
        // break :control .{ .no_return = true };
        // if (flow) |it| {
        //     self.debugPrint(">>> Need to break out WITH {s}.\n", .{ 
        //         it.return_value.toString(allocator) 
        //     });
        //     break :control flow;
        // } else {
        //     continue :control;
        // }
    } // else .{ .no_return = true }; // else null;
    self.env = parent_env;

    // if (control_flow) |flow| {
    //     self.debugPrint(">>> executeBlock(): done WITH {s}\n", .{ 
    //         flow.return_value.toString(allocator) 
    //     });
    //     self.env.*.inner_return = flow.return_value;
    // } else {
    //     self.debugPrint(">>> executeBlock(): done WITHOUT control flow result.\n", .{});
    // }
    self.debugPrint("  >> caller_depth: {}, current_depth: {}\n", .{ 
        self.caller_depth, self.current_depth
    });
    self.debugPrint("  >> executeBlock() done!\n", .{});

    self.current_depth -= 1;
    // return control_flow;
    return eval_result;
}

pub fn executeBlockEnv(
    self: *Self, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt), 
    with_env: *Environment
) !EvaluateResult {
    self.current_depth += 1;
    // if (self.env.*.inner_return) |inner_return| {
    //     self.debugPrint(">>> Inner block returned a value: {s}. Need to early break!\n", .{ 
    //         inner_return.toString(allocator) 
    //     });
        // return ControlFlow{
        //     .return_value = inner_return,
        // };
    // } else {
    //     self.debugPrint(">>> Inner block returned NOTHING!\n", .{});
    // }

    const parent_env = self.env;

    self.debugPrint("  >> executeBlockEnv():\n", .{});
    self.env = with_env;
    // const control_flow = try self.executeAll(allocator, statements);
    // const control_flow: ?ControlFlow = control: for (statements.items) |stmt| {
    const eval_result: EvaluateResult = control: for (statements.items) |stmt| {
        const stmt_eval: EvaluateResult = try self.evaluateStatement(allocator, stmt);

        switch (stmt_eval) {
            .func_return => |func_return| {
                self.debugPrint("  >> Need to return the function call. " ++ 
                    "(Return value: {s}, caller_depth: {})\n", .{
                        func_return.value.toString(allocator), func_return.caller_depth
                    });
                break :control stmt_eval;
            },
            else => {
                continue :control;
            }
        }
        // break :control .{ .no_return = true };
        // if (flow) |it| {
        //     self.debugPrint(">>> Need to break out WITH {s}.\n", .{ 
        //         it.return_value.toString(allocator) 
        //     });
        //     break :control flow;
        // } else {
        //     continue :control;
        // }
    } else .{ .no_return = true }; // else null;
    self.env = parent_env;
    
    // if (control_flow) |flow| {
    //     self.debugPrint(">>> executeBlockEnv(): done WITH {s}\n", .{ 
    //         flow.return_value.toString(allocator) 
    //     });
    //     self.env.*.inner_return = flow.return_value;
    // } else {
    //     self.debugPrint(">>> executeBlockEnv(): done WITHOUT control flow result.\n", .{});
    // }
    self.debugPrint("  >> caller_depth: {}, current_depth: {}\n", .{ 
        self.caller_depth, self.current_depth
    });

    self.current_depth -= 1;
    // return control_flow;
    return eval_result;
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
    inner_return: ?Value,
    values: std.StringHashMap(EnvValue),

    pub fn init(allocator: Allocator, enclosing: ?*Environment) *Environment {
        const env = allocator.create(Environment) catch unreachable;
        env.*.enclosing = enclosing;
        env.*.inner_return = null;
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

    fn getValue(self: *const Environment, name: []const u8) RuntimeError!EnvValue {
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

fn debugPrint(self: *const Self, comptime fmt: []const u8, args: anytype) void {
    if (!self.debug_print) {
        return;
    }
    debug.print("[DEPTH {}]   ", .{ self.current_depth });
    debug.print(fmt, args);
}
