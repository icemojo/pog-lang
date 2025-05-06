const std = @import("std");
const fmt = @import("std").fmt;
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const ast = @import("ast.zig");
const Interpreter = @import("interpreter.zig");
const Environment = @import("interpreter.zig").Environment;
const Value = @import("interpreter.zig").Value;
const EvaluateResult = @import("interpreter.zig").EvaluateResult;

// 1) free functions can be called
// 2) class 'member functions' can be called in scope of its instance
// 3) 'class definitions' can be called to construct a new instance

pub const LoxFunction = struct {
    declaration: ast.FunctionDeclareStmt,

    pub fn init(func_declare_stmt: ast.FunctionDeclareStmt) LoxFunction {
        return .{
            .declaration = func_declare_stmt,
        };
    }

    pub fn arity(self: LoxFunction) usize {
        return if (self.declaration.params) |params| params.items.len else 0;
    }

    pub fn call(
        self: *const LoxFunction, allocator: Allocator,
        interpreter: *Interpreter, 
        func_args: ?std.ArrayList(Value)
    ) EvaluateResult {
        const func_env = Environment.init(allocator, interpreter.env);
        defer allocator.destroy(func_env);

        if (self.declaration.params != null and func_args != null) {
            const params = self.declaration.params.?;
            const args = func_args.?;

            for (params.items, 0..) |param, i| {
                const arg = args.items[i];
                if (param.lexeme) |name| {
                    func_env.*.define(name, arg) catch continue;
                }
            }
        }

        const func_eval_result: EvaluateResult = interpreter.executeBlockEnv(
            allocator, self.declaration.body, func_env
        );
        return func_eval_result;
    }

    pub fn display(self: LoxFunction) void {
        const name = if (self.declaration.name.lexeme) |lexeme| lexeme else "NA";
        debug.print("<fun {s} *{}>", .{ name, self.arity() });
    }
};
