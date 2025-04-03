const std = @import("std");
const fmt = @import("std").fmt;
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const ast = @import("ast.zig");
const Interpreter = @import("interpreter.zig");
const Environment = @import("interpreter.zig").Environment;
const Value = @import("interpreter.zig").Value;
// const ControlFlowError = @import("interpreter.zig").ControlFlowError;
const ControlFlow = @import("interpreter.zig").ControlFlow;

pub const LoxFunction = struct {
    const Self = @This();
    
    declaration: *const ast.FunctionDeclareStmt,

    pub fn init(func_declare_stmt: *const ast.FunctionDeclareStmt) Self {
        return .{
            .declaration = func_declare_stmt,
        };
    }

    pub fn arity(self: *const Self) usize {
        return if (self.declaration.params) |params| params.items.len else 0;
    }

    pub fn call(
        self: *const LoxFunction, allocator: Allocator,
        interpreter: *Interpreter, 
        func_args: ?std.ArrayList(Value)
    ) ?ControlFlow { //} Value {
        const func_env = Environment.init(allocator, interpreter.global_env);
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

        debug.print("LoxFunction.call():\n", .{});
        const control_flow: ?ControlFlow = interpreter.executeBlockEnv(
            allocator, 
            self.declaration.*.body, 
            func_env
        ) catch null;
        debug.print("LoxFunction.call() done ", .{});
        if (control_flow) |flow| {
            debug.print("WITH control flow result: {s}", .{ flow.return_value.toString(allocator) });
        } else {
            debug.print("WITHOUT any control flow result.\n", .{});
        }

        return control_flow;

        // if (control_flow) |flow| {
        //     return flow.return_value;
        // } else {
        //     return Value{ .nil = true };
        // }

        //     ControlFlowError.FunctionReturned => {
        //         // TODO(yemon): how am I actually gonna grab the returned 'value' 
        //         // through error??
        //     },
        //     else => {
        //         return err;
        //     },
        // };
    }

    pub fn toString(self: LoxFunction, allocator: Allocator) []const u8 {
        const name = if (self.declaration.name.lexeme) |lexeme| lexeme else "NA";
        const str = fmt.allocPrint(allocator, "<fun {s}>", .{ name }) catch "-";
        return str;
    }
};
