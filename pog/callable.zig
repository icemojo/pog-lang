const std = @import("std");
const fmt = @import("std").fmt;
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const ast = @import("ast.zig");
const Value = @import("value.zig").Value;
const Interpreter = @import("interpreter.zig");
const Environment = @import("env.zig");
const EvaluateResult = @import("interpreter.zig").EvaluateResult;

// 1) free functions can be called
// 2) class 'member functions' can be called in scope of its instance
// 3) 'class definitions' can be called to construct a new instance

pub const PogFunction = struct {
    declaration: ast.FunctionDeclareStmt,

    pub fn init(func_declare_stmt: ast.FunctionDeclareStmt) PogFunction {
        return .{
            .declaration = func_declare_stmt,
        };
    }

    pub fn arity(self: PogFunction) usize {
        return if (self.declaration.params) |params| params.items.len else 0;
    }

    pub fn call(
        self: *const PogFunction, allocator: Allocator,
        interpreter: *Interpreter, 
        func_args: ?std.ArrayList(Value)
    ) EvaluateResult {
        const func_env = Environment.init(allocator, interpreter.env);
        defer {
            func_env.deinit();
            allocator.destroy(func_env);
        }

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

    pub fn display(self: PogFunction) void {
        const name = if (self.declaration.name.lexeme) |lexeme| lexeme else "NA";
        debug.print("<fun {s} *{}>", .{ name, self.arity() });
    }

    pub fn toString(self: PogFunction, allocator: Allocator) []const u8 {
        const name = if (self.declaration.name.lexeme) |lexeme| lexeme else "NA";
        return std.fmt.allocPrint(allocator, "<fun {s} *{}>", .{ name, self.arity() })
            catch "<fun ->";
    }
};

pub const PogStruct = struct {
    name: []const u8,
    declaration: ast.StructDeclareStmt,

    pub fn call(self: *const PogStruct, allocator: Allocator) PogInstance {
        _ = self;
        _ = allocator;
        const instance = PogInstance.init();
        return instance;
    }

    pub fn display(self: *const PogStruct) void {
        _ = self;
        debug.print("<struct >", .{});
    }
};

pub const PogInstance = struct {
    //pog_struct: *const PogStruct,
    //name: Token,
    name: []u8,
    fields: std.StringHashMap(Value),

    pub fn init(allocator: Allocator) PogInstance {
        const instance: PogInstance = .{
            .fields = .init(allocator),
        };
        return instance;
    }

    pub fn deinit(self: *const PogInstance) void {
        self.fields.deinit();
    }

    pub fn toString(self: PogInstance) void {
        debug.print("{s} instance", .{ self.pog_struct.name });
    }
};

