const std       = @import("std");
const fmt       = @import("std").fmt;
const debug     = @import("std").debug;
const assert    = @import("std").debug.assert;
const eql       = @import("std").mem.eql;
const Allocator = @import("std").mem.Allocator;

const Token = @import("lexer.zig").Token;
const Value = @import("value.zig").Value;
const ArithmeticOp = @import("value.zig").ArithmeticOp;

pub const Expr = union(enum) {
    assign: AssignmentExpr,
    binary: BinaryExpr,
    logical: LogicalExpr,
    unary: UnaryExpr,
    grouping: GroupingExpr,
    literal: LiteralExpr,
    variable: VariableExpr,
    func_call: FunctionCallExpr,
    getter: GetterExpr,
    setter: SetterExpr,

    pub fn getTypeName(self: Expr) []const u8 {
        return switch (self) {
            .assign => "assignment",
            .binary => "binary",
            .logical => "logical",
            .unary => "unary",
            .grouping => "grouping",
            .literal => "literal",
            .variable => "variable",
            .func_call => "function call",
            .getter => "field getter",
            .setter => "field setter",
        };
    }

    pub fn display(self: Expr, line_break: bool) void { 
        switch (self) {
            .assign => |assignment| {
                assignment.display(line_break);
            },
            .binary => |binary| {
                binary.display();
            },
            .logical => |logical| {
                logical.display();
            },
            .unary => |unary| {
                unary.display(line_break);
            },
            .grouping => |grouping| {
                grouping.display(line_break);
            },
            .literal => |literal| {
                literal.display();
            },
            .variable => |variable| {
                if (variable.lexeme) |lexeme| {
                    debug.print("{s}", .{ lexeme });
                } else {
                    debug.print("-", .{});
                }
            },
            .func_call => |func_call| {
                func_call.display(line_break);
            },
            .getter => |getter| {
                if (getter.field.lexeme) |lexeme| {
                    debug.print("{s}", .{ lexeme });
                } else {
                    debug.print("-", .{});
                }
            },
            .setter => |setter| {
                if (setter.name.lexeme) |lexeme| {
                    debug.print("{s}", .{ lexeme });
                } else {
                    debug.print("-", .{});
                }
            },
        }
        if (line_break) {
            debug.print("\n", .{});
        }
    }
};

// NOTE(yemon): In the grand scheme of things, this error variation *does not* matter at all.
// What the caller (the `parser`) actually cares about is that expression allocation has failed.
// That's it.
pub const AllocError = error {
    StringAllocFailed,
    OutOfMemory
};

pub const AssignmentExpr = struct {
    identifier: []u8,
    value: *Expr,

    fn display(self: *const AssignmentExpr, line_break: bool) void {
        debug.print("{s} = ", .{ self.identifier });
        self.value.*.display(line_break);
    }
};

pub fn createAssignmentExpr(allocator: Allocator, variable: Token, value: *Expr) AllocError!*Expr {
    if (variable.lexeme) |lexeme| {
        const name = allocator.alloc(u8, lexeme.len) catch
            return AllocError.StringAllocFailed;
        @memcpy(name, lexeme);
        errdefer allocator.free(name);

        const expr = allocator.create(Expr) catch
            return AllocError.OutOfMemory;
        expr.* = Expr{
            .assign = AssignmentExpr{
                .identifier = name,
                .value = value,
            },
        };
        return expr;
    } else {
        return AllocError.StringAllocFailed;
    }
}

pub const BinaryExpr = struct {
    left:  *Expr,
    optr:  Token,
    right: *Expr,

    fn display(self: *const BinaryExpr) void {
        debug.print("({s} ", .{ self.optr.toString() });
        self.left.*.display(false);
        debug.print(" ", .{});
        self.right.*.display(false);
        debug.print(")", .{});
    }
};

pub fn createBinaryExpr(allocator: Allocator, left: *Expr, optr: Token, right: *Expr) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .binary = BinaryExpr{
            .left = left,
            .optr = optr,
            .right = right,
        },
    };
    return expr;
}

pub const LogicalExpr = struct {
    left: *Expr,
    optr: Token,
    right: *Expr,

    fn display(self: *const LogicalExpr) void {
        debug.print("({s} ", .{ self.optr.toString() });
        self.left.*.display(false);
        debug.print(" ", .{});
        self.right.*.display(false);
        debug.print(")", .{});
    }
};

pub fn createLogicalExpr(allocator: Allocator, left: *Expr, optr: Token, right: *Expr) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .logical = LogicalExpr{
            .left = left,
            .optr = optr,
            .right = right,
        },
    };
    return expr;
}

pub const UnaryExpr = struct {
    optr:  Token,
    right: *Expr,

    fn display(self: *const UnaryExpr, line_break: bool) void {
        debug.print("(", .{});
        self.optr.display();
        debug.print(" ", .{});
        self.right.*.display(line_break);
        debug.print(")", .{});
    }
};

pub fn createUnaryExpr(allocator: Allocator, optr: Token, right: *Expr) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .unary = UnaryExpr{
            .optr = optr,
            .right = right,
        },
    };
    return expr;
}

pub const GroupingExpr = struct {
    inner: *Expr,

    fn display(self: *const GroupingExpr, line_break: bool) void {
        self.inner.display(line_break);
    }
};

pub fn createGroupingExpr(allocator: Allocator, inner: *Expr) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .grouping = GroupingExpr{
            .inner = inner,
        },
    };
    return expr;
}

pub const LiteralExpr = union(enum) {
    integer: i64,
    double:  f64,
    text:  []u8,
    boolean: bool,
    nil: bool,

    pub fn evaluate(self: LiteralExpr, allocator: Allocator) Value {
        switch (self) {
            .integer => |value| {
                return Value{ .integer = value };
            },

            .double => |value| {
                return Value{ .double = value };
            },

            .text => |value| {
                // TODO(yemon): this is leaking right now, but whatever...
                // will probably chug the entirety of AST into an arena eventually anyway
                const target_string: []u8 = allocator.alloc(u8, value.len) catch "";
                @memcpy(target_string, value);
                return Value{
                    .string = target_string,
                };
            },

            .boolean => |value| {
                return Value{ .boolean = value };
            },

            .nil => {
                return Value{ .nil = true };
            },
        }
    }

    fn display(self: LiteralExpr) void {
        switch (self) {
            .integer => |value| {
                debug.print("{any}", .{ value });
            },
            .double => |value| {
                debug.print("{d}", .{ value });
            },
            .text => |value| {
                debug.print("\"{s}\"", .{ value });
            },
            .boolean => |value| {
                debug.print("{any}", .{ value });
            },
            .nil => {
                debug.print("nil", .{});
            },
        }
    }
};

pub fn createLiteral(allocator: Allocator, value: LiteralExpr) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .literal = value,
    };
    return expr;
}

pub fn createStringLiteral(allocator: Allocator, string_value: []const u8) !*Expr {
    // TODO(yemon): Double leakage here then the other 'create' functions...
    const target_value = try allocator.alloc(u8, string_value.len);
    @memcpy(target_value, string_value);

    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .literal = LiteralExpr{
            .text = target_value,
        },
    };
    return expr;
}

const VariableExpr = Token;

pub fn createVariableExpr(allocator: Allocator, name: Token) !*Expr {
    // NOTE(yemon): Could the `name` Token's lexeme and literal could cause problems
    // in terms of their lifetimes here. If you think about it, the heap allocated `expr`
    // here can potentially outlive the `source` string, of which the tokens were created.
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .variable = name,
    };
    return expr;
}

pub const FunctionCallExpr = struct {
    callee: *Expr,
    closing_paren: Token,
    arguments: ?std.ArrayList(*Expr),

    pub fn display(self: *const FunctionCallExpr, line_break: bool) void {
        self.callee.*.display(line_break);
        debug.print("(", .{});

        if (self.arguments) |args| {
            if (args.items.len > 0) {
                for (args.items) |arg| {
                    arg.*.display(false);
                    debug.print(", ", .{});
                }
            }
        }

        debug.print(")", .{});
    }
};

pub const GetterExpr = struct {
    object: *Expr,
    field: Token,
};

pub const SetterExpr = struct {
    object: *Expr,
    name: Token,
    value: *Expr,
};

fn printIndents(indents: u32) void {
    if (indents == 0) {
        return;
    }
    var count: u32 = 0;
    while (count <= indents) : (count += 1) {
        debug.print("    ", .{});
    }
}

pub const Stmt = union(enum) {
    expr_stmt: ExprStmt,
    block_stmt: BlockStmt,
    print_stmt: PrintStmt,
    compound_stmt: CompoundStmt,
    if_stmt: IfStmt,
    loop_stmt: LoopStmt,
    variable_declare_stmt: VariableDeclareStmt,
    func_declare_stmt: FunctionDeclareStmt,
    return_stmt: ReturnStmt,

    pub fn display(self: Stmt, indents: u32) void {
        printIndents(indents);
        switch (self) {
            .expr_stmt => |expr| {
                expr.display();
            },
            .block_stmt => |block| {
                block.display(indents);
            },
            .print_stmt => |print| {
                print.display();
            },
            .compound_stmt => |compound| {
                compound.display();
            },
            .if_stmt => |if_stmt| {
                if_stmt.display(indents);
            },
            .loop_stmt => |loop_stmt| {
                loop_stmt.display(indents);
            },
            .variable_declare_stmt => |variable_declare| {
                variable_declare.display();
            },
            .func_declare_stmt => |func_declare| {
                func_declare.display(indents);
            },
            .return_stmt => |return_stmt| {
                return_stmt.display();
            },
        }
    }
};

const ExprStmt = struct {
    expr: *Expr,

    fn display(self: *const ExprStmt) void {
        self.expr.*.display(false);
        debug.print("\n", .{});
    }
};

pub fn createExprStmt(allocator: Allocator, expr: *Expr) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = Stmt{
        .expr_stmt = ExprStmt{
            .expr = expr,
        },
    };
    return stmt;
}

pub const BlockStmt = struct {
    statements: std.ArrayList(*Stmt),

    pub fn init(allocator: Allocator) BlockStmt {
        return .{
            .statements = std.ArrayList(*Stmt).init(allocator),
        };
    }

    pub fn display(self: *const BlockStmt, indents: u32) void {
        debug.print("{{\n", .{});
        for (self.statements.items) |stmt| {
            stmt.*.display(indents+1);
        }
        printIndents(indents);
        debug.print("}}\n", .{});
    }
};

const PrintStmt = struct {
    expr: *Expr,

    fn display(self: *const PrintStmt) void {
        debug.print("print ", .{});
        self.expr.*.display(false);
        debug.print(";\n", .{});
    }
};

pub fn createPrintStmt(allocator: Allocator, expr: *Expr) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = Stmt{
        .print_stmt = PrintStmt{
            .expr = expr,
        },
    };
    return stmt;
}

const CompoundStmt = struct {
    identifier: []u8,
    optr: ArithmeticOp,
    expr: *Expr,

    fn display(self: *const CompoundStmt) void {
        debug.print("{s} ", .{ self.identifier });
        self.optr.display();
        debug.print(" ", .{});
        self.expr.*.display(false);
        debug.print("\n", .{});
    }
};

pub fn createCompoundStmt(
    allocator: Allocator, identifier_name: []u8, optr: ArithmeticOp, expr: *Expr,
) AllocError!*Stmt {
    if (identifier_name.len > 0) {
        const name = allocator.alloc(u8, identifier_name.len) catch
            return AllocError.StringAllocFailed;
        @memcpy(name, identifier_name);
        errdefer allocator.free(name);

        const stmt = allocator.create(Stmt) catch
            return AllocError.OutOfMemory;
        stmt.* = Stmt{
            .compound_stmt = CompoundStmt{
                .identifier = name,
                .optr = optr,
                .expr = expr,
            },
        };
        return stmt;
    } else {
        return AllocError.StringAllocFailed;
    }
}

const IfStmt = struct {
    condition: *Expr,
    then_branch: *Stmt,
    else_branch: ?*Stmt,

    fn display(self: *const IfStmt, indents: u32) void {
        debug.print("if (", .{});
        self.condition.*.display(false);
        debug.print(")\n", .{});

        self.then_branch.*.display(indents+1);

        if (self.else_branch) |else_branch| {
            printIndents(indents);
            debug.print("else\n", .{});
            else_branch.display(indents+1);
        }
    }
};

pub fn createIfStmt(allocator: Allocator, condition: *Expr, then_branch: *Stmt, else_branch: ?*Stmt) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = Stmt{
        .if_stmt = IfStmt{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        },
    };
    return stmt;
}

const LoopStmt = struct {
    condition: *Expr,
    body: *Stmt,

    fn display(self: *const LoopStmt, indents: u32) void {
        debug.print("while (", .{});
        self.condition.*.display(false);
        debug.print(")\n", .{});
        self.body.*.display(indents+1);
    }
};

pub fn createLoopStmt(allocator: Allocator, condition: *Expr, body: *Stmt) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = Stmt{
        .loop_stmt = LoopStmt{
            .condition = condition,
            .body = body,
        },
    };
    return stmt;
}

const VariableDeclareStmt = struct {
    name: []u8,
    initializer: ?*Expr,

    fn display(self: *const VariableDeclareStmt) void {
        debug.print("var {s}", .{ self.name });
        if (self.initializer) |initializer| {
            debug.print(" = ", .{});
            initializer.*.display(false);
        }
        debug.print(";\n", .{});
    }
};

pub fn createVariableDeclareStmt(
    allocator: Allocator, identifier: Token, initializer: ?*Expr
) AllocError!*Stmt {
    if (identifier.lexeme) |lexeme| {
        const target_name = allocator.alloc(u8, lexeme.len) catch 
            return AllocError.StringAllocFailed;
        @memcpy(target_name, lexeme);
        errdefer allocator.free(target_name);

        const stmt = allocator.create(Stmt) catch
            return AllocError.OutOfMemory;
        stmt.* = Stmt{
            .variable_declare_stmt = VariableDeclareStmt{
                .name = target_name,
                .initializer = initializer,
            },
        };
        return stmt;
    } else {
        return AllocError.StringAllocFailed;
    }
}

pub const FunctionDeclareStmt = struct {
    name: Token,
    params: ?std.ArrayList(Token),
    body: std.ArrayList(*Stmt),

    pub fn display(self: *const FunctionDeclareStmt, indents: u32) void {
        assert(self.name.lexeme != null);
        const name = self.name.lexeme.?;

        debug.print("fun {s}(", .{ name });
        if (self.params) |params| {
            for (params.items) |param| {
                if (param.lexeme) |lexeme| {
                    debug.print("{s}, ", .{ lexeme });
                } else {
                    debug.print("-, ", .{});
                }
            }
        }
        debug.print(")\n{{\n", .{});

        for (self.body.items) |stmt| {
            stmt.*.display(indents+1);
        }
        debug.print("}}\n", .{});
    }
};

pub fn createFunctionDeclareStmt(
    allocator: Allocator, 
    identifier: Token, 
    params: ?std.ArrayList(Token), 
    body: std.ArrayList(*Stmt)
) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = .{
        .func_declare_stmt = .{
            .name = identifier,
            .params = params,
            .body = body,
        },
    };
    return stmt;
}

const ReturnStmt = struct {
    keyword: Token,
    expr: ?*Expr,

    fn display(self: *const ReturnStmt) void {
        debug.print("return", .{});
        if (self.expr) |expr| {
            debug.print(" ", .{});
            expr.*.display(false);
        }
        debug.print(";\n", .{});
    }
};

pub fn createReturnStmt(
    allocator: Allocator, 
    keyword: Token, expr: ?*Expr
) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = .{
        .return_stmt = .{
            .keyword = keyword,
            .expr = expr,
        },
    };
    return stmt;
}

// -----------------------------------------------------------------------------

const expect = std.testing.expect;
const TestGpa = std.heap.GeneralPurposeAllocator(.{});

test "(-123) unary value" {
    var gpa = TestGpa{};
    const allocator = gpa.allocator();
    
    const optr = Token{
        .token_type = .Minus,
        .lexeme = "-",
        .literal = null,
        .line = 1,
    };

    const right = try allocator.create(Expr);
    defer allocator.destroy(right);
    right.* = Expr{ 
        .literal = LiteralExpr{ 
            .integer = 123 
        },
    };

    const unary = Expr{
        .unary = UnaryExpr{
            .optr = optr,
            .right = right,
        }
    };
    debug.print("Testing Unary (-123): ", .{});
    unary.display(allocator, true);
}

test "(-232 * 45.67) binary expr" {
    var gpa = TestGpa{};
    const allocator = gpa.allocator();

    // NOTE(yemon): It's mostly fine to have stack allocated nodes for testing purposes,
    // but for actual usage, they should probably be heap allocated with their 
    // lifetimes properly managed.

    var left_literal = Expr{
        .literal = LiteralExpr{
            .integer = 232,
        },
    };
    var left = Expr{
        .unary = UnaryExpr{
            .optr = Token{
                .token_type = .Minus,
                .lexeme = "-",
                .literal = null,
                .line = 1
            },
            .right = &left_literal,
        },
    };

    var right = Expr{
        .literal = LiteralExpr{
            .double = 45.67,
        },
    };

    const binary = Expr{
        .binary = BinaryExpr{
            .left = &left,
            .optr = Token{
                .token_type = .Star,
                .lexeme = "*",
                .literal = null,
                .line = 1,
            },
            .right = &right,
        }
    };

    debug.print("Testing Binary (-232 * 45.67): ", .{});
    binary.display(allocator, true);
}
