const std       = @import("std");
const fmt       = @import("std").fmt;
const debug     = @import("std").debug;
const eql       = @import("std").mem.eql;
const Allocator = @import("std").mem.Allocator;

const Token = @import("lexer.zig").Token;
const Value = @import("interpreter.zig").Value;

pub const Expr = union(enum) {
    assign: AssignmentExpr,
    binary: BinaryExpr,
    logical: LogicalExpr,
    unary: UnaryExpr,
    grouping: GroupingExpr,
    literal: LiteralExpr,
    variable: VariableExpr,
    func_call: FunctionCall,

    pub fn display(self: Expr, allocator: Allocator, line_break: bool) void { 
        switch (self) {
            .assign => |assignment| {
                debug.print("{s}", .{ assignment.toString(allocator) });
            },
            .binary => |binary| {
                debug.print("{s}", .{ binary.toString(allocator) });
            },
            .logical => |logical| {
                debug.print("{s}", .{ logical.toString(allocator) });
            },
            .unary => |unary| {
                debug.print("{s}", .{ unary.toString(allocator) });
            },
            .grouping => |grouping| {
                debug.print("{s}", .{ grouping.toString(allocator) });
            },
            .literal => |literal| {
                debug.print("{s}", .{ literal.toString(allocator) });
            },
        }
        if (line_break) {
            debug.print("\n", .{});
        }
    }

    pub fn toString(self: Expr, allocator: Allocator) []const u8 {
        switch (self) {
            .assign => |assignment| {
                return assignment.toString(allocator);
            },
            .binary => |binary| {
                return binary.toString(allocator);
            },
            .logical => |logical| {
                return logical.toString(allocator);
            },
            .unary => |unary| {
                return unary.toString(allocator);
            },
            .grouping => |grouping| {
                return grouping.toString(allocator);
            },
            .literal => |literal| {
                return literal.toString(allocator);
            },
            .variable => |variable| {
                return variable.toString();
            },
            .func_call => |func_call| {
                return func_call.toString(allocator);
            },
        }
    }
};

pub const AssignmentExpr = struct {
    name: []u8,
    value: *Expr,

    fn toString(self: *const AssignmentExpr, allocator: Allocator) []const u8 {
        const str = fmt.allocPrint(allocator, "({s}={s})", .{
            self.name, self.value.*.toString(allocator),
        }) catch "(NA)";
        return str;
    }
};

pub fn createAssignmentExpr(allocator: Allocator, variable: Token, value: *Expr) AllocError!*Expr {
    if (variable.lexeme) |lexeme| {
        const target_name = try allocator.alloc(u8, lexeme.len);
        @memcpy(target_name, lexeme);

        const expr = try allocator.create(Expr);
        expr.* = Expr{
            .assign = AssignmentExpr{
                .name = target_name,
                .value = value,
            },
        };
        return expr;
    } else {
        return AllocError.OutOfMemory;
    }
}

pub const BinaryExpr = struct {
    left:  *Expr,
    optr:  Token,
    right: *Expr,

    fn toString(self: *const BinaryExpr, allocator: Allocator) []const u8 {
        const optr_string = self.optr.toString();
        const str = fmt.allocPrint(allocator, "({s} {s} {s})", .{
            optr_string,
            self.left.toString(allocator),
            self.right.toString(allocator),
        }) catch "(NA)";
        return str;
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

    fn toString(self: *const LogicalExpr, allocator: Allocator) []const u8 {
        const optr_string = self.optr.toString();
        const str = fmt.allocPrint(allocator, "{s} {s} {s}", .{
            optr_string,
            self.left.toString(allocator),
            self.right.toString(allocator),
        }) catch "(NA)";
        return str;
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

    fn toString(self: *const UnaryExpr, allocator: Allocator) []const u8 {
        const optr_string = self.optr.toString();
        const str = fmt.allocPrint(allocator, "({s} {s})", .{ 
            optr_string, 
            self.right.toString(allocator),
        }) catch "(NA)";
        return str;
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

    fn toString(self: *const GroupingExpr, allocator: Allocator) []const u8 {
        const str = fmt.allocPrint(allocator, "{s}", .{
            self.inner.toString(allocator),
        }) catch "(NA)";
        return str;
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

    fn toString(self: LiteralExpr, allocator: Allocator) []const u8 {
        var str: []const u8 = undefined;
        switch (self) {
            .integer => |value| {
                str = fmt.allocPrint(allocator, "{any}", .{ value }) 
                    catch "NA";
            },

            .double => |value| {
                str = fmt.allocPrint(allocator, "{d}", .{ value }) 
                    catch "NA";
            },

            .text => |value| {
                str = fmt.allocPrint(allocator, "\"{s}\"", .{ value }) 
                    catch "NA";
            },

            .boolean => |value| {
                str = fmt.allocPrint(allocator, "{any}", .{ value }) 
                    catch "NA";
            },

            .nil => {
                str = "nil";
            }
        }
        return str;
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
    // NOTE(yemon): Double leakage here then the other 'create' functions...
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

pub const FunctionCall = struct {
    callee: *Expr,
    paren: Token,
    arguments: ?std.ArrayList(*Expr),

    pub fn toString(self: FunctionCall, allocator: Allocator) []const u8 {
        if (self.arguments) |args| {
            if (args.items.len > 0) {
                return fmt.allocPrint(allocator, "{s}({} args) ...\n", .{ 
                    self.callee.*.toString(allocator), args.items.len 
                }) catch "func ...";
            } else {
                return fmt.allocPrint(allocator, "{s}() ...\n", .{ self.callee.*.toString(allocator) }) 
                    catch "func ...";
            }
        } else {
            return fmt.allocPrint(allocator, "{s}() ...\n", .{ self.callee.*.toString(allocator) }) catch "func ...";
        }
    }
};

pub fn createFunctionCall(allocator: Allocator, callee: *Expr, paren: Token, arguments: ?std.ArrayList(*Expr)) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .func_call = FunctionCall{
            .callee = callee,
            .paren = paren,
            .arguments = arguments,
        },
    };
    return expr;
}

pub const Stmt = union(enum) {
    variable: VariableStmt,
    print: PrintStmt,
    expr: ExprStmt,
    if_stmt: IfStmt,
    while_stmt: WhileStmt,
    block: Block,
    func_stmt: FunctionStmt,

    pub fn toString(self: Stmt, allocator: Allocator) []const u8 {
        switch (self) {
            .variable => |variable| {
                return variable.toString(allocator);
            },
            .print => |print| {
                return print.toString(allocator);
            },
            .expr => |expr| {
                return expr.toString(allocator);
            },
            .if_stmt => |if_stmt| {
                return if_stmt.toString(allocator);
            },
            .while_stmt => |while_stmt| {
                return while_stmt.toString(allocator);
            },
            .block => |_| {
                return "{...}";
            },

            .func_stmt => |func_stmt| {
                const name = if (func_stmt.name.lexeme) |lexeme| lexeme else "(NA)";
                const str = fmt.allocPrint(allocator, "fun {s}(...)", .{ name }) 
                    catch "fun ...";
                return str;
            },
        }
    }
};

pub const VariableStmt = struct {
    name: []u8,
    initializer: ?*Expr,

    fn toString(self: *const VariableStmt, allocator: Allocator) []const u8 {
        if (self.initializer) |initializer| {
            const str = fmt.allocPrint(allocator, "var {s} = {s};", .{ self.name, initializer.*.toString(allocator) }) catch "-";
            return str;
        } else {
            const str = fmt.allocPrint(allocator, "var {s};", .{ self.name }) catch "-";
            return str;
        }
    }
};

pub const AllocError = error {
    OutOfMemory,
};

pub fn createVariableStmt(allocator: Allocator, identifier: Token, initializer: ?*Expr) AllocError!*Stmt {
    if (identifier.lexeme) |lexeme| {
        const target_name = try allocator.alloc(u8, lexeme.len);
        @memcpy(target_name, lexeme);

        const stmt = try allocator.create(Stmt);
        stmt.* = Stmt{
            .variable = VariableStmt{
                .name = target_name,
                .initializer = initializer,
            },
        };
        return stmt;
    } else {
        return AllocError.OutOfMemory;
    }
}

pub const PrintStmt = struct {
    expr: *Expr,

    fn toString(self: *const PrintStmt, allocator: Allocator) []const u8 {
        const str = fmt.allocPrint(allocator, "print {s};\n", .{ 
            self.expr.*.toString(allocator) 
        }) catch "print -";
        return str;
    }
};

pub fn createPrintStmt(allocator: Allocator, expr: *Expr) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = Stmt{
        .print = PrintStmt{
            .expr = expr,
        },
    };
    return stmt;
}

pub const ExprStmt = struct {
    expr: *Expr,

    fn toString(self: *const ExprStmt, allocator: Allocator) []const u8 {
        const str = fmt.allocPrint(allocator, "{s};\n", .{ 
            self.expr.*.toString(allocator) 
        }) catch "-";
        return str;
    }
};

pub fn createExprStmt(allocator: Allocator, expr: *Expr) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = Stmt{
        .expr = ExprStmt{
            .expr = expr,
        },
    };
    return stmt;
}

pub const IfStmt = struct {
    condition: *Expr,
    then_branch: *Stmt,
    else_branch: ?*Stmt,

    fn toString(self: *const IfStmt, allocator: Allocator) []const u8 {
        // NOTE(yemon): Zig's formatting strings does not accept '{' or '}' right now AFAIK...
        const cond_str = fmt.allocPrint(allocator, "if ({s}) ...", .{ 
            self.condition.toString(allocator) 
        }) catch "if ...";
        if (self.else_branch) |_| {
            const str = fmt.allocPrint(allocator, "{s}\nelse ...", .{ cond_str }) catch "-";
            return str;
        } else {
            return cond_str;
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

pub const WhileStmt = struct {
    condition: *Expr,
    body: *Stmt,

    fn toString(self: *const WhileStmt, allocator: Allocator) []const u8 {
        // NOTE(yemon): Zig's formatting strings does not accept '{' or '}' right now AFAIK...
        const str = fmt.allocPrint(allocator, "while ({s}) ...", .{
            self.condition.toString(allocator)
        }) catch "while ...";
        return str;
    }
};

pub fn createWhileStmt(allocator: Allocator, condition: *Expr, body: *Stmt) !*Stmt {
    const stmt = try allocator.create(Stmt);
    stmt.* = Stmt{
        .while_stmt = WhileStmt{
            .condition = condition,
            .body = body,
        },
    };
    return stmt;
}

pub const Block = struct {
    statements: std.ArrayList(*Stmt),

    pub fn init(allocator: Allocator) Block {
        return .{
            .statements = std.ArrayList(*Stmt).init(allocator),
        };
    }
};

pub const FunctionStmt = struct {
    name: Token,
    params: ?std.ArrayList(Token),
    body: std.ArrayList(*Stmt),
};

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
