const std       = @import("std");
const debug     = @import("std").debug;
const eql       = @import("std").mem.eql;
const Allocator = @import("std").mem.Allocator;

const Token = @import("lexer.zig").Token;

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

pub fn createGroupingExpr(allocator: Allocator, inner: *Expr) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .grouping = GroupingExpr{
            .inner = inner,
        },
    };
    return expr;
}

pub fn createLiteral(allocator: Allocator, value: LiteralExpr) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = Expr{
        .literal = value,
    };
    return expr;
}

pub const Expr = union(enum) {
    binary: BinaryExpr,
    unary: UnaryExpr,
    grouping: GroupingExpr,
    literal: LiteralExpr,

    pub fn display(self: Expr, allocator: Allocator, line_break: bool) void { 
        switch (self) {
            .binary => |binary| {
                debug.print("{s}", .{ binary.string(allocator) });
            },

            .unary => |unary| {
                debug.print("{s}", .{ unary.string(allocator) });
            },

            .grouping => |grouping| {
                debug.print("{s}", .{ grouping.string(allocator) });
            },

            .literal => |literal| {
                debug.print("{s}", .{ literal.string(allocator) });
            },
        }
        if (line_break) {
            debug.print("\n", .{});
        }
    }

    fn string(self: Expr, allocator: Allocator) []const u8 {
        switch (self) {
            .binary => |binary| {
                return binary.string(allocator);
            },

            .unary => |unary| {
                return unary.string(allocator);
            },

            .grouping => |grouping| {
                return grouping.string(allocator);
            },

            .literal => |literal| {
                return literal.string(allocator);
            },
        }
    }
};

pub const BinaryExpr = struct {
    left:  *Expr,
    optr:  Token,
    right: *Expr,

    fn string(self: *const BinaryExpr, allocator: Allocator) []const u8 {
        const optr_string = self.optr.toString();
        const str = std.fmt.allocPrint(allocator, "({s} {s} {s})", .{
            optr_string,
            self.left.string(allocator),
            self.right.string(allocator),
        }) catch "(NA)";
        return str;
    }
};

pub const UnaryExpr = struct {
    optr:  Token,
    right: *Expr,

    fn string(self: *const UnaryExpr, allocator: Allocator) []const u8 {
        const optr_string = self.optr.toString();
        const str = std.fmt.allocPrint(allocator, "({s} {s})", .{ 
            optr_string, 
            self.right.string(allocator),
        }) catch "(NA)";
        return str;
    }
};

pub const GroupingExpr = struct {
    inner: *Expr,

    fn string(self: *const GroupingExpr, allocator: Allocator) []const u8 {
        const str = std.fmt.allocPrint(allocator, "({s})", .{
            self.inner.string(allocator),
        }) catch "(NA)";
        return str;
    }
};

pub const LiteralExpr = union(enum) {
    integer: i64,
    double:  f64,
    text:  []u8,
    boolean: bool,
    nil: bool,

    fn string(self: LiteralExpr, allocator: Allocator) []const u8 {
        var str: []const u8 = undefined;
        switch (self) {
            .integer => |value| {
                str = std.fmt.allocPrint(allocator, "{any}", .{ value }) 
                    catch "NA";
            },

            .double => |value| {
                str = std.fmt.allocPrint(allocator, "{d}", .{ value }) 
                    catch "NA";
            },

            .text => |value| {
                str = std.fmt.allocPrint(allocator, "{s}", .{ value }) 
                    catch "NA";
            },

            .boolean => |value| {
                str = std.fmt.allocPrint(allocator, "{any}", .{ value }) 
                    catch "NA";
            },

            .nil => {
                str = "nil";
            }
        }
        return str;
    }
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
