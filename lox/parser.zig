const std       = @import("std");
const debug     = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const lexer     = @import("lexer.zig");
const Token     = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const ast       = @import("ast.zig");

// NOTE(yemon): Inferred error sets are not compatible with
// recursive function calls. So, Zig needs an explicit error 
// definition with all possible errors from the call stack merged.
const ParserError = error {
    UnknownPrimaryNode,
    InvalidStringComposition,
    InvalidSyncPosition,
    DunnoIdentifier,

    OutOfMemory,
};

pub const Parser = struct {
    tokens: *std.ArrayList(Token),
    current: usize,
    has_error: bool,
    debug_print: bool,

    pub fn init(tokens: *std.ArrayList(Token)) Parser {
        return .{
            .tokens = tokens,
            .current = 0,
            .has_error = false,
            .debug_print = false,
        };
    }

    pub fn parse(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        debugPrint(self, "Start parsing the expression...\n", .{});
        return self.expression(allocator);
    }

    // NOTE(yemon): The expression parser grammar doesn't currently have 
    // assignments and statements yet.
    fn expression(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        if (self.equality(allocator)) |it| {
            debugPrint( self, "Root expression done with {s}\n", .{ it.toString(allocator) });
            return it;
        } else |err| {
            debugPrint(self, "Root expression done with error {}\n", .{ err });
            return err;
        }
    }

    fn equality(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        var expr = try self.comparision(allocator);
        const tokens_to_check = [_]TokenType{ .BangEqual, .EqualEqual };
        while (self.advanceIfMatchedAny(&tokens_to_check)) {
            // NOTE(yemon): `previous()` call here wouldn't panic, 
            // since the `advance()` call was successful. So, it's guarantee
            // to have a token left of the current one.
            const optr = self.previous().?;
            const right = try self.comparision(allocator);
            expr = try ast.createBinaryExpr(allocator, expr, optr, right);
            debugPrint(self, "Equality done with binary. {s}\n", .{ expr.toString(allocator) });
        } 
        debugPrint(self, "Equality done. {s}\n", .{ expr.toString(allocator) });
        return expr;
    }
    
    fn comparision(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        var expr = try self.term(allocator);
        const tokens_to_check = [_]TokenType{ 
            .Greater, .GreaterEqual, .Less, .LessEqual 
        };
        while (self.advanceIfMatchedAny(&tokens_to_check)) {
            const optr = self.previous().?;
            const right = try self.comparision(allocator);
            expr = try ast.createBinaryExpr(allocator, expr, optr, right);
            debugPrint(self, "Comparison done with binary. {s}\n", .{ expr.toString(allocator) });
        }
        debugPrint(self, "Comparision done. {s}\n", .{ expr.toString(allocator) });
        return expr;
    }

    fn term(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        var expr = try self.factor(allocator);
        const tokens_to_check = [_]TokenType{ .Minus, .Plus };
        while (self.advanceIfMatchedAny(&tokens_to_check)) {
            const optr = self.previous().?;
            const right = try self.factor(allocator);
            expr = try ast.createBinaryExpr(allocator, expr, optr, right);
            debugPrint(self, "Terminal done with binary. {s}\n", .{ expr.toString(allocator) });
        } 
        debugPrint(self, "Terminal done. {s}\n", .{ expr.toString(allocator) });
        return expr;
    }

    fn factor(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        var expr = try self.unary(allocator);
        const tokens_to_check = [_]TokenType{ .Slash, .Star };
        while (self.advanceIfMatchedAny(&tokens_to_check)) {
            const optr = self.previous().?;
            const right = try self.unary(allocator);
            expr = try ast.createBinaryExpr(allocator, expr, optr, right);
            debugPrint(self, "Factor done with binary. {s}\n", .{ expr.toString(allocator) });
        } 
        debugPrint(self, "Factor done. {s}\n", .{ expr.toString(allocator) });
        return expr;
    }

    fn unary(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        const tokens_to_check = [_]TokenType{ .Bang, .Minus };
        if (self.advanceIfMatchedAny(&tokens_to_check)) {
            const optr = self.previous().?;
            const right = try self.unary(allocator);
            const expr = try ast.createUnaryExpr(allocator, optr, right);
            debugPrint(self, "Unary done with unary. {s}\n", .{ expr.toString(allocator) });
            return expr;
        } else {
            if (self.primary(allocator)) |it| {
                debugPrint(self, "Unary done with primary. {s}\n", .{ it.toString(allocator) });
                return it;
            } else |err| {
                debugPrint(self, "Unary done with error {}\n", .{ err });
                return err;
            }
        }
    }

    fn primary(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        debugPrint(self, "Primary...\n", .{});
        const token = self.peek();

        if (self.check(TokenType.Number)) {
            _ = self.advance();

            const literal = if (token.literal) |it| it else "0";
            const int_value = std.fmt.parseInt(i64, literal, 10) catch unreachable;
            const int_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
                .integer = int_value,
            });
            debugPrint(self, "Primary done with int literal. {s}\n", .{ int_lit.toString(allocator) });
            return int_lit;
        }

        if (self.check(TokenType.NumberFractional)) {
            _ = self.advance();

            const literal = if (token.literal) |it| it else "0";
            const double_value = std.fmt.parseFloat(f64, literal) catch unreachable;
            const double_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
                .double = double_value,
            });
            debugPrint(self, "Primary done with double literal. {s}\n", .{ double_lit.toString(allocator) });
            return double_lit;
        }

        if (self.check(TokenType.String)) {
            _ = self.advance();
            if (token.literal) |literal| {
                const string_lit = try ast.createStringLiteral(allocator, literal);
                debugPrint(self, "Primary done with string literal. {s}\n", .{ string_lit.toString(allocator) });
                return string_lit;
            } else {
                return error.InvalidStringComposition;
            }
        }
        
        if (self.check(TokenType.True)) {
            _ = self.advance();
            const true_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
                .boolean = true,
            });
            debugPrint(self, "Primary done with true literal. {s}\n", .{ true_lit.toString(allocator) });
            return true_lit;
        }
        if (self.check(TokenType.False)) {
            _ = self.advance();
            const false_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
                .boolean = false,
            });
            debugPrint(self, "Primary done with false literal. {s}\n", .{ false_lit.toString(allocator) });
            return false_lit;
        }
        if (self.check(TokenType.Nil)) {
            _ = self.advance();
            const nil_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
                .nil = true,
            });
            debugPrint(self, "Primary done with nil literal. {s}\n", .{ nil_lit.toString(allocator) });
            return nil_lit;
        }

        if (self.check(TokenType.LeftParen)) {
            _ = self.advance();

            const inner_expr = try self.expression(allocator);
            const group = try ast.createGroupingExpr(allocator, inner_expr);

            // NOTE(yemon): This just handles the error and report it in place.
            // Maybe it should unwind/return over the tree until the "statement boundary" is hit somehow...
            const parser_error = self.consume(TokenType.RightParen, "Expecting \')\' after the expression.");
            if (parser_error.error_message) |error_message| {
                self.has_error = true;
                reportError(parser_error.token, error_message);
            }

            debugPrint(self, "Primary done with group. {s}\n", .{ group.toString(allocator) });
            return group;
        }

        debugPrint(self, "Primary done with error.\n", .{});
        return error.UnknownPrimaryNode;
    }

    // NOTE(yemon): This'll usually get called right after `ParserError` is hit.
    // The easiest synchronization point is at the 'statement boundaries',
    // ie., between ';' and the start of next statement keywords.
    fn synchronize(self: *Parser) !void {
        _ = self.advance();
        while (!self.isEnd()) : (_ = self.advance()) {
            if (self.previous()) |it| {
                if (it.token_type == .Semicolon) {
                    return;
                }
            } else {
                return error.InvalidSyncPosition;
            }

            switch (self.peek()) {
                .Class => return,
                .Fun => return,
                .Var => return,
                .If => return,
                .For => return,
                .While => return,
                .Print => return,
                .Return => return,
            }
        }
    }

// -----------------------------------------------------------------------------

    fn advanceIfMatchedAny(self: *Parser, token_types: []const TokenType) bool {
        for (token_types) |token_type| {
            if (self.isEnd()) {
                return false;
            }
            if (self.peek().token_type == token_type) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *const Parser, token_type: TokenType) bool {
        if (self.isEnd()) {
            return false;
        } else {
            return self.peek().token_type == token_type;
        }
    }

    fn peek(self: *const Parser) Token {
        return self.tokens.*.items[self.current];
    }

    fn previous(self: *const Parser) ?Token {
        if (self.current == 0) {
            return null;
        } else {
            return self.tokens.*.items[self.current-1];
        }
    }

    fn advance(self: *Parser) Token {
        if (!self.isEnd()) {
            self.current += 1;
            return self.tokens.*.items[self.current];
        } else {
            if (self.previous()) |it| {
                return it;
            } else {
                return .{
                    .token_type = .Invalid,
                    .lexeme = null,
                    .literal = null,
                    .line = 0,
                };
            }
        }
    }

    fn consume(self: *Parser, token_type: TokenType, error_message: []const u8) ParserResult {
        if (self.check(token_type)) {
            const token = self.advance();
            return .{
                .token = token,
                .error_message = null,
            };
        } else {
            const next_token = self.peek();
            return .{
                .token = next_token,
                .error_message = error_message,
            };
        }
    }

    fn isEnd(self: *const Parser) bool {
        if (self.current >= self.tokens.*.items.len) {
            return true;
        }
        return self.peek().token_type == .Eof;
    }
};

const ParserResult = struct {
    token: Token, 
    error_message: ?[]const u8,
};

// TODO(yemon): Might need a bit more permanent place for this
fn report(line: u32, where_message: []const u8, error_message: []const u8) void {
    debug.print("[{}] ERROR {s}: {s}\n", .{ line, where_message, error_message });
}

// TODO(yemon): Might need a bit more permanent place for this
fn reportError(token: Token, message: []const u8) void {
    if (token.token_type == .Eof) {
        report(token.line, "at end", message);
    } else {
        if (token.lexeme) |lexeme| {
            debug.print("[{}] ERROR on lexeme {s}: {s}\n", .{ 
                token.line, lexeme, message 
            });
        } else {
            report(token.line, "(Unidentified lexeme)", message);
        }
    }
}

// TODO(yemon): Reporting functions needs a bit of consolidations
fn reportPrint(allocator: Allocator, token: Token, index: u32, comptime message: []const u8, args: anytype) void {
    const fmt_message = std.fmt.allocPrint(allocator, message, args) catch "";
    debug.print("Error when parsing token {} at position index {}: {s}\n", .{
        token, index, fmt_message,
    });
}

fn debugPrint(self: *const Parser, comptime fmt: []const u8, args: anytype) void {
    if (!self.debug_print) {
        return;
    }
    debug.print(fmt, args);
}