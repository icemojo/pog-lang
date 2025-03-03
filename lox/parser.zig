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
    InvalidSyncToken,
    InvalidIdentifierDeclaration,
    DunnoIdentifier,
    CanSkip,

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

    pub fn parse(self: *Parser, allocator: Allocator) ParserError!std.ArrayList(*ast.Stmt) {
        var statements = std.ArrayList(*ast.Stmt).init(allocator);
        parsing: while (!self.isEnd()) {
            if (self.declaration(allocator)) |stmt| {
                try statements.append(stmt);
            } else |err| switch (err) {
                ParserError.CanSkip => continue :parsing,
                else => {
                    debugPrint(self, "Parsing a statement failed with '{}'. Need to synchronize until end of statement.\n", .{ err });
                    self.synchronize() catch |sync_err| {
                        debugPrint(self, "Synchronization failed with error '{}'.\n", .{ sync_err });
                        break :parsing;
                    };
                    continue :parsing;
                }
            }
        }
        return statements;
    }

    fn declaration(self: *Parser, allocator: Allocator) ParserError!*ast.Stmt {
        if (self.advanceIfMatchedAny(&[_]TokenType{ .Var })) {
            return try self.variableDeclareStmt(allocator);
        } else {
            return try self.statement(allocator);
        }
    }

    fn variableDeclareStmt(self: *Parser, allocator: Allocator) ParserError!*ast.Stmt {
        debugPrint(self, "Seems like a variable declaration statement...\n", .{});
        var parser_result = self.consume(.Identifier, "Expect a valid identifier declaration.");
        if (parser_result.error_message) |error_message| {
            self.has_error = true;
            reportError(parser_result.token, error_message);
            return error.InvalidIdentifierDeclaration;
        }

        // TODO(yemon): Do I need to add the 'identifier' to the interpreter state?
        if (self.advanceIfMatchedAny(&[_]TokenType{ .Equal })) {
            const initializer = self.expression(allocator) catch null;
            return try ast.createVariableStmt(allocator, parser_result.token, initializer);
        } else {
            self.has_error = true;
            const current_token = self.peek();
            reportError(current_token, "Invalid identifier declaration, must be followed by an equal and some form of initializer expression.");
            return error.InvalidIdentifierDeclaration;
        }

        parser_result = self.consume(.Semicolon, "Expect an EOL terminator ';' after an identifier declaration expression.");
        if (parser_result.error_message) |error_message| {
            self.has_error = true;
            reportError(parser_result.token, error_message);
            return error.InvalidIdentifierDeclaration;
        }
    }

    fn statement(self: *Parser, allocator: Allocator) ParserError!*ast.Stmt {
        if (self.advanceIfMatchedAny(&[_]TokenType{ .Print })) {
            return try self.printStmt(allocator);
        } else {
            return try self.expressionStmt(allocator);
        }
    }

    fn printStmt(self: *Parser, allocator: Allocator) ParserError!*ast.Stmt {
        debugPrint(self, "Seems like a print statement...\n", .{});
        const expr = try self.expression(allocator);
        _ = self.consume(.Semicolon, "Expect ';' after value.");
        const stmt = try ast.createPrintStmt(allocator, expr);
        return stmt;
    }

    fn expressionStmt(self: *Parser, allocator: Allocator) ParserError!*ast.Stmt {
        debugPrint(self, "Seems like an expression statement...\n", .{});
        const expr = try self.expression(allocator);
        _ = self.consume(.Semicolon, "Expect ';' after expression.");
        const stmt = try ast.createExprStmt(allocator, expr);
        return stmt;
    }

    fn expression(self: *Parser, allocator: Allocator) ParserError!*ast.Expr {
        if (self.equality(allocator)) |it| {
            debugPrint(self, "Root expression done with {s}\n", .{ it.toString(allocator) });
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
        const current_token = self.peek();
        debugPrint(self, "Primary... {s}\n", .{ current_token.toString() });

        // if (current_token.isTerminator()) {
        //     debugPrint(self, "Terminator reached. No need to parse.\n", .{});
        //     return error.CanSkip;
        // }

        if (self.check(TokenType.Number)) {
            _ = self.advance();

            const literal = if (current_token.literal) |it| it else "0";
            const int_value = std.fmt.parseInt(i64, literal, 10) catch unreachable;
            const int_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
                .integer = int_value,
            });
            debugPrint(self, "Primary done with int literal. {s}\n", .{ int_lit.toString(allocator) });
            return int_lit;
        }

        if (self.check(TokenType.NumberFractional)) {
            _ = self.advance();

            const literal = if (current_token.literal) |it| it else "0";
            const double_value = std.fmt.parseFloat(f64, literal) catch unreachable;
            const double_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
                .double = double_value,
            });
            debugPrint(self, "Primary done with double literal. {s}\n", .{ double_lit.toString(allocator) });
            return double_lit;
        }

        if (self.check(TokenType.String)) {
            _ = self.advance();
            if (current_token.literal) |literal| {
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
            const parser_result = self.consume(TokenType.RightParen, "Expecting \')\' after the expression.");
            if (parser_result.error_message) |error_message| {
                self.has_error = true;
                reportError(parser_result.token, error_message);
            }

            debugPrint(self, "Primary done with group. {s}\n", .{ group.toString(allocator) });
            return group;
        }

        if (self.check(TokenType.Identifier)) {
            _ = self.advance();
            const variable = try ast.createVariableExpr(allocator, current_token);
            debugPrint(self, "Primary done with variable identifier. {s}\n", .{ variable.toString(allocator) });
            return variable;
        }

        debugPrint(self, "Primary done with error on token: {s}.\n", .{ current_token.toString() });
        return error.UnknownPrimaryNode;
    }

    // NOTE(yemon): This'll usually get called right after `ParserError` is hit.
    // The easiest synchronization point is at the 'statement boundaries',
    // ie., between ';' and the start of next statement keywords.
    fn synchronize(self: *Parser) ParserError!void {
        _ = self.advance();
        while (!self.isEnd()) : (_ = self.advance()) {
            if (self.previous()) |it| {
                if (it.token_type == .Semicolon) {
                    return;
                }
            } else {
                return ParserError.InvalidSyncPosition;
            }

            switch (self.peek().token_type) {
                .Class => return,
                .Function => return,
                .Var => return,
                .If => return,
                .For => return,
                .While => return,
                .Print => return,
                .Return => return,
                else => return ParserError.InvalidSyncToken,
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

    fn advance(self: *Parser) ?Token {
        if (!self.isEnd()) {
            self.current += 1;
            return self.tokens.*.items[self.current];
        } else {
            return null;
        }
    }

    fn consume(self: *Parser, token_type: TokenType, error_message: []const u8) ParserResult {
        if (self.check(token_type)) {
            if (self.advance()) |next_token| {
                return .{
                    .token = next_token,
                    .error_message = null,
                };
            } else {
                const current_token = self.peek();
                const buffer: []u8 = undefined;
                const message = 
                    std.fmt.bufPrint(
                        buffer, 
                        "Unable to consume current token from {s}.", 
                        .{ current_token.toString() }) 
                    catch "No space left even to compose an error message.";
                return .{
                    .token = current_token,
                    .error_message = message,
                };
            }
        } else {
            const current_token = self.peek();
            return .{
                .token = current_token,
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