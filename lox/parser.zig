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
    UnknownPrimaryToken,
    InvalidStringComposition,
    InvalidSyncPosition,
    InvalidSyncToken,
    InvalidIdentifierDeclaration,
    InvalidAssignmentTarget,
    DunnoIdentifier,
    CanSkip,

    OutOfMemory,
};

const Self = @This();

tokens: *std.ArrayList(Token),
current: usize,
has_error: bool,
debug_print: bool,
debug_ast: bool,

pub fn init(tokens: *std.ArrayList(Token), debug_print: bool, debug_ast: bool) Self {
    return .{
        .tokens = tokens,
        .current = 0,
        .has_error = false,
        .debug_print = debug_print,
        .debug_ast = debug_ast,
    };
}

pub fn parse(self: *Self, allocator: Allocator) ParserError!std.ArrayList(*ast.Stmt) {
    var statements = std.ArrayList(*ast.Stmt).init(allocator);
    parsing: while (!self.isEnd()) {
        if (self.declaration(allocator)) |stmt| {
            try statements.append(stmt);
        } else |err| {
            errorPrint(self, "Parsing a statement failed with '{}'. Need to synchronize until end of statement.\n", .{ err });
            self.synchronize() catch |sync_err| {
                errorPrint(self, "Synchronization failed with error '{}'.\n", .{ sync_err });
                break :parsing;
            };
            continue :parsing;
        }
    }
    self.debugPrint("End of all the statements.\n", .{});

    if (self.debug_ast) {
        debug.print("Final AST:\n", .{});
        for (statements.items) |stmt| {
            stmt.*.display(0);
        }
    }

    return statements;
}

fn declaration(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    if (self.advanceIfMatched(.Var)) {
        return try self.variableDeclareStmt(allocator);
    } else if (self.advanceIfMatched(.Function)) {
        return try self.functionDeclareStmt(allocator, "function");
    } else {
        return try self.statement(allocator);
    }
}

fn variableDeclareStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like a variable declaration statement...\n", .{});
    var parser_result = self.consume(.Identifier, "Expect a valid identifier declaration.");
    if (parser_result.error_message) |error_message| {
        self.has_error = true;
        reportError(parser_result.token, error_message);
        return ParserError.InvalidIdentifierDeclaration;
    }

    var var_stmt: *ast.Stmt = undefined;
    const identifier: Token = parser_result.token;
    self.debugPrint("Identifier name is '{s}'\n", .{ identifier.lexeme.? });
    var initializer: ?*ast.Expr = null;
    if (self.advanceIfMatched(.Equal)) {
        initializer = try self.expression(allocator);
    }
    var_stmt = ast.createVariableStmt(allocator, identifier, initializer) catch |err| switch (err) {
        ast.AllocError.OutOfMemory => {
            return ParserError.OutOfMemory;
        },
    };

    // TODO(yemon): this would cause REPL to report an error if the statement 
    // being entered didn't get terminated with ';'
    // Right now, the temporary solution is to add manually add ';' at the end of the
    // tokenizer scanning chain. Hacky, don't like it at all!
    parser_result = self.consume(.Semicolon, "Expect an EOL terminator ';' after an identifier declaration expression.");
    if (parser_result.error_message) |error_message| {
        self.has_error = true;
        reportError(parser_result.token, error_message);
        return ParserError.InvalidIdentifierDeclaration;
    }

    return var_stmt;
}

fn functionDeclareStmt(self: *Self, allocator: Allocator, comptime kind: []const u8) ParserError!*ast.Stmt {
    var parser_result = self.consume(.Identifier, "Expect " ++ kind ++ " name.");
    const name: Token = if (parser_result.error_message == null) parser_result.token 
        else Token{ .token_type = .Invalid, .lexeme = null, .literal = null, .line = 0 };

    _ = self.consume(.LeftParen, "Expect '(' after " ++ kind ++ " name.");
    var params = std.ArrayList(Token).init(allocator);

    while (!self.check(.RightParen) and params.items.len <= 255) {
        parser_result = self.consume(.Identifier, "Expect an identifier as a " ++ kind ++ " parameter.");
        if (parser_result.error_message == null) {
            try params.append(parser_result.token);
        } else {
            // TODO(yemon): Report the parameter parsing error in place?
        }
        _ = self.advanceIfMatched(.Comma);
    }
    _ = self.consume(.RightParen, "Expect ')' after the end of parameters.");

    _ = self.consume(.LeftBrace, "Expect '{' before the " ++ kind ++ " body.");
    const body = try self.blockStmts(allocator);

    // TODO(yemon): the 'name' identification is wrong, the last argument name is
    // being marked as the function name here...
    const stmt = try allocator.create(ast.Stmt);
    stmt.* = ast.Stmt{
        .func_stmt = ast.FunctionStmt{
            .name = name,
            .params = if (params.items.len > 0) params else null,
            .body = body,
        },
    };
    return stmt;
}

fn statement(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    if (self.advanceIfMatched(.Print)) {
        return try self.printStmt(allocator);
    } else if (self.advanceIfMatched(.If)) {
        return try self.ifStmt(allocator);
    } else if (self.advanceIfMatched(.While)) {
        return try self.whileStmt(allocator);
    } else if (self.advanceIfMatched(.For)) {
        return try self.forStmt(allocator);
    } else if (self.advanceIfMatched(.LeftBrace)) {
        var block = ast.Block.init(allocator);
        const statements = try self.blockStmts(allocator);
        block.statements = statements;

        const stmt = try allocator.create(ast.Stmt);
        stmt.* = ast.Stmt{
            .block = block,
        };
        return stmt;
    } else {
        return try self.expressionStmt(allocator);
    }
}

fn printStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like a print statement...\n", .{});
    const expr = try self.expression(allocator);
    _ = self.consume(.Semicolon, "Expect ';' after value.");
    const stmt = try ast.createPrintStmt(allocator, expr);
    return stmt;
}

fn ifStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like an if statement block...\n", .{});
    _ = self.consume(.LeftParen, "Expect '(' after 'if'.");
    const condition = try self.expression(allocator);
    _ = self.consume(.RightParen, "Expect ')' after if condition.");

    const then_branch = try self.statement(allocator);
    var else_branch: ?*ast.Stmt = null;
    if (self.advanceIfMatched(.Else)) {
        else_branch = try self.statement(allocator);
    }

    const if_stmt = try ast.createIfStmt(allocator, condition, then_branch, else_branch);
    return if_stmt;
}

fn whileStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like a while statement block...\n", .{});
    _ = self.consume(.LeftParen, "Expect '(' after 'while'.");
    const condition = try self.expression(allocator);
    _ = self.consume(.RightParen, "Expect ')' after 'while' condition.");

    const body = try self.statement(allocator);

    const while_stmt = try ast.createWhileStmt(allocator, condition, body);
    return while_stmt;
}

fn forStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like a for statement block...\n", .{});
    _ = self.consume(.LeftParen, "Expect '(' after 'for'.");
    var initializer: ?*ast.Stmt = null;
    if (self.advanceIfMatched(.Semicolon)) {
        initializer = null;
    } else if (self.advanceIfMatched(.Var)) {
        initializer = try self.variableDeclareStmt(allocator);
    } else {
        initializer = try self.expressionStmt(allocator);
    }

    var condition: *ast.Expr = undefined;
    if (!self.check(.Semicolon)) {
        condition = try self.expression(allocator);
    } else {
        condition = try ast.createLiteral(allocator, ast.LiteralExpr{ .boolean = true });
    }
    _ = self.consume(.Semicolon, "Expect ';' after 'for' loop condition.");

    var increment: ?*ast.Expr = null;
    if (!self.check(.RightParen)) {
        increment = try self.expression(allocator);
    }
    _ = self.consume(.RightParen, "Expect ')' after 'for' clauses.");

    // NOTE(yemon): De-sugered loop structure instead of having separate AST node 
    // for the 'for' loop, composing it similar to a 'block' using the 'while' loop.
    var body_block = ast.Block.init(allocator);
    const body_stmt = try self.statement(allocator);
    try body_block.statements.append(body_stmt);

    if (increment) |it| {
        const increment_stmt = try ast.createExprStmt(allocator, it);
        try body_block.statements.append(increment_stmt);
    }

    const loop_body = try allocator.create(ast.Stmt);
    loop_body.* = ast.Stmt{
        .block = body_block,
    };
    const for_loop = try ast.createWhileStmt(allocator, condition, loop_body);

    if (initializer) |it| {
        var for_loop_initialized = ast.Block.init(allocator);
        try for_loop_initialized.statements.append(it);
        try for_loop_initialized.statements.append(for_loop);

        const result = try allocator.create(ast.Stmt);
        result.* = ast.Stmt{
            .block = for_loop_initialized,
        };
        return result;
    } else {
        return for_loop;
    }
}

// NOTE(yemon): This is not returning `*ast.Stmt` on purpose.
fn blockStmts(self: *Self, allocator: Allocator) ParserError!std.ArrayList(*ast.Stmt) {
    var statements = std.ArrayList(*ast.Stmt).init(allocator);
    while (!self.check(.RightBrace) and !self.isEnd()) {
        const block_stmt = try self.declaration(allocator);
        try statements.append(block_stmt);
    }
    _ = self.consume(.RightBrace, "Expect '}' at the end of a block.");
    return statements;
}

fn expressionStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like an expression statement...\n", .{});
    const expr = try self.expression(allocator);
    _ = self.consume(.Semicolon, "Expect ';' after expression.");
    const stmt = try ast.createExprStmt(allocator, expr);
    return stmt;
}

fn expression(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    if (self.assignment(allocator)) |expr| {
        if (self.debug_print) {
            debug.print("Root expression done with ", .{});
            expr.*.display(true);
        }

        return expr;
    } else |err| {
        self.debugPrint("Root expression done with error {}\n", .{ err });
        return err;
    }
}

// NOTE(yemon): assignment expression will get more complex, when the objects and 
// field accessors come into play.
fn assignment(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    const expr = try self.logicOr(allocator);

    if (self.advanceIfMatched(.Equal)) {
        const value = try self.assignment(allocator);
        switch (expr.*) {
            .variable => |variable| {
                const assign_expr = ast.createAssignmentExpr(allocator, variable, value) catch |err| switch (err) {
                    ast.AllocError.OutOfMemory => {
                        return ParserError.OutOfMemory;
                    },
                };

                if (self.debug_print) {
                    debug.print("Assignment done with ", .{});
                    assign_expr.*.display(true);
                }
                
                return assign_expr;
            },
            else => {
                return ParserError.InvalidAssignmentTarget;
            },
        }

        const equals: Token = self.previous().?;
        reportError(equals, "Invalid assignment target.");
    }

    self.debugPrint("Assignment done with non-assignment equality.\n", .{});
    return expr;
}

fn logicOr(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.logicAnd(allocator);

    if (self.advanceIfMatched(.Or)) {
        const optr = self.previous().?;
        const right = try self.logicAnd(allocator);
        expr = try ast.createLogicalExpr(allocator, expr, optr, right);

        if (self.debug_print) {
            debug.print("Logical done with 'or' condition. ", .{});
        }
    }

    if (self.debug_print) {
        debug.print("Logical 'or' done. ", .{});
        expr.*.display(true);
    }

    return expr;
}

fn logicAnd(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.equality(allocator);

    if (self.advanceIfMatched(.And)) {
        const optr = self.previous().?;
        const right = try self.equality(allocator);
        expr = try ast.createLogicalExpr(allocator, expr, optr, right);

        if (self.debug_print) {
            debug.print("Logicial done with 'and' condition. ", .{});
        }
    }

    if (self.debug_print) {
        debug.print("Logical 'and' done. ", .{});
        expr.*.display(true);
    }

    return expr;
}

fn equality(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.comparision(allocator);
    const tokens_to_check = [_]TokenType{ .BangEqual, .EqualEqual };
    while (self.advanceIfMatchedAny(&tokens_to_check)) {
        // NOTE(yemon): `previous()` call here wouldn't panic, 
        // since the `advance()` call was successful. So, it's guarantee
        // to have a token left of the current one.
        const optr = self.previous().?;
        const right = try self.comparision(allocator);
        expr = try ast.createBinaryExpr(allocator, expr, optr, right);

        if (self.debug_print) {
            debug.print("Equality done with binary. ", .{});
        }
    } 
    
    if (self.debug_print) {
        debug.print("Equality done. ", .{});
        expr.*.display(true);
    }

    return expr;
}

fn comparision(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.term(allocator);
    const tokens_to_check = [_]TokenType{ 
        .Greater, .GreaterEqual, .Less, .LessEqual 
    };
    while (self.advanceIfMatchedAny(&tokens_to_check)) {
        const optr = self.previous().?;
        const right = try self.comparision(allocator);
        expr = try ast.createBinaryExpr(allocator, expr, optr, right);
        
        if (self.debug_print) {
            debug.print("Comparison done with binary. ", .{});
        }
    }

    if (self.debug_print) {
        debug.print("Comparison done. ", .{});
        expr.*.display(true);
    }

    return expr;
}

fn term(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.factor(allocator);
    const tokens_to_check = [_]TokenType{ .Minus, .Plus };
    while (self.advanceIfMatchedAny(&tokens_to_check)) {
        const optr = self.previous().?;
        const right = try self.factor(allocator);
        expr = try ast.createBinaryExpr(allocator, expr, optr, right);

        if (self.debug_print) {
            debug.print("Terminal done with binary. ", .{});
        }
    } 

    if (self.debug_print) {
        debug.print("Terminal done. ", .{});
        expr.*.display(true);
    }

    return expr;
}

fn factor(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.unary(allocator);
    const tokens_to_check = [_]TokenType{ .Slash, .Star };
    while (self.advanceIfMatchedAny(&tokens_to_check)) {
        const optr = self.previous().?;
        const right = try self.unary(allocator);
        expr = try ast.createBinaryExpr(allocator, expr, optr, right);

        if (self.debug_print) {
            debug.print("Factor done with binary. ", .{});
        }
    } 

    if (self.debug_print) {
        debug.print("Factor done. ", .{});
        expr.*.display(true);
    }

    return expr;
}

fn unary(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    const tokens_to_check = [_]TokenType{ .Bang, .Minus };
    if (self.advanceIfMatchedAny(&tokens_to_check)) {
        const optr = self.previous().?;
        const right = try self.unary(allocator);
        const expr = try ast.createUnaryExpr(allocator, optr, right);

        if (self.debug_print) {
            debug.print("Unary done with unary prefix. ", .{});
            expr.*.display(true);
        }
        return expr;
    } else {
        const func = try self.funcCall(allocator);

        if (self.debug_print) {
            debug.print("Unary done with function call.\n", .{});
        }
        return func;
    }
}

fn funcCall(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.primary(allocator);

    while (true) {
        if (self.advanceIfMatched(.LeftParen)) {
            expr = try self.finishCall(allocator, expr);

            if (self.debug_print) {
                debug.print("Function call done.\n", .{});
            }
            return expr;
        } else {
            if (self.debug_print) {
                debug.print("Invalid function call. Probably a syntax error.\n", .{});
            }
            break;
        }
    }

    if (self.debug_print) {
        debug.print("Function call done with primary.\n", .{});
        expr.*.display(true);
    }
    return expr;
}

fn finishCall(self: *Self, allocator: Allocator, callee: *ast.Expr) ParserError!*ast.Expr {
    var arguments = std.ArrayList(*ast.Expr).init(allocator);

    if (!self.check(.RightParen)) {
        var arg = try self.expression(allocator);
        try arguments.append(arg);
        while (self.advanceIfMatched(.Comma)) {
            if (arguments.items.len > 255) {
                // NOTE(yemon): report error instead of throwing, since throwing will 
                // kick into the panic mode and try to synchronize.
                reportError(self.peek(), "Functions cannot have more than 255 arguments.");
            }
            arg = try self.expression(allocator);
            try arguments.append(arg);
        }
    }
    var paren: Token =  undefined;
    const parser_result = self.consume(.RightParen, "Expect ')' after function arguments.");
    if (parser_result.error_message == null) {
        paren = parser_result.token;
    } else {
        // TODO(yemon): should probably assign something to `paren`
    }

    return ast.createFunctionCall(allocator, callee, paren, arguments);
}

fn primary(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    const current_token = self.peek();
    self.debugPrint("Primary... {s}\n", .{ current_token.toString() });

    if (self.check(TokenType.Number)) {
        _ = self.advance();

        const literal = if (current_token.literal) |it| it else "0";
        const int_value = std.fmt.parseInt(i64, literal, 10) catch unreachable;
        const int_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
            .integer = int_value,
        });

        if (self.debug_print) {
            debug.print("Primary done with int literal. ", .{});
            int_lit.*.display(true);
        }
        return int_lit;
    }

    if (self.check(TokenType.NumberFractional)) {
        _ = self.advance();

        const literal = if (current_token.literal) |it| it else "0";
        const double_value = std.fmt.parseFloat(f64, literal) catch unreachable;
        const double_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
            .double = double_value,
        });

        if (self.debug_print) {
            debug.print("Primary done with double literal. ", .{});
            double_lit.*.display(true);
        }
        return double_lit;
    }

    if (self.check(TokenType.String)) {
        _ = self.advance();
        if (current_token.literal) |literal| {
            const string_lit = try ast.createStringLiteral(allocator, literal);

            if (self.debug_print) {
                debug.print("Primary done with string literal. ", .{});
                string_lit.*.display(true);
            }
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

        if (self.debug_print) {
            debug.print("Primary done with true literal. ", .{});
            true_lit.*.display(true);
        }
        return true_lit;
    }
    if (self.check(TokenType.False)) {
        _ = self.advance();
        const false_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
            .boolean = false,
        });

        if (self.debug_print) {
            debug.print("Primary done with false literal. ", .{});
            false_lit.*.display(true);
        }
        return false_lit;
    }
    if (self.check(TokenType.Nil)) {
        _ = self.advance();
        const nil_lit = try ast.createLiteral(allocator, ast.LiteralExpr{
            .nil = true,
        });

        if (self.debug_print) {
            debug.print("Primary done with nil literal. ", .{});
            nil_lit.*.display(true);
        }
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

        if (self.debug_print) {
            debug.print("Primary done with group. ", .{});
            group.*.display(true);
        }
        return group;
    }

    if (self.check(TokenType.Identifier)) {
        _ = self.advance();
        const variable = try ast.createVariableExpr(allocator, current_token);

        if (self.debug_print) {
            debug.print("Primary done with variable identifier. ", .{});
            variable.*.display(true);
        }
        return variable;
    }

    self.debugPrint("Primary done with error on token: {s}.\n", .{ current_token.toString() });
    return error.UnknownPrimaryToken;
}

// NOTE(yemon): The easiest synchronization point is at the 'statement boundaries',
// ie., between ';' and the start of next statement keywords.
fn synchronize(self: *Self) ParserError!void {
    _ = self.advance();
    self.debugPrint("Syncing from token {}...\n", .{ self.peek().token_type });
    while (!self.isEnd()) : (_ = self.advance()) {
        if (self.previous()) |it| {
            if (it.token_type == .Semicolon) {
                self.debugPrint("Already passed the end of statement. Nothing to sync.\n", .{});
                return;
            } else {
                return ParserError.InvalidSyncPosition;
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
    self.debugPrint("Synced with statement boundary.\n", .{});
}

// -----------------------------------------------------------------------------

fn advanceIfMatched(self: *Self, token_type: TokenType) bool {
    if (self.isEnd()) {
        return false;
    }
    if (self.peek().token_type == token_type) {
        _ = self.advance();
        return true;
    }
    return false;
}

fn advanceIfMatchedAny(self: *Self, token_types: []const TokenType) bool {
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

fn check(self: *const Self, token_type: TokenType) bool {
    if (self.isEnd()) {
        return false;
    } else {
        return self.peek().token_type == token_type;
    }
}

fn peek(self: *const Self) Token {
    return self.tokens.*.items[self.current];
}

fn previous(self: *const Self) ?Token {
    if (self.current == 0) {
        return null;
    } else {
        return self.tokens.*.items[self.current-1];
    }
}

fn advance(self: *Self) ?Token {
    if (!self.isEnd()) {
        self.current += 1;
        return self.tokens.*.items[self.current];
    } else {
        return null;
    }
}

// TODO(yemon): The usage of the `ParserResult` feels a bit clunky now...
fn consume(self: *Self, token_type: TokenType, error_message: []const u8) ParserResult {
    const current_token = self.peek();
    if (current_token.token_type == token_type) {
        if (self.advance()) |next_token| {
            self.debugPrint("Consumed '{s}', next token waiting is '{s}'\n", 
                .{ current_token.toString(), next_token.toString() });
            return .{
                .token = current_token,
                .error_message = null,
            };
        } else {
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
        self.debugPrint("Unable to consume token type '{}', returning the current one instead '{s}'.\n", .{ token_type, current_token.toString() });
        return .{
            .token = current_token,
            .error_message = error_message,
        };
    }
}

fn isEnd(self: *const Self) bool {
    if (self.current >= self.tokens.*.items.len) {
        return true;
    }
    return self.peek().token_type == .Eof;
}

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

// TODO(yemon): Reporting functions needs a bit of consolidations. Maybe grouped into its own module?
// NOTE(yemon): Unused!
fn reportPrint(allocator: Allocator, token: Token, index: u32, comptime message: []const u8, args: anytype) void {
    const fmt_message = std.fmt.allocPrint(allocator, message, args) catch "";
    debug.print("Error when parsing token {} at position index {}: {s}\n", .{
        token, index, fmt_message,
    });
}

fn debugPrint(self: *const Self, comptime fmt: []const u8, args: anytype) void {
    if (!self.debug_print) {
        return;
    }
    debug.print(fmt, args);
}

// TODO(yemon): error reporting really needs a proper structure
fn errorPrint(self: *const Self, comptime fmt: []const u8, args: anytype) void {
    _ = self;
    debug.print(fmt, args);
}
