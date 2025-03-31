const std       = @import("std");
const fmt       = @import("std").fmt;
const debug     = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const lexer     = @import("lexer.zig");
const Token     = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const ast       = @import("ast.zig");
const report    = @import("report.zig");

// NOTE(yemon): Inferred error sets are not compatible with
// recursive function calls. So, Zig needs an explicit error 
// definition with all possible errors from the call stack merged.
const ParserError = error {
    UnknownPrimaryToken,
    InvalidStringComposition,
    InvalidFunctionComposition,
    InvalidBranchComposition,
    InvalidLoopComposition,
    InvalidBlockComposition,
    InvalidExpressionStatement,
    InvalidSyncPosition,
    InvalidSyncToken,
    InvalidIdentifierDeclaration,
    InvalidAssignmentTarget,
    UnterminatedStatement,
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
            // TODO(yemon): might wanna do the synchronization only based on the reported
            // error, for instance; only for the block statements like 'if' and 'for'
            self.debugPrint("Parsing a statement failed with '{}' error. " ++ 
                "Need to synchronize until end of statement.\n", .{ err, });
            self.synchronize() catch |sync_err| {
                self.debugPrint("Synchronization failed with error '{}'.\n", .{ sync_err });
                break :parsing;
            };
            continue :parsing;
        }
    }
    self.debugPrint("End of all the statements.\n", .{});

    if (self.debug_ast) {
        debug.print("------------------------------------------------------------\n", .{});
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
    var identifier: Token = undefined;

    if (self.consume(.Identifier)) |it| {
        identifier = it;
    } else {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect a valid identifier declaration.");
        return ParserError.InvalidIdentifierDeclaration;
    }

    var var_stmt: *ast.Stmt = undefined;
    self.debugPrint("Identifier name is '{s}'\n", .{ identifier.lexeme.? });

    var initializer: ?*ast.Expr = null;
    if (self.advanceIfMatched(.Equal)) {
        initializer = try self.expression(allocator);
    }
    var_stmt = ast.createVariableStmt(allocator, identifier, initializer) 
        catch |err| switch (err) {
            ast.AllocError.OutOfMemory => {
                return ParserError.OutOfMemory;
            },
        };

    // TODO(yemon): this would cause REPL to report an error if the statement 
    // being entered didn't get terminated with ';'
    // Right now, the temporary solution is to add manually add ';' at the end of the
    // tokenizer scanning chain. Hacky, don't like it at all!
    if (self.consume(.Semicolon) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), 
            "Expect an EOL terminator ';' after an identifier declaration expression."
        );
        return ParserError.UnterminatedStatement;
    }

    return var_stmt;
}

fn functionDeclareStmt(
    self: *Self, 
    allocator: Allocator, 
    comptime kind: []const u8
) ParserError!*ast.Stmt {
    self.debugPrint("Seems like a " ++ kind ++ " declare block...\n", .{});

    var identifier: Token = undefined;

    if (self.consume(.Identifier)) |it| {
        identifier = it;
    } else {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect " ++ kind ++ " name.");
        return ParserError.InvalidFunctionComposition;
    }

    self.debugPrint("Identifier name is {s}.\n", .{ identifier.toString() });

    if (self.consume(.LeftParen) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect '(' after the " ++ kind ++ " name.");
        return ParserError.InvalidFunctionComposition;
    }

    var params = std.ArrayList(Token).init(allocator);
    while (!self.check(.RightParen) and params.items.len <= 255) {
        if (self.consume(.Identifier)) |it| {
            try params.append(it);
        } else {
            self.has_error = true;
            report.errorToken(self.peek(), "Expect an identifier as a " ++ kind ++ " parameter.");
            return ParserError.InvalidFunctionComposition;
        }
        _ = self.advanceIfMatched(.Comma);
    }

    if (self.consume(.RightParen) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect ')' after the end of " ++ kind ++ " parameters.");
        return ParserError.InvalidFunctionComposition;
    }

    self.debugPrint("Parsed {} parameters.\n", .{ params.items.len });
    if (self.debug_print) {
        for (params.items) |param| {
            self.debugPrint(" -> {s}\n", .{ param.toString() });
        }
    }

    if (self.consume(.LeftBrace) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect '{' after the " ++ kind ++ " body.");
        return ParserError.InvalidFunctionComposition;
    }

    self.debugPrint("Parsing the function body...\n", .{});
    const body = try self.blockStmts(allocator);
    self.debugPrint("Function body parsed.\n", .{});

    // TODO(yemon): the 'name' identification is wrong, the last argument name is
    // being marked as the function name here...
    const stmt = try allocator.create(ast.Stmt);
    stmt.* = ast.Stmt{
        .func_declare_stmt = ast.FunctionDeclareStmt{
            .name = identifier,
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

    // TODO(yemon): This kinda has the same problem as the variable declare statement
    // It doesn't really report the error properly in the REPL
    if (self.consume(.Semicolon) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect ';' after the print statement expression.");
    }

    const stmt = try ast.createPrintStmt(allocator, expr);
    return stmt;
}

fn ifStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like an if statement block...\n", .{});
    if (self.consume(.LeftParen) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect '(' before the 'if' condition.");
        return ParserError.InvalidBranchComposition;
    }

    const condition = try self.expression(allocator);

    if (self.consume(.RightParen) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect ')' after the 'if' condition.");
        return ParserError.InvalidBranchComposition;
    }

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
    if (self.consume(.LeftParen) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect '(' after the 'while' condition.");
        return ParserError.InvalidLoopComposition;
    }

    const condition = try self.expression(allocator);

    if (self.consume(.RightParen) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect ')' after the 'while' condition.");
        return ParserError.InvalidLoopComposition;
    }

    const body = try self.statement(allocator);

    const while_stmt = try ast.createWhileStmt(allocator, condition, body);
    return while_stmt;
}

fn forStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like a for statement block...\n", .{});
    if  (self.consume(.LeftParen) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect '(' after 'for'.");
        return ParserError.InvalidLoopComposition;
    }

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
    if (self.consume(.Semicolon) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect ';' after the 'for' loop condition.");
        return ParserError.InvalidLoopComposition;
    }

    var increment: ?*ast.Expr = null;
    if (!self.check(.RightParen)) {
        increment = try self.expression(allocator);
    }
    if (self.consume(.RightParen) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect ')' after the 'for' clauses.");
        return ParserError.InvalidLoopComposition;
    }

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
    
    if (self.consume(.RightBrace) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect '}' at the end of a block.");
        return ParserError.InvalidBlockComposition;
    }

    return statements;
}

fn expressionStmt(self: *Self, allocator: Allocator) ParserError!*ast.Stmt {
    self.debugPrint("Seems like an expression statement...\n", .{});
    const expr = try self.expression(allocator);

    if (self.consume(.Semicolon) == null) {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect ';' after the expression.");
        return ParserError.InvalidExpressionStatement;
    }

    const stmt = try ast.createExprStmt(allocator, expr);
    return stmt;
}

fn expression(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    if (self.assignment(allocator)) |expr| {
        if (self.debug_print) {
            self.debugPrint("Expression done with assignment. ", .{});
            expr.*.display(true);
        }

        return expr;
    } else |err| {
        self.has_error = true;
        self.debugPrint("Expression done with error {}.\n", .{ err });
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
                const assign_expr = ast.createAssignmentExpr(allocator, variable, value) 
                    catch |err| switch (err) {
                        ast.AllocError.OutOfMemory => {
                            return ParserError.OutOfMemory;
                        },
                    };

                if (self.debug_print) {
                    self.debugPrint("Assignment done with ", .{});
                    assign_expr.*.display(true);
                }
                
                return assign_expr;
            },
            else => {
                self.has_error = true;
                return ParserError.InvalidAssignmentTarget;
            },
        }

        const equals: Token = self.previous().?;
        report.errorToken(equals, "Invalid assignment target.");
    }

    self.debugPrint("Assignment done with logical or.\n", .{});
    return expr;
}

fn logicOr(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.logicAnd(allocator);

    if (self.advanceIfMatched(.Or)) {
        const optr = self.previous().?;
        const right = try self.logicAnd(allocator);
        expr = try ast.createLogicalExpr(allocator, expr, optr, right);

        if (self.debug_print) {
            debug.print("Logical 'or' done with another 'and' condition. ", .{});
        }
    }

    if (self.debug_print) {
        debug.print("Logical 'or' done ", .{});
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
            debug.print("Logicial 'and' done with equality. ", .{});
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
            debug.print("Unary done with proper unary prefix. ", .{});
            expr.*.display(true);
        }
        return expr;
    }

    const func = try self.funcCall(allocator);

    if (self.debug_print) {
        debug.print("Unary done with function call.\n", .{});
    }
    return func;
}

fn funcCall(self: *Self, allocator: Allocator) ParserError!*ast.Expr {
    var expr = try self.primary(allocator);

    while (true) {
        if (self.advanceIfMatched(.LeftParen)) {
            // NOTE(yemon): Would the original `expr` be leaking, if there were
            // multiple call chains and `expr` is replaced on each call iteration
            expr = try self.finishCall(allocator, expr);

            if (self.debug_print) {
                debug.print("Function call done on sub-expression:\n", .{});
                expr.*.display(true);
            }
        } else {
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
    self.debugPrint("Parsing the function arguments...\n", .{});
    var arguments = std.ArrayList(*ast.Expr).init(allocator);

    if (!self.check(.RightParen)) {
        var arg = try self.expression(allocator);
        try arguments.append(arg);
        // NOTE(yemon): two identifier tokens NOT separated by 'comma' could be an issue
        while (self.advanceIfMatched(.Comma)) {
            if (arguments.items.len > 255) {
                // NOTE(yemon): report error instead of throwing, since throwing will 
                // kick into the panic mode and try to synchronize.
                report.errorToken(self.peek(), "Functions cannot have more than 255 arguments.");
            }
            arg = try self.expression(allocator);
            try arguments.append(arg);
        }
    }
    self.debugPrint("Parsed {} arguments.\n", .{ arguments.items.len });

    // NOTE(yemon): Closing paren token is marked just for error reporting if needed
    var closing_paren: Token =  undefined;
    if (self.consume(.RightParen)) |it| {
        closing_paren = it;
    } else {
        self.has_error = true;
        report.errorToken(self.peek(), "Expect ')' after function arguments.");
    }

    self.debugPrint("Function call done.\n", .{});
    return ast.createFunctionCallExpr(allocator, callee, closing_paren, arguments);
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
        if (self.consume(.RightParen) == null) {
            self.has_error = true;
            report.errorToken(current_token, "Expect ')' after the grouping expression.");
            return ParserError.UnterminatedStatement;
        }

        if (self.debug_print) {
            self.debugPrint("Primary done with group. ", .{});
            group.*.display(true);
        }
        return group;
    }

    if (self.check(TokenType.Identifier)) {
        _ = self.advance();
        const variable = try ast.createVariableExpr(allocator, current_token);

        if (self.debug_print) {
            self.debugPrint("Primary done with variable identifier. ", .{});
            variable.*.display(true);
        }
        return variable;
    }

    self.debugPrint("Primary done with error on token: {s}.\n", .{ current_token.toString() });
    return error.UnknownPrimaryToken;
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

fn advance(self: *Self) ?Token {
    if (!self.isEnd()) {
        self.current += 1;
        return self.tokens.*.items[self.current];
    } else {
        return null;
    }
}

fn check(self: *const Self, token_type: TokenType) bool {
    if (self.isEnd()) {
        return false;
    } else {
        return self.peek().token_type == token_type;
    }
}

// `peek()` returns the token that is being currently looked at.
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

// NOTE(yemon): The easiest synchronization point is at the 'statement boundaries',
// ie., between ';' and the start of next statement keywords.
fn synchronize(self: *Self) ParserError!void {
    self.debugPrint("Syncing from token {}...\n", .{ self.peek().token_type });
    _ = self.advance();
    while (!self.isEnd()) : (_ = self.advance()) {
        const current_token = self.peek();
        self.debugPrint("-> Token {s}\n", .{ current_token.toString() });
        // NOTE(yemon): `previous()` shouldn't be null, since `advance()` was successful
        const previous_token = self.previous().?;
        if (previous_token.token_type == .Semicolon) {
            self.debugPrint("Already passed the end of statement. Nothing to sync.\n", .{});
            return;
        }

        switch (current_token.token_type) {
            .Class => return,
            .Function => return,
            .Var => return,
            .If => return,
            .For => return,
            .While => return,
            .Print => return,
            .Return => return,
            else => continue,
        }
    }
    self.debugPrint("Successfully synced with statement boundary.\n", .{});
}

const MatchedToken = Token;

fn consume(self: *Self, token_type: TokenType) ?MatchedToken {
    const current_token = self.peek();
    if (current_token.token_type == token_type) {
        self.debugPrint("Consume matching token '{s}'.\n", .{ current_token.toString() });
        _ = self.advance();
        return current_token;
    } else {
        return null;
    }
}

fn isEnd(self: *const Self) bool {
    if (self.current >= self.tokens.*.items.len) {
        return true;
    }
    return self.peek().token_type == .Eof;
}

// -----------------------------------------------------------------------------

fn debugPrint(self: *const Self, comptime fmt_message: []const u8, args: anytype) void {
    if (!self.debug_print) {
        return;
    }
    debug.print(fmt_message, args);
}
