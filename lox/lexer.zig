const std       = @import("std");
const debug     = @import("std").debug;
const ascii     = @import("std").ascii;
const Allocator = @import("std").mem.Allocator;

pub const TokenType = enum {
    // single-character token
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals
    Identifier,
    String,
    Number,
    NumberFractional,

    // keywords
    And,
    Or,
    Var,
    Class,
    Function,
    Return,
    If,
    Else,
    For,
    While,
    True,
    False,
    Nil,
    Super,
    This,

    // built-in functions
    Print,

    // controls
    Invalid,
    Eol,
    Eof,
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: ?[]const u8,
    literal: ?[]const u8,
    line: u32,

    pub fn isTerminator(self: *const Token) bool {
        return switch (self.token_type) {
            .Eof => true,
            .Eol => true,
            .Semicolon => true,
            else => false,
        };
    }

    pub fn display(self: *const Token) void {
        debug.print("[{}] Token ({}", .{ self.line, self.token_type });
        if (self.lexeme) |lexeme| {
            debug.print(", {s}", .{ lexeme });
        }
        if (self.literal) |literal| {
            debug.print(", \"{s}\"", .{ literal });
        }
        debug.print(")\n", .{});
    }

    pub fn toString(self: *const Token) []const u8 {
        return if (self.lexeme) |lexeme| blk: {
            if (!std.mem.eql(u8, lexeme, "")) {
                break :blk lexeme;
            } else {
                break :blk @tagName(self.token_type);
            }
        } else @tagName(self.token_type);
    }
};

pub const Scanner = struct {
    source: []const u8,
    start: u32,
    current: u32,
    line: u32,
    tokens: std.ArrayList(Token),
    verbose: bool,

    pub fn init(allocator: Allocator, source: []const u8, verbose: bool) Scanner {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .tokens = std.ArrayList(Token).init(allocator),
            .verbose = verbose,
        };
    }

    pub fn startScanning(self: *Scanner, is_repl: bool) void {
        while (!self.isEnd()) {
            self.scanToken() catch |err| {
                // NOTE(yemon): should I not use the `reportError` defined in main.zig?
                debugPrint(self, "Error at [{}]: {}\n", .{ self.current, err });
                continue;
            };
        }

        const last_token = self.tokens.items[self.tokens.items.len-1];
        if (is_repl and last_token.token_type != .Semicolon) {
            self.addNonLexemeToken(.Semicolon);
        }
        self.addNonLexemeToken(.Eof);
    }

    fn scanToken(self: *Scanner) !void {
        self.start = self.current;
        // debugPrint(self, ">> {}", .{ self.current });
        if (self.advance()) |current_ch| {
            // debugPrint(self, > '{c}'\n", .{ current_ch });
            switch (current_ch) {
                '(' => {
                    self.addNonLexemeToken(.LeftParen);
                },
                ')' => {
                    self.addNonLexemeToken(.RightParen);
                },
                '{' => {
                    self.addNonLexemeToken(.LeftBrace);
                },
                '}' => {
                    self.addNonLexemeToken(.RightBrace);
                },
                ',' => {
                    self.addNonLexemeToken(.Comma);
                },
                '-' => {
                    self.addLexemeToken(.Minus, "-");
                },
                '+' => {
                    self.addLexemeToken(.Plus, "+");
                },
                '*' => {
                    self.addLexemeToken(.Star, "*");
                },
                ';' => {
                    self.addNonLexemeToken(.Semicolon);
                },

                '.' => {
                    if (self.peek()) |peek_ch| {
                        if (isNumeric(peek_ch)) {
                            try self.tokenizeNumberZeroFractional();
                        } else {
                            self.addNonLexemeToken(.Dot);
                        }
                    }
                },

                '!' => {
                    var token_type: TokenType = undefined;
                    var lexeme: []const u8 = undefined;
                    if (self.advanceIfMatched('=')) {
                        token_type = .BangEqual;
                        lexeme = "!=";
                    } else {
                        token_type = .Bang;
                        lexeme = "!";
                    }
                    self.addLexemeToken(token_type, lexeme);
                },
                '=' => {
                    var token_type: TokenType = undefined;
                    var lexeme: []const u8 = undefined;
                    if (self.advanceIfMatched('=')) {
                        token_type = .EqualEqual;
                        lexeme = "==";
                    } else {
                        token_type = .Equal;
                        lexeme = "=";
                    }
                    self.addLexemeToken(token_type, lexeme);
                },
                '<' => {
                    var token_type: TokenType = undefined;
                    var lexeme: []const u8 = undefined;
                    if (self.advanceIfMatched('=')) {
                        token_type = .LessEqual;
                        lexeme = "<=";
                    } else {
                        token_type = .Less;
                        lexeme = "<";
                    }
                    self.addLexemeToken(token_type, lexeme);
                },
                '>' => {
                    var token_type: TokenType = undefined;
                    var lexeme: []const u8 = undefined;
                    if (self.advanceIfMatched('=')) {
                        token_type = .GreaterEqual;
                        lexeme = ">=";
                    } else {
                        token_type = .Greater;
                        lexeme = ">";
                    }
                    self.addLexemeToken(token_type, lexeme);
                },

                '/' => {
                    if (self.advanceIfMatched('/')) {
                        while (self.peek()) |peek_ch| {
                            if (isEOL(peek_ch) or self.isEnd()) {
                                break;
                            }
                            _ = self.advance();
                        }
                    } else {
                        self.addLexemeToken(.Slash, "/");
                    }
                },

                '"' => {
                    try self.tokenizeString();
                },

                ' ', '\t' => {
                    // ignore general whitespaces
                    return;
                },
                '\n', '\r' => {
                    self.line += 1;
                },

                else => {
                    if (isNumeric(current_ch)) {
                        try self.tokenizeNumber();
                    } else if (ascii.isAlphabetic(current_ch)) {
                        try self.tokenizeIdentifier();
                    } else {
                        // TODO(yemon): is this error report a correct edge case?
                        return error.UnknownToken;
                    }
                },
            }
        } else {
            return error.UnknownToken;
        }
    }

    fn tokenizeString(self: *Scanner) !void {
        // NOTE(yemon): `peek()` can return null if `isEnd()` is true,
        // and doing the `isEnd()` check again, while ignoring/unwrapping
        // peek_ch below is... kinda contradictory
        var peek_ch = self.peek();
        while (peek_ch != '"' or self.isEnd()) : (peek_ch = self.peek()) {
            // supporting the multi-line string literals
            if (isEOL(peek_ch.?)) {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isEnd()) {
            return error.UnterminatedString;
        }
        // the closing '"'
        _ = self.advance();

        // trim/ignore the start and end double quotes
        const literal = self.source[self.start+1..self.current-1];
        try self.tokens.append(.{
            .token_type = .String,
            .lexeme = null,
            .literal = literal,
            .line = self.line,
        });
    }

    fn tokenizeNumber(self: *Scanner) !void {
        // consume all numeric characters
        while (self.peek()) |peek_ch| {
            if (isNumeric(peek_ch)) {
                _ = self.advance();
            } else {
                break;
            }
        }

        var token_type = TokenType.Number;

        // detect the fractional point '.', skips it...
        var peek_ch = self.peek();
        const next_ch = self.peekNext();
        if (peek_ch != null and next_ch != null) blk: {
            if (peek_ch == '.' and isNumeric(next_ch.?)) {
                _ = self.advance();
                token_type = .NumberFractional;
            } else {
                break :blk;
            }

            // ...and consume the rest of the digit characters
            peek_ch = self.peek();
            while (peek_ch != null) : (peek_ch = self.peek()) {
                if (isNumeric(peek_ch.?)) {
                    _ = self.advance();
                } else {
                    break;
                }
            }
            // NOTE(yemon): I don't quite get why below constructs won't work.
            // Maybe I'm trying to shoehorn the 'while' statements too much as
            // the traditional 3-segments 'for' loops. I mean, the first, I get.
            // It'll be hard to identify which value to capture from the binary 
            // statement of while-condition. But I do feel like the
            // second form with pointer captures should work naturally though.
            // while (self.peek() and isNumeric(char.*)) |*char| : (char.* = self.peek()) { ... }
            // while (self.peek()) |*char| : (char.* = self.peek()) { ... }
        }

        // TODO(yemon): only tokenize the literal number as a string for the time being
        // might be better if I can parse them into their own specific types later on
        // (double, int64, etc)
        const literal = self.source[self.start..self.current];
        return self.tokens.append(.{
            .token_type = token_type,
            .lexeme = null,
            .literal = literal,
            .line = self.line,
        });
    }

    fn tokenizeNumberZeroFractional(self: *Scanner) !void {
        // consume the rest of the numeric characters being '.'
        while (self.peek()) |peek_ch| {
            if (isNumeric(peek_ch)) {
                _ = self.advance();
            } else {
                break;
            }
        }

        // slicing from the '.' character until the end of numbers
        // NOTE(yemon): The parser need an extra step to make sure that
        // the literal is properly treated (prefixed) with '0.'
        const literal = self.source[self.start..self.current];
        return self.tokens.append(.{
            .token_type = .NumberFractional,
            .lexeme = null,
            .literal = literal,
            .line = self.line,
        });
    }

    fn tokenizeIdentifier(self: *Scanner) !void {
        var peek_ch = self.peek();
        while (peek_ch != null or !self.isEnd()) : (peek_ch = self.peek()) {
            if (ascii.isAlphanumeric(peek_ch.?)) {
                _ = self.advance();
            } else {
                break;
            }
        }

        const literal = self.source[self.start..self.current];
        if (checkKeyword(literal)) |keyword| {
            return self.tokens.append(.{
                .token_type = keyword.token_type,
                .lexeme = null,
                .literal = keyword.lexeme,
                .line = self.line,
            });
        } else {
            return self.tokens.append(.{
                .token_type = .Identifier,
                .lexeme = literal,
                .literal = null,
                .line = self.line,
            });
        }
    }

    fn advance(self: *Scanner) ?u8 {
        if (self.isEnd()) {
            return null;
        }
        const ch = self.source[self.current];
        self.current += 1;
        return ch;
    }

    fn peek(self: *const Scanner) ?u8 {
        return if (self.isEnd()) {
            return null;
        } else {
            return self.source[self.current];
        };
    }

    fn peekNext(self: *const Scanner) ?u8 {
        if (self.isEnd() or self.current+1 >= self.source.len) {
            return null;
        } else {
            return self.source[self.current+1];
        }
    }

    fn advanceIfMatched(self: *Scanner, expected: u8) bool {
        if (self.isEnd()) {
            return false;
        }
        if (self.source[self.current] != expected) {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn addNonLexemeToken(self: *Scanner, token_type: TokenType) void {
        self.tokens.append(.{
            .token_type = token_type,
            .lexeme = null,
            .literal = null,
            .line = self.line,
        }) catch return;
    }

    fn addLexemeToken(self: *Scanner, token_type: TokenType, lexeme: []const u8) void {
        self.tokens.append(.{
            .token_type = token_type,
            .lexeme = lexeme,
            .literal = null,
            .line = self.line,
        }) catch return;
    }

    fn isEnd(self: *const Scanner) bool {
        return self.current >= self.source.len;
    }
};

fn debugPrint(self: *const Scanner, comptime fmt: []const u8, args: anytype) void {
    if (!self.verbose) {
        return;
    }
    debug.print(fmt, args);
}

const Keyword = struct {
    token_type: TokenType,
    lexeme: []const u8,
};

pub fn checkKeyword(literal: []const u8) ?Keyword {
    const eql = std.mem.eql;

    var l: [32]u8 = undefined;
    const lower = ascii.lowerString(&l, literal);
    if (eql(u8, lower, "class")) {
        return .{ .token_type = .Class, .lexeme = "class" };
    } else if (eql(u8, lower, "fun")) {
        return .{ .token_type = .Function, .lexeme = "fun" };
    } else if (eql(u8, lower, "return")) {
        return .{ .token_type = .Return, .lexeme = "return" };
    } else if (eql(u8, lower, "if")) {
        return .{ .token_type = .If, .lexeme = "if" };
    } else if (eql(u8, lower, "else")) {
        return .{ .token_type = .Else, .lexeme = "else" };
    } else if (eql(u8, lower, "true")) {
        return .{ .token_type = .True, .lexeme = "true" };
    } else if (eql(u8, lower, "false")) {
        return .{ .token_type = .False, .lexeme = "false" };
    } else if (eql(u8, lower, "for")) {
        return .{ .token_type = .For, .lexeme = "for" };
    } else if (eql(u8, lower, "while")) {
        return .{ .token_type = .While, .lexeme = "while" };
    } else if (eql(u8, lower, "nil")) {
        return .{ .token_type = .Nil, .lexeme = "nil" };
    } else if (eql(u8, lower, "and")) {
        return .{ .token_type = .And, .lexeme = "and" };
    } else if (eql(u8, lower, "or")) {
        return .{ .token_type = .Or, .lexeme = "or" };
    } else if (eql(u8, lower, "this")) {
        return .{ .token_type = .This, .lexeme = "this" };
    } else if (eql(u8, lower, "super")) {
        return .{ .token_type = .Super, .lexeme = "super" };
    } else if (eql(u8, lower, "print")) {
        return .{ .token_type = .Print, .lexeme = "print" };
    } else if (eql(u8, lower, "var")) {
        return .{ .token_type = .Var, .lexeme = "var" };
    } else {
        return null;
    }
}

fn isEOL(char: u8) bool {
    return char == '\n' or char == '\r';
}

fn isNumeric(char: u8) bool {
    return switch (char) {
        '0'...'9' => true,
        else => false,
    };
}
