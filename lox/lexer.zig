const std = @import("std");
const debug = @import("std").debug;
const ascii = @import("std").ascii;
const Allocator = @import("std").mem.Allocator;

const TokenType = enum {
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

const String = []u8;
const ConstString = []const u8;

const Token = struct {
    token_type: TokenType,
    lexeme: ?ConstString,
    literal: ?ConstString,
    line: u32,
};

pub const Scanner = struct {
    source: ConstString,
    start: u32,
    current: u32,
    line: u32,
    tokens: std.ArrayList(Token),

    pub fn new(allocator: Allocator, source: ConstString) Scanner {
        return .{ .source = source, .start = 0, .current = 0, .line = 1, .tokens = std.ArrayList(Token).init(allocator) };
    }

    pub fn startScanning(self: *Scanner) void {
        while (!self.isEnd()) {
            self.start = self.current;
            self.scanToken() catch |err| {
                debug.print("ERROR: {}\n", .{err});
                continue;
            };
        }
        self.addNonLexemeToken(.Eof);
    }

    fn scanToken(self: *Scanner) !void {
        if (self.advance()) |next_ch| {
            switch (next_ch) {
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
                    self.addNonLexemeToken(.Minus);
                },
                '+' => {
                    self.addNonLexemeToken(.Plus);
                },
                '*' => {
                    self.addNonLexemeToken(.Star);
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
                    try self.tokens.append(.{
                        .token_type = if (self.matchNext('=')) .BangEqual else .Bang,
                        .lexeme = null,
                        .literal = null,
                        .line = self.line,
                    });
                    _ = self.advance();
                },
                '=' => {
                    try self.tokens.append(.{
                        .token_type = if (self.matchNext('=')) .EqualEqual else .Equal,
                        .lexeme = null,
                        .literal = null,
                        .line = self.line,
                    });
                    _ = self.advance();
                },
                '<' => {
                    try self.tokens.append(.{
                        .token_type = if (self.matchNext('=')) .LessEqual else .Equal,
                        .lexeme = null,
                        .literal = null,
                        .line = self.line,
                    });
                    _ = self.advance();
                },
                '>' => {
                    try self.tokens.append(.{
                        .token_type = if (self.matchNext('=')) .GreaterEqual else .Equal,
                        .lexeme = null,
                        .literal = null,
                        .line = self.line,
                    });
                },

                '/' => {
                    if (self.matchNext('/')) {
                        while (self.peek()) |peek_ch| {
                            if (self.isEnd() or isEOL(peek_ch)) {
                                break;
                            }
                            _ = self.advance();
                        }
                    } else {
                        try self.tokens.append(.{
                            .token_type = .Slash,
                            .lexeme = null,
                            .literal = null,
                            .line = self.line,
                        });
                    }
                },

                '"' => {
                    try self.tokenizeString();
                },

                ' ', '\t' => {
                    // ignore general whitespaces
                    return;
                },
                '\n' => {
                    self.line += 1;
                },

                else => {
                    if (isNumeric(next_ch)) {
                        try self.tokenizeNumber();
                    } else if (ascii.isAlphabetic(next_ch)) {
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
        var ch = self.peek();
        while (ch != '"' or self.isEnd()) : (ch = self.peek()) {
            // supporting the multi-line literals
            if (ch == '\n' or ch == '\r') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isEnd()) {
            return error.InvalidComposition;
        }
        _ = self.advance();

        // trim/ignore the start and end double quotes
        const literal = self.source[self.start + 1 .. self.current - 1];
        return self.tokens.append(.{
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

        // detect the fractional point '.'
        var peek_ch = self.peek();
        const next_ch = self.peekNext();
        if (peek_ch != null and next_ch != null) {
            // consume the '.' token
            if (peek_ch == '.' and isNumeric(next_ch.?)) {
                _ = self.advance();
            }

            // consume the rest of the digit characters
            peek_ch = self.peek();
            while (peek_ch != null) : (peek_ch = self.peek()) {
                if (isNumeric(peek_ch.?)) {
                    _ = self.advance();
                } else {
                    break;
                }
            }
        }

        // TODO(yemon): only tokenize the literal number as a string for the time being
        // might be better if I can parse them into their own specific types later on
        // (double, int64, etc)
        const literal = self.source[self.start..self.start];
        return self.tokens.append(.{
            .token_type = .Number,
            .lexeme = null,
            .literal = literal,
            .line = self.line,
        });
    }

    fn tokenizeNumberZeroFractional(self: *Scanner) !void {
        // consume the rest of the numeric characters being '.'
        while (self.peek()) |peek_ch| { // : (peek_ch = self.peek()) {
            if (isNumeric(peek_ch)) {
                _ = self.advance();
            } else {
                break;
            }
        }

        // slicing from the '.' character until the end of numbers
        // NOTE(yemon): The parser need an extra step to make sure that
        // the literal is properly treated (prefixed) with '0.'
        const fractLit = self.source[self.start..self.current];
        return self.tokens.append(.{
            .token_type = .NumberFractional,
            .lexeme = null,
            .literal = fractLit,
            .line = self.line,
        });
    }

    fn tokenizeIdentifier(self: *Scanner) !void {
        var peek_ch = self.peek();
        //while (peek_ch != null and self.isEnd()) : (peek_ch = self.peek()) {
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
                .token_type = keyword,
                .lexeme = null,
                .literal = null,
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
        if (self.isEnd() or self.current + 1 >= self.source.len) {
            return null;
        } else {
            return self.source[self.current];
        }
    }

    fn matchNext(self: *const Scanner, expected: u8) bool {
        if (self.isEnd()) {
            return false;
        }
        return self.source[self.current] == expected;
    }

    fn addNonLexemeToken(self: *Scanner, token_type: TokenType) void {
        self.tokens.append(.{
            .token_type = token_type,
            .lexeme = null,
            .literal = null,
            .line = self.line,
        }) catch return;
    }

    fn isEnd(self: *const Scanner) bool {
        return self.current >= self.source.len;
    }
};

fn checkKeyword(literal: []const u8) ?TokenType {
    const eql = std.mem.eql;

    var l: [32]u8 = undefined;
    const lower = ascii.lowerString(&l, literal);
    if (eql(u8, lower, "class")) {
        return .Class;
    } else if (eql(u8, lower, "fun")) {
        return .Function;
    } else if (eql(u8, lower, "return")) {
        return .Return;
    } else if (eql(u8, lower, "if")) {
        return .If;
    } else if (eql(u8, lower, "true")) {
        return .True;
    } else if (eql(u8, lower, "false")) {
        return .False;
    } else if (eql(u8, lower, "for")) {
        return .For;
    } else if (eql(u8, lower, "while")) {
        return .While;
    } else if (eql(u8, lower, "nil")) {
        return .Nil;
    } else if (eql(u8, lower, "and")) {
        return .And;
    } else if (eql(u8, lower, "or")) {
        return .Or;
    } else if (eql(u8, lower, "this")) {
        return .This;
    } else if (eql(u8, lower, "super")) {
        return .Super;
    } else if (eql(u8, lower, "print")) {
        return .Print;
    } else if (eql(u8, lower, "var")) {
        return .Var;
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
