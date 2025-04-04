const std = @import("std");
const debug = @import("std").debug;
const log = @import("std").log;

const Token = @import("lexer.zig").Token;

// TODO(yemon): Maybe add a column position to the error report as well...
pub fn errorToken(token: Token, message: []const u8) void {
    debug.print("[ERR] Line {} token '{s}': {s}\n", .{ 
        token.line, token.toString(), message 
    });
}
