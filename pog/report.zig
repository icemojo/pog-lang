const std = @import("std");
const Allocator = std.mem.Allocator;
const debug = std.debug;
const log = std.log;

const Token = @import("lexer.zig").Token;

// TODO(yemon): Maybe add a column position to the error report as well...
pub fn errorToken(token: Token, message: []const u8) void {
    debug.print("[ERR] Line {} token '{s}': {s}\n", .{ 
        token.line, token.toString(), message 
    });
}

// TODO(yemon): Do I need the token (and also column position) as well?
// The caller should compose the message (whether allocated or buffered)
// prior to calling this.
pub fn runtimeError(comptime message: []const u8) void {
    debug.print("[ERR] {s}\n", .{ message });
}

pub fn runtimeErrorAlloc(allocator: Allocator, comptime message: []const u8, args: anytype) void {
    const msg = std.fmt.allocPrint(allocator, message, args) 
        catch "Runtime error occured";
    defer allocator.free(msg);
    // runtimeError(msg);
    debug.print("[ERR] {s}\n", .{ msg });
}

pub fn arithmeticError(
    allocator: Allocator, comptime message: []const u8, 
    lhs_name: []const u8, rhs_name: []const u8
) void {
    const invalid_msg = "Invalid arithmetic operands. ";
    const operand_msg = "Got '{s}' and '{s}'. ";
    const msg_fmt = invalid_msg ++ message ++ " " ++ operand_msg;

    const msg = std.fmt.allocPrint(allocator, msg_fmt, .{ lhs_name, rhs_name }) 
        catch invalid_msg;
    defer allocator.free(msg);
    // runtimeError(msg);
    debug.print("[ERR] {s}\n", .{ msg });
}

pub fn comparisonError(
    allocator: Allocator, 
    lhs_name: []const u8, rhs_name: []const u8
) void {
    runtimeErrorAlloc(allocator, "Unable to do comparison between '{s}' and '{s}'.", .{
        lhs_name, rhs_name
    });
}

pub fn unaryError(
    allocator: Allocator, operand_name: []const u8
) void {
    runtimeErrorAlloc(allocator, "Unary '-' cannot be used to negate a type of '{s}'.", .{
        operand_name
    });
}
