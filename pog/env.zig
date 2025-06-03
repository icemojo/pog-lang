const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const Value = @import("value.zig").Value;

const StackError = error {
    Undefined,
    AlreadyDefined,
    UnableToDefine,
};

const USIZE_MAX: usize = std.math.maxInt(usize);

// NOTE(yemon):
// It's kinda rare in for language to have both the variables and function declarations
// to NOT live in the same namespace. (e.g., Common Lisp). 
// If the functions need to be "first-class citizens", they both have to exist 
// in the same namespace.

// NOTE(yemon): This specifically does not need to be a generic, but since I'm writing pog
// as part of the Zig learning experience, I'm making this generic just for reference purposes.
pub const Stack = struct {
    const Frame = struct {
        start: usize,
        end: usize,
    };

    const EnvValue = struct {
        name: []const u8,
        value: Value,
    };

    inner: std.ArrayList(EnvValue),
    frames: std.ArrayList(Frame),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        var self: Self = .{
            .inner = undefined,
            .frames = undefined,
        };
        self.inner = std.ArrayList(EnvValue).initCapacity(allocator, 1024) catch unreachable;
        self.frames = std.ArrayList(Frame).initCapacity(allocator, 128) catch unreachable;

        self.frames.append(.{ 
            .start = 0, 
            .end = 0, // USIZE_MAX,
        }) catch unreachable;

        return self;
    }

    pub fn deinit(self: Self) void {
        self.inner.deinit();
        self.frames.deinit();
    }

    pub fn pushFrame(self: *Self) void {
        if (self.frames.items.len == 0) {
            self.frames.append(.{ .start = 0, .end = 0 }) catch unreachable;
        }

        const last_frame = self.frames.items[self.frames.items.len-1];
        const new_frame: Frame = .{ 
            .start = last_frame.end+1,
            .end = last_frame.end+1,
        };
        self.frames.append(new_frame) catch unreachable;
    }

    /// Will just bump the frame index tracker. Does not guarantee the 'inner' collection
    /// will grow together, and thus the new `current_index` may or may not be a valid index.
    pub fn popFrame(self: *Self) void {
        if (self.frames.items.len == 1) {
            return;
        }
        _ = self.frames.pop();
    }

    /// Define a new variable in the *current frame* if it's not already defined.
    pub fn define(self: *Self, name: []const u8, value: Value) !void {
        const last_frame = &self.frames.items[self.frames.items.len-1];

        if (self.inner.items.len == 0 or last_frame.end == 0) {
            self.inner.append(.{ .name = name, .value = value })
                catch return StackError.UnableToDefine;
            return;
        }

        if (self.findFrameIndex(name) != null) {
            return StackError.AlreadyDefined;
        }

        if (last_frame.end < self.inner.items.len and last_frame.start != last_frame.end) {
            self.inner.items[last_frame.*.end] = .{ .name = name, .value = value };
            last_frame.*.end += 1;
        } else {
            self.inner.append(.{ .name = name, .value = value })
                catch return StackError.UnableToDefine;
            last_frame.*.end = self.inner.items.len-1;
        }
        _ = last_frame.*;
    }

    /// Find the variable in the *current frame* and replace its value.
    pub fn assign(self: *Self, name: []const u8, value: Value) !void {
        if (self.findFrameIndex(name)) |frame_index| {
            self.inner.items[frame_index] = .{ .name = name, .value = value };
        } else return StackError.Undefined;
    }

    /// Find the allocated index of the variable with the given name in the *current frame*.
    fn findFrameIndex(self: *const Self, name: []const u8) ?usize {
        if (self.inner.items.len == 0 or self.frames.items.len == 0) {
            return null;
        }

        const last_frame = self.frames.items[self.frames.items.len-1];
        if (last_frame.end == 0) {
            return null;
        }

        const inner_len = self.inner.items.len;
        if (last_frame.start < inner_len and last_frame.end < inner_len) {
            for (self.inner.items[last_frame.start..last_frame.end+1], last_frame.start..last_frame.end+1) |item, idx| {
                if (std.mem.eql(u8, name, item.name)) {
                    return idx;
                }
            }
        } else {
            return null;
        }

        return null;
    }

    /// Find the variable with the given name starting from the *current frame*,
    /// going down the stack in reverse order until it hits the bottom.
    /// Returns the value of the very first name it hits, no matter the frame it's defined.
    pub fn getValue(self: *const Self, name: []const u8) !Value {
        if (self.inner.items.len == 0 or self.frames.items.len == 0) {
            return StackError.Undefined;
        }

        const last_frame = &self.frames.items[self.frames.items.len-1];

        const inner_len = self.inner.items.len;
        var frame_iter = std.mem.reverseIterator(self.frames.items);
        rev_frames: while (frame_iter.next()) |frame| {
            if (frame.start >= inner_len and frame.end >= inner_len) {
                continue :rev_frames;
            } else if (frame.start < inner_len and frame.end >= inner_len) {
                for (self.inner.items[frame.start..]) |item| {
                    if (std.mem.eql(u8, name, item.name)) {
                        return item.value;
                    }
                }
            } else {
                for (self.inner.items[frame.start..frame.end+1]) |item| {
                    if (std.mem.eql(u8, name, item.name)) {
                        return item.value;
                    }
                }
            }
        }
        _ = last_frame;
        return StackError.Undefined;
    }
    
    pub fn display(self: *const Self) void {
        if (self.frames.items.len == 0) {
            debug.print("[Stack: ]\n", .{});
        } else {
            debug.print("[Stack: \n", .{});
            const inner_len = self.inner.items.len;
            print_frames: for (self.frames.items) |frame| {
                debug.print("  >> ", .{});
                if (frame.start >= inner_len and frame.end >= inner_len) {
                    continue :print_frames;
                } else if (frame.start < inner_len and frame.end >= inner_len) {
                    for (self.inner.items[frame.start..]) |item| {
                        debug.print("{s}=", .{ item.name });
                        item.value.display(true);
                        debug.print(" | ", .{});
                    }
                } else {
                    for (self.inner.items[frame.start..frame.end+1]) |item| {
                        debug.print("{s}=", .{ item.name });
                        item.value.display(true);
                        debug.print(" | ", .{});
                    }
                }
                debug.print("\n", .{});
            }
            debug.print("]\n", .{});
        }
    }
};
