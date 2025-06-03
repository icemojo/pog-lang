const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const StackError = error {
    UndefinedValue,
    AlreadyDefinedValue,
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
pub fn Stack(comptime V: type) type {
    const Frame = struct {
        start: usize,
        end: ?usize,
    };

    const EnvValue = struct {
        name: []const u8,
        value: V,
    };

    return struct {
        inner: std.ArrayList(EnvValue),
        last_frame_index: usize,
        current_index: usize,
        frames: std.ArrayList(Frame),

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            var self: Self = .{
                .inner = undefined,
                .last_frame_index = USIZE_MAX,
                .current_index = 0,
                .frames = undefined,
            };
            self.inner = std.ArrayList(EnvValue).initCapacity(allocator, 1024) catch unreachable;
            self.frames = std.ArrayList(Frame).initCapacity(allocator, 128) catch unreachable;
            self.frames.append(.{ .start = 0, .end = null }) catch unreachable;
            return self;
        }

        pub fn deinit(self: Self) void {
            self.inner.deinit();
            self.frames.deinit();
        }

        /// Will just bump the frame index tracker. Does not guarantee the 'inner' collection
        /// will grow together, and thus the new `current_index` may or may not be a valid index.
        pub fn pushFrame(self: *Self) void {
            // self.frames.items[self.frames.items.len-1].end = if (self.inner.items.len >= 1) 
            //     self.current_index-1 else 0;
            // self.last_frame_index = self.current_index-1;

            if (self.frames.items.len > 0) {
                const current_frame = &self.frames.items[self.frames.items.len-1];
                if (self.current_index > current_frame.start) {
                    current_frame.end = self.current_index-1;
                } else {
                    current_frame.end = null;
                }
            }

            if (self.current_index == 0) {
                self.last_frame_index = USIZE_MAX;
            } else {
                self.last_frame_index = self.current_index-1;
            }

            self.frames.append(.{
                .start = self.current_index,
                .end = null,
            }) catch unreachable;
        }

        pub fn popFrame(self: *Self) void {
            // self.current_index = self.last_frame_index+1;
            // _ = self.frames.pop();

            if (self.frames.items.len == 0) {
                return;
            }

            const current_frame = &self.frames.items[self.frames.items.len-1];
            self.current_index = current_frame.start;
            _ = self.frames.pop();

            // NOTE(yemon): `last_frame_index` will be reset back to `USIZE_MAX`
            // when the *current frame* has already reached the stack-bottom frame.
            if (self.frames.items.len > 0) {
                if (self.current_index == 0) {
                    self.last_frame_index = USIZE_MAX;
                } else {
                    self.last_frame_index = self.current_index-1;
                }
            }
        }

        /// Define a new variable in the *current frame* if it's not already defined.
        pub fn define(self: *Self, name: []const u8, value: V) !void {
            if (self.findFrameIndex(name) != null) {
                return StackError.AlreadyDefinedValue;
            }

            if (self.current_index < self.inner.items.len) {
                self.inner.items[self.current_index] = .{ .name = name, .value = value };
                self.current_index += 1;
            } else {
                self.inner.append(.{ .name = name, .value = value }) 
                    catch return StackError.UnableToDefine;
                self.current_index = self.inner.items.len;
            }
        }

        /// Find the variable in the *current frame* and update its value.
        pub fn assign(self: *Self, name: []const u8, value: V) !void {
            if (self.findFrameIndex(name)) |frame_index| {
                self.inner.items[frame_index] = .{ .name = name, .value = value };
            } else return StackError.UndefinedValue;
        }

        /// Find the allocated index of the variable with the given name in the *current frame*.
        fn findFrameIndex(self: *const Self, name: []const u8) ?usize {
            if (self.inner.items.len == 0 or self.frames.items.len == 0) {
                return null;
            }

            const current_frame_start = self.last_frame_index+%1;
            if (current_frame_start >= self.current_index) {
                return null;
            }

            for (self.inner.items[current_frame_start..self.current_index], current_frame_start..) |item, idx| {
                if (std.mem.eql(u8, name, item.name)) {
                    return current_frame_start + idx;
                }
            }

            // const start = self.last_frame_index+1;
            // const end   = self.current_index;
            // for (self.inner.items[start..end], start..end) |item, idx| {
            //     if (std.mem.eql(u8, name, item.name)) {
            //         return idx;
            //     }
            // }
            return null;
        }

        /// Find the variable with the given name starting from the *current frame*,
        /// going down the stack in reverse order until it hits the bottom.
        /// Returns the value of the very first name it hits, no matter the frame it's defined.
        pub fn getValue(self: *const Self, name: []const u8) !V {
            var frame_iter = std.mem.reverseIterator(self.frames.items);
            while (frame_iter.next()) |frame| {
                if (frame.end) |end| {
                    for (self.inner.items[frame.start..end+1]) |item| {
                        if (std.mem.eql(u8, name, item.name)) {
                            return item.value;
                        }
                    }
                } else {
                    for (self.inner.items[frame.start..self.current_index]) |item| {
                        if (std.mem.eql(u8, name, item.name)) {
                            return item.value;
                        }
                    }
                }
            }
            return StackError.UndefinedValue;
        }
        
        pub fn display(self: *const Self) void {
            debug.print("[Stack: \n", .{});
            for (self.frames.items) |frame| {
                debug.print("  >> ", .{});
                if (frame.end) |end| {
                    for (self.inner.items[frame.start..end+1]) |item| {
                        debug.print("{s}=", .{ item.name });
                        item.value.display(true);
                        debug.print(" | ", .{});
                    }
                } else {
                    for (self.inner.items[frame.start..]) |item| {
                        debug.print("{s}=", .{ item.name });
                        item.value.display(true);
                        debug.print(" | ", .{});
                    }
                }
                debug.print("\n", .{});
            }
            debug.print("]\n", .{});
        }
    };
}
