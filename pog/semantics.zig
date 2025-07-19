const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const report = @import("report.zig");
const ast = @import("ast.zig");

const ResolveError = error {
    Failed,
    PoorlyDefinedLexeme,
    UnexpectedStatement,

    OutOfMemory,
};

const LocalScope = std.StringHashMap(bool);

pub const Resolver = struct {
    scopes: std.ArrayList(LocalScope),

    pub fn init(allocator: Allocator) Resolver {
        return .{ 
            .scopes = .init(allocator),
        };
    }

    fn beginScope(self: *Resolver, allocator: Allocator) Allocator.Error!void {
        const new_scope: LocalScope = .init(allocator);
        try self.scopes.append(new_scope);
    }

    fn endScope(self: *Resolver) void {
        const last_scope = self.getLastScope();
        last_scope.*.deinit();
        _ = self.scopes.pop();
    }

    fn isScopesEmpty(self: *const Resolver) bool {
        return self.scopes.items.len == 0;
    }

    fn getLastScope(self: *const Resolver) *LocalScope {
        const len = self.scopes.items.len;
        return &self.scopes.items[len-1];
    }

    fn declare(self: *Resolver, name: []const u8) Allocator.Error!void {
        if (self.isScopesEmpty()) return;

        const last_scope = self.getLastScope();
        try last_scope.*.put(name, false);
    }

    fn define(self: *Resolver, name: []const u8) Allocator.Error!void {
        if (self.isScopesEmpty()) return;

        const last_scope = self.getLastScope();
        try last_scope.*.put(name, true);
    }
};

pub fn resolve(
    resolver: *Resolver, allocator: Allocator, 
    statements: std.ArrayList(*ast.Stmt), 
) ResolveError!void {
    for (statements.items) |stmt| {
        try resolveStmt(resolver, allocator, stmt);
    }
}

fn resolveStmt(
    resolver: *Resolver, allocator: Allocator, 
    stmt: *const ast.Stmt
) ResolveError!void {
    switch (stmt.*) {
        .block_stmt => |block_stmt| {
            try resolver.beginScope(allocator);

            resolve(resolver, allocator, block_stmt.statements) catch |err| 
                switch (err) {
                    ResolveError.OutOfMemory => {},
                    else => resolver.endScope(),
                };
        },

        .variable_declare_stmt => |variable_declare_stmt| {
            try resolver.declare(variable_declare_stmt.name);
            if (variable_declare_stmt.initializer) |initializer| {
                try resolveExpr(resolver, initializer);
            }
            try resolver.define(variable_declare_stmt.name);
        },

        .func_declare_stmt => |func_declare_stmt| {
            if (func_declare_stmt.name.lexeme) |func_name| {
                try resolver.declare(func_name);
                try resolver.define(func_name);
            } else {
                return ResolveError.PoorlyDefinedLexeme;
            }

            try resolveFunction(resolver, allocator, func_declare_stmt);
        },

        .expr_stmt => |expr_stmt| {
            try resolveExpr(resolver, expr_stmt.expr);
        },

        .if_stmt => |if_stmt| {
            try resolveExpr(resolver, if_stmt.condition);
            try resolveStmt(resolver, allocator, if_stmt.then_branch);
            if (if_stmt.else_branch) |else_branch| {
                try resolveStmt(resolver, allocator, else_branch);
            }
        },

        .print_stmt => |print_stmt| {
            try resolveExpr(resolver, print_stmt.expr);
        },

        .return_stmt => |return_stmt| {
            if (return_stmt.expr) |expr| {
                try resolveExpr(resolver, expr);
            }
        },
        
        .loop_stmt => |loop_stmt| {
            try resolveExpr(resolver, loop_stmt.condition);
            try resolveStmt(resolver, allocator, loop_stmt.body);
        },

        .compound_stmt => {
            return ResolveError.UnexpectedStatement;
        },
    }
}

fn resolveExpr(resolver: *Resolver, expr: *const ast.Expr) ResolveError!void {
    switch (expr.*) {
        .variable => |variable| {
            if (variable.lexeme) |lexeme| {
                const last_scope = resolver.getLastScope();
                var is_var_defined = false;
                if (last_scope.*.get(lexeme)) |is_defined| {
                    is_var_defined = is_defined;
                }

                if (!resolver.isScopesEmpty() and !is_var_defined) {
                    // TODO(yemon): proceeding to resolve the 'local' variable
                    // when the error occured, is that correct to do so?
                    report.errorToken(variable, "Cannot read local variable in " ++
                        "its own initializer.");
                }

                resolveLocal(resolver, expr, lexeme);
            } else {
                return ResolveError.PoorlyDefinedLexeme;
            }
        },

        .assign => |assign| {
            try resolveExpr(resolver, assign.value);
            resolveLocal(resolver, expr, assign.identifier);
        },

        .binary => |binary| {
            try resolveExpr(resolver, binary.left);
            try resolveExpr(resolver, binary.right);
        },

        .func_call => |func_call| {
            try resolveExpr(resolver, func_call.callee);
            if (func_call.arguments) |arguments| {
                for (arguments.items) |arg| {
                    try resolveExpr(resolver, arg);
                }
            }
        },

        .grouping => |grouping| {
            try resolveExpr(resolver, grouping.inner);
        },

        .logical => |logical| {
            try resolveExpr(resolver, logical.left);
            try resolveExpr(resolver, logical.right);
        },

        .unary => |unary| {
            try resolveExpr(resolver, unary.right);
        },

        else => {},
    }
    return ResolveError.Failed;
}

fn resolveLocal(
    resolver: *const Resolver, 
    expr: *const ast.Expr, var_name: []const u8
) void {
    var scopes_iter = std.mem.reverseIterator(resolver.scopes.items);
    // .{ .ptr = slice.ptr, .index = slice.len };
    while (scopes_iter.next()) |scope| {
        if (scope.contains(var_name)) {
            debug.print("  > resolver rev. iter idx: {}", .{ scopes_iter.index });
            const num_scopes = resolver.scopes.items.len-1-scopes_iter.index;
            _ = expr;
            _ = num_scopes;
            //interpreter.resolve(expr, num_scopes);
        }
    }
}

fn resolveFunction(
    resolver: *Resolver, allocator: Allocator, 
    func_declare_stmt: ast.FunctionDeclareStmt,
) ResolveError!void {
    try resolver.beginScope(allocator);
    if (func_declare_stmt.params) |params| {
        for (params.items) |param| {
            if (param.lexeme) |lexeme| {
                try resolver.declare(lexeme);
                try resolver.define(lexeme);
            } else {
                return ResolveError.PoorlyDefinedLexeme;
            }
        }
    }

    try resolve(resolver, allocator, func_declare_stmt.body);
    resolver.endScope();
}

