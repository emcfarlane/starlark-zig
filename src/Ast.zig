//! Abstract Syntax Tree for Starlark source code.

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: TokenList.Slice,
/// The root AST node is assumed to be index 0. Since there can be no
/// references to the root node, this means 0 is available to indicate null.
nodes: NodeList.Slice,
extra_data: []Node.Index,
errors: []const Error,

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const syntax = @import("./syntax.zig");
const Token = syntax.Token;
const Ast = @This();

pub const TokenIndex = u32;
pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

//pub const Location = struct {
//    line: usize,
//    column: usize,
//    line_start: usize,
//    line_end: usize,
//};

pub fn deinit(tree: *Ast, gpa: mem.Allocator) void {
    tree.tokens.deinit(gpa);
    tree.nodes.deinit(gpa);
    gpa.free(tree.extra_data);
    gpa.free(tree.errors);
    tree.* = undefined;
}

// rootDecls returns the list of root nodes.
pub fn rootDecls(tree: Ast) []const Node.Index {
    // Root is always index 0.
    const nodes_data = tree.nodes.items(.data);
    return tree.extra_data[nodes_data[0].lhs..nodes_data[0].rhs];
}

//pub fn tokenLocation(self: Ast, start_offset: ByteOffset, token_index: TokenIndex) Location {
//    var loc = Location{
//        .line = 0,
//        .column = 0,
//        .line_start = start_offset,
//        .line_end = self.source.len,
//    };
//    const token_start = self.tokens.items(.start)[token_index];
//    for (self.source[start_offset..]) |c, i| {
//        if (i + start_offset == token_start) {
//            loc.line_end = i + start_offset;
//            while (loc.line_end < self.source.len and self.source[loc.line_end] != '\n') {
//                loc.line_end += 1;
//            }
//            return loc;
//        }
//        if (c == '\n') {
//            loc.line += 1;
//            loc.column = 0;
//            loc.line_start = i + 1;
//        } else {
//            loc.column += 1;
//        }
//    }
//    return loc;
//}

pub const Error = struct {
    tag: Tag,
    is_note: bool = false,
    /// True if `token` points to the token before the token causing an issue.
    token_is_prev: bool = false,
    token: TokenIndex,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
    } = .{ .none = {} },

    pub const Tag = enum {

        /// `expected_tag` is populated.
        expected_token,
        expected_primary_expr,
        unexpected_trailing_comma,
        unexpected_associate,
    };
};

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Index = u32;

    comptime {
        // Goal is to keep this under one byte for efficiency.
        assert(@sizeOf(Tag) == 1);
    }

    /// Note: The FooComma/FooSemicolon variants exist to ease the implementation of
    /// Ast.lastToken()
    pub const Tag = enum {
        /// sub_list[lhs...rhs]
        root,

        assignment_stmt,
        branch_stmt,
        def_stmt,
        if_stmt,
        return_stmt,
        simple_stmt,

        binary_expr,
        cond_expr,
        expr,
        lambda_expr,
        paren_expr,
        tuple_expr,
        unary_expr,

        dot,
        ident,
        literal,
        params,
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const SubRange = struct {
        /// Index into sub_list.
        start: Index,
        /// Index into sub_list.
        end: Index,
    };

    pub const IfStmt = struct {
        then_expr: Index,
        else_expr: Index,
    };

    pub const CondExpr = struct {
        then_expr: Index,
        else_expr: Index,
    };

    pub const DefStmt = struct {
        params_start: Index,
        params_end: Index,
        /// Populated if align(A) is present.
        align_expr: Index,
        /// Populated if addrspace(A) is present.
        addrspace_expr: Index,
        /// Populated if linksection(A) is present.
        section_expr: Index,
        /// Populated if callconv(A) is present.
        callconv_expr: Index,
    };
};
