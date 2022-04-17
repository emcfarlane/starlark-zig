const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const AstError = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const syntax = @import("syntax.zig");
const Token = syntax.Token;
const Tokenizer = syntax.Tokenizer;

pub const Error = error{ParseError} || Allocator.Error;

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8) Error!Ast {
    var tokens = Ast.TokenList{};
    defer tokens.deinit(gpa);

    // Guess from zig that we have an 8:1 ratio of source bytes to token count.
    const estimated_token_count = source.len / 8;
    try tokens.ensureTotalCapacity(gpa, estimated_token_count);

    var tokenizer = try Tokenizer.init(gpa, source);
    defer tokenizer.deinit();
    while (true) {
        const token = try tokenizer.next();
        std.debug.print("token: {}\n", .{token});
        try tokens.append(gpa, .{
            .tag = token.tag,
            .loc = token.loc,
        });
        if (token.tag == .eof) break;
    }

    var parser: Parser = .{
        .source = source,
        .gpa = gpa,
        .token_tags = tokens.items(.tag),
        .errors = .{},
        .nodes = .{},
        .extra_data = .{},
        .scratch = .{},
        .tok_i = 0,
    };
    defer parser.errors.deinit(gpa);
    defer parser.nodes.deinit(gpa);
    defer parser.extra_data.deinit(gpa);
    defer parser.scratch.deinit(gpa);

    // Guess from Zig, has a 2:1 ratio of tokens to AST nodes.
    // Make sure at least 1 so we can use appendAssumeCapacity on the root node below.
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(gpa, estimated_node_count);

    // Root node must be index 0.
    // Root <- skip ContainerMembers eof
    parser.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });
    const root_stmts = try parser.parseStmts();
    const root_decls = try root_stmts.toSpan(&parser);
    _ = try parser.expectToken(.eof);
    parser.nodes.items(.data)[0] = .{
        .lhs = root_decls.start,
        .rhs = root_decls.end,
    };

    return Ast{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = parser.extra_data.toOwnedSlice(gpa),
        .errors = parser.errors.toOwnedSlice(gpa),
    };
}

const null_node: Node.Index = 0;

/// Represents in-progress parsing, will be converted to an Ast after completion.
const Parser = struct {
    gpa: Allocator,
    source: []const u8,
    token_tags: []Token.Tag, // var for "not in"
    //token_starts: []const Ast.ByteOffset,
    tok_i: TokenIndex,
    nodes: Ast.NodeList,
    errors: std.ArrayListUnmanaged(AstError),
    extra_data: std.ArrayListUnmanaged(Node.Index),
    scratch: std.ArrayListUnmanaged(Node.Index),

    const SmallSpan = union(enum) {
        zero_or_one: Node.Index,
        multi: Node.SubRange,
    };

    const Stmts = struct {
        len: usize,
        lhs: Node.Index,
        rhs: Node.Index,

        fn toSpan(self: Stmts, p: *Parser) !Node.SubRange {
            if (self.len <= 2) {
                const nodes = [2]Node.Index{ self.lhs, self.rhs };
                return p.listToSpan(nodes[0..self.len]);
            } else {
                return Node.SubRange{ .start = self.lhs, .end = self.rhs };
            }
        }
    };

    fn listToSpan(p: *Parser, list: []const Node.Index) !Node.SubRange {
        try p.extra_data.appendSlice(p.gpa, list);
        return Node.SubRange{
            .start = @intCast(Node.Index, p.extra_data.items.len - list.len),
            .end = @intCast(Node.Index, p.extra_data.items.len),
        };
    }

    fn addNode(p: *Parser, elem: Ast.NodeList.Elem) Allocator.Error!Node.Index {
        std.debug.print("addNode: {}\n", .{elem});
        const result = @intCast(Node.Index, p.nodes.len);
        try p.nodes.append(p.gpa, elem);
        return result;
    }

    fn setNode(p: *Parser, i: usize, elem: Ast.NodeList.Elem) Node.Index {
        p.nodes.set(i, elem);
        return @intCast(Node.Index, i);
    }

    fn addExtra(p: *Parser, extra: anytype) Allocator.Error!Node.Index {
        const fields = std.meta.fields(@TypeOf(extra));
        try p.extra_data.ensureUnusedCapacity(p.gpa, fields.len);
        const result = @intCast(u32, p.extra_data.items.len);
        inline for (fields) |field| {
            comptime assert(field.field_type == Node.Index);
            p.extra_data.appendAssumeCapacity(@field(extra, field.name));
        }
        return result;
    }

    fn failMsg(p: *Parser, msg: Ast.Error) error{ ParseError, OutOfMemory } {
        @setCold(true);
        try p.errors.append(p.gpa, msg);
        return error.ParseError;
    }

    fn fail(p: *Parser, tag: Ast.Error.Tag) error{ ParseError, OutOfMemory } {
        @setCold(true);
        return p.failMsg(.{ .tag = tag, .token = p.tok_i });
    }

    fn parseStmts(p: *Parser) !Stmts {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            switch (p.token_tags[p.tok_i]) {
                .eof => break,
                .newline => {
                    _ = p.nextToken();
                },
                else => {
                    const stmt = try p.parseStmt();
                    if (stmt == 0) break;
                    try p.scratch.append(p.gpa, stmt);
                },
            }
        }

        const items = p.scratch.items[scratch_top..];
        switch (items.len) {
            0 => return Stmts{
                .len = 0,
                .lhs = 0,
                .rhs = 0,
            },
            1 => return Stmts{
                .len = 1,
                .lhs = items[0],
                .rhs = 0,
            },
            2 => return Stmts{
                .len = 2,
                .lhs = items[0],
                .rhs = items[1],
            },
            else => {
                const span = try p.listToSpan(items);
                return Stmts{
                    .len = items.len,
                    .lhs = span.start,
                    .rhs = span.end,
                };
            },
        }
    }

    fn parseStmt(p: *Parser) !Node.Index {
        return try switch (p.token_tags[p.tok_i]) {
            .keyword_def => p.parseDefStmt(),
            .keyword_if => p.parseIfStmt(),
            .keyword_for => p.parseForStmt(),
            .keyword_while => p.parseWhileStmt(),
            else => p.parseSimpleStmt(),
        };
    }

    fn parseDefStmt(p: *Parser) !Node.Index {
        const def_token = p.eatToken(.keyword_def) orelse return null_node;
        std.debug.print("{}\n", .{def_token});
        unreachable;
    }
    fn parseIfStmt(p: *Parser) !Node.Index {
        const if_token = p.eatToken(.keyword_if) orelse return null_node;
        std.debug.print("{}\n", .{if_token});
        unreachable;
    }
    fn parseForStmt(p: *Parser) !Node.Index {
        const for_token = p.eatToken(.keyword_for) orelse return null_node;
        std.debug.print("{}\n", .{for_token});
        unreachable;
    }
    fn parseWhileStmt(p: *Parser) !Node.Index {
        const while_token = p.eatToken(.keyword_while) orelse return null_node;
        std.debug.print("{}\n", .{while_token});
        unreachable;
    }
    fn parseSimpleStmt(p: *Parser) !Node.Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        const main_token = p.tok_i;

        while (true) {
            const stmt = try parseSmallStmt(p);
            try p.scratch.append(p.gpa, stmt);

            if (p.currentTag() != .semi) break;
            _ = p.nextToken(); // consume semi

            const tag = p.currentTag();
            if (tag == .newline or tag == .eof) break;
        }

        const stmts = p.scratch.items[scratch_top..];
        switch (stmts.len) {
            0 => return p.addNode(.{
                .tag = .simple_stmt,
                .main_token = main_token,
                .data = .{
                    .lhs = 0,
                    .rhs = 0,
                },
            }),
            1 => return p.addNode(.{
                .tag = .simple_stmt,
                .main_token = main_token,
                .data = .{
                    .lhs = stmts[0],
                    .rhs = 0,
                },
            }),
            2 => return p.addNode(.{
                .tag = .simple_stmt,
                .main_token = main_token,
                .data = .{
                    .lhs = stmts[0],
                    .rhs = stmts[1],
                },
            }),
            else => {
                const span = try p.listToSpan(stmts);
                return p.addNode(.{
                    .tag = .simple_stmt,
                    .main_token = main_token,
                    .data = .{
                        .lhs = span.start,
                        .rhs = span.end,
                    },
                });
            },
        }
    }

    // small_stmt = RETURN expr?
    //            | PASS | BREAK | CONTINUE
    //            | LOAD ...
    //            | expr ('=' | '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=') expr   // assign
    //            | expr
    fn parseSmallStmt(p: *Parser) !Node.Index {
        switch (p.currentTag()) {
            .keyword_return => {
                const return_token = p.nextToken(); // consume

                var result: Node.Index = 0;
                const tag = p.currentTag();
                if (tag != .keyword_return and tag != .newline and tag != .semi) {
                    result = try p.parseExpr(false);
                }

                return p.addNode(.{
                    .tag = .return_stmt,
                    .main_token = return_token,
                    .data = .{
                        .lhs = result,
                        .rhs = 0,
                    },
                });
            },
            .keyword_break, .keyword_continue, .keyword_pass => {
                const simple_token = p.nextToken(); // consume
                return p.addNode(.{
                    .tag = .branch_stmt,
                    .main_token = simple_token,
                    .data = .{
                        .lhs = 0,
                        .rhs = 0,
                    },
                });
            },
            .keyword_load => {
                return p.parseLoadStmt();
            },
            else => {},
        }

        // Assigment
        const x = try p.parseExpr(false);
        switch (p.token_tags[p.tok_i]) {
            .eq, .plus_eq, .minus_eq, .star_eq, .slash_eq, .slashslash_eq, .percent_eq, .amp_eq, .pipe_eq, .circumflex_eq, .ltlt_eq, .gtgt_eq => {
                const op_token = p.nextToken();
                const rhs = try p.parseExpr(false);
                return p.addNode(.{
                    .tag = .assignment_stmt,
                    .main_token = op_token,
                    .data = .{
                        .lhs = x,
                        .rhs = rhs,
                    },
                });
            },
            else => {
                const main_token = p.nodes.items(.main_token)[x]; // hacky
                return p.addNode(.{
                    .tag = .expr,
                    .main_token = main_token,
                    .data = .{
                        .lhs = x,
                        .rhs = 0,
                    },
                });
            },
        }
    }

    // parseExpr parses an expression, possible consisting of a
    // comma-separated list of 'test' expressions.
    //
    // In many cases we must use parseTest to avoid ambiguity such as
    // f(x, y) vs. f((x, y)).
    fn parseExpr(p: *Parser, in_parens: bool) !Node.Index {
        std.debug.print("parseExpr {}\n", .{in_parens});

        const x = try p.parseTest();
        if (p.token_tags[p.tok_i] != .comma) {
            return x;
        }
        // tuple
        return try p.parseExprs(.tuple_expr, x, in_parens);
    }

    // preclevels groups operators of equal precedence.
    // Comparisons are nonassociative; other binary operators associate to the left.
    // Unary MINUS, unary PLUS, and TILDE have higher precedence so are handled in parsePrimary.
    // See https://github.com/google/starlark-go/blob/master/doc/spec.md#binary-operators
    // precedence maps each operator to its precedence (0-7), or -1 for other tokens.
    fn precedence(tag: Token.Tag) i8 {
        return switch (tag) {
            .keyword_or => 0, // OR
            .keyword_and => 1, // AND
            .keyword_not => 2, // NOT (UNARY)
            .eql, .neq, .lt, .gt, .lt_eq, .gt_eq, .keyword_in, .keyword_not_in => 3, // == != < > <= >= IN NOT IN
            .pipe => 4, // |
            .circumflex => 5, // ^
            .amp => 6, // &
            .ltlt, .gtgt => 7, // << >>
            .minus, .plus => 8, // -
            .star, .percent, .slash, .slashslash => 9, // * % / //
            else => -1,
        };
    }

    //  primary = IDENT
    //          | INT | FLOAT | STRING | BYTES
    //          | '[' ...                    // list literal or comprehension
    //          | '{' ...                    // dict literal or comprehension
    //          | '(' ...                    // tuple or parenthesized expression
    //          | ('-'|'+'|'~') primary_with_suffix
    fn parsePrimary(p: *Parser) !Node.Index {
        switch (p.currentTag()) {
            .ident => {
                return p.parseIdent();
            },
            .int, .float, .string, .bytes => {
                const main_token = p.nextToken();
                return p.addNode(.{
                    .tag = .literal,
                    .main_token = main_token,
                    .data = .{
                        .lhs = 0,
                        .rhs = 0,
                    },
                });
            },
            .lbrack => {
                return p.parseList();
            },
            .lbrace => {
                return p.parseDict();
            },
            .lparen => {
                const lparen_token = p.nextToken();
                if (p.currentTag() == .rparen) {
                    // empty tuple
                    _ = p.nextToken();
                    return p.addNode(.{
                        .tag = .tuple_expr,
                        .main_token = lparen_token,
                        .data = .{
                            .lhs = 0,
                            .rhs = 0,
                        },
                    });
                }
                const x = try p.parseExpr(true); // allow trailing comma
                _ = try p.expectToken(.rparen);
                return p.addNode(.{
                    .tag = .paren_expr,
                    .main_token = lparen_token,
                    .data = .{
                        .lhs = x,
                        .rhs = 0,
                    },
                });
            },
            .minus, .plus, .tilde => {
                const main_token = p.nextToken();
                const x = try p.parsePrimaryWithSuffix();
                return p.addNode(.{
                    .tag = .unary_expr,
                    .main_token = main_token,
                    .data = .{
                        .lhs = x,
                        .rhs = 0,
                    },
                });
            },
            else => {
                std.debug.print("got {}\n", .{p.currentTag()});
                return p.fail(.expected_primary_expr);
            },
        }
    }

    fn parseList(_: *Parser) !Node.Index {
        unreachable;
    }

    fn parseDict(_: *Parser) !Node.Index {
        unreachable;
    }

    // primary_with_suffix = primary
    //                     | primary '.' IDENT
    //                     | primary slice_suffix
    //                     | primary call_suffix
    fn parsePrimaryWithSuffix(p: *Parser) Error!Node.Index {
        var x = try p.parsePrimary();
        while (true) {
            switch (p.currentTag()) {
                .dot => {
                    const dot_token = p.nextToken();
                    const id = try p.parseIdent();
                    x = try p.addNode(.{
                        .tag = .dot,
                        .main_token = dot_token,
                        .data = .{
                            .lhs = id,
                            .rhs = 0,
                        },
                    });
                },
                .lbrack => {
                    x = try p.parseSliceSuffix(x);
                },
                .lparen => {
                    x = try p.parseCallSuffix(x);
                },
                else => {
                    return x;
                },
            }
        }
    }

    // slice_suffix = '[' expr? ':' expr?  ':' expr? ']'
    fn parseSliceSuffix(_: *Parser, parent: Node.Index) !Node.Index {
        std.debug.print("parseSliceSuffix {}\n", .{parent});
        unreachable;
    }

    // call_suffix = '(' arg_list? ')'
    fn parseCallSuffix(_: *Parser, parent: Node.Index) !Node.Index {
        std.debug.print("parseCallSuffix {}\n", .{parent});
        unreachable;
    }

    fn parseIdent(p: *Parser) !Node.Index {
        const ident_token = try p.expectToken(.ident);

        return p.addNode(.{
            .tag = .ident,
            .main_token = ident_token,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        });
    }

    fn parseTestPrec(p: *Parser, prec: i8) Error!Node.Index {
        std.debug.print("parseTestPrec {}\n", .{prec});

        if (prec >= 10) {
            return p.parsePrimaryWithSuffix();
        }

        const tag = p.currentTag();
        if (tag == .keyword_not and prec == precedence(tag)) {
            const main_token = p.nextToken();
            const x = try p.parseTestPrec(prec);

            return p.addNode(.{
                .tag = .unary_expr,
                .main_token = main_token,
                .data = .{
                    .lhs = x,
                    .rhs = 0,
                },
            });
        }
        return p.parseBinopExpr(prec);
    }

    // expr = test (OP test)*
    // Uses precedence climbing; see http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing.
    fn parseBinopExpr(p: *Parser, prec: i8) Error!Node.Index {
        var x = try p.parseTestPrec(prec + 1);

        var first = true;
        while (true) {
            if (p.currentTag() == .keyword_not) {
                _ = p.nextToken();
                // In this context, NOT must be followed by IN.
                // Replace NOT IN by a single NOT_IN token.
                _ = try p.expectToken(.keyword_in);
                p.token_tags[p.tok_i] = .keyword_not_in;
            }

            // Binary operator of specified precedence?
            const opprec = precedence(p.currentTag());
            std.debug.print("opprec {}, prec {}", .{ .opprec, .prec });
            if (opprec < prec) {
                return x;
            }

            // Comparisons are non-associative.
            if (!first and opprec == precedence(.eql)) {
                // %s does not associate with %s (use parens)
                // x.(*BinaryExpr).Op, p.tok)
                return p.fail(.unexpected_associate);
            }

            const op_token = p.nextToken();
            const y = try p.parseTestPrec(opprec + 1);
            x = try p.addNode(.{
                .tag = .binary_expr,
                .main_token = op_token,
                .data = .{
                    .lhs = x,
                    .rhs = y,
                },
            });
            first = false;
        }
    }

    fn parseTestNoCond(_: *Parser) !Node.Index {
        std.debug.print("parseTestNoCond", .{});
        unreachable;
    }

    // parseTest parses a 'test', a single-component expression.
    fn parseTest(p: *Parser) Error!Node.Index {
        if (p.token_tags[p.tok_i] == .keyword_lambda) {
            return p.parseLambda(true);
        }

        const main_token = p.tok_i;
        const x = try p.parseTestPrec(0);

        // conditional expression (t IF cond ELSE f)
        if (p.currentTag() == .keyword_if) {
            _ = p.nextToken(); // if token

            const cond_expr = try p.parseTestPrec(0);
            _ = try p.expectToken(.keyword_else);
            const else_expr = try p.parseTest();

            return p.addNode(.{
                .tag = .cond_expr,
                .main_token = main_token,
                .data = .{
                    .lhs = cond_expr,
                    .rhs = try p.addExtra(Node.CondExpr{
                        .then_expr = x, // true
                        .else_expr = else_expr,
                    }),
                },
            });
        }
        return x;
    }

    // parseLambda parses a lambda expression.
    // The allowCond flag allows the body to be an 'a if b else c' conditional.
    fn parseLambda(p: *Parser, allow_cond: bool) !Node.Index {
        const lambda_token = p.eatToken(.keyword_lambda) orelse return null_node;
        std.debug.print("{}\n", .{lambda_token});

        const params = try p.parseParams();
        var body: Node.Index = null_node;
        if (allow_cond) {
            //body = try p.parseTest();
            unreachable;
        } else {
            body = try p.parseTestNoCond();
        }

        return p.addNode(.{
            .tag = .lambda_expr,
            .main_token = lambda_token,
            .data = .{
                .lhs = params,
                .rhs = body,
            },
        });
    }

    // params = (param COMMA)* param COMMA?
    //        |
    //
    // param = IDENT
    //       | IDENT EQ test
    //       | STAR
    //       | STAR IDENT
    //       | STARSTAR IDENT
    //
    // parseParams parses a parameter list.  The resulting expressions are of the form:
    //
    //      *Ident                                          x
    //      *Binary{Op: EQ, X: *Ident, Y: Expr}             x=y
    //      *Unary{Op: STAR}                                *
    //      *Unary{Op: STAR, X: *Ident}                     *args
    //      *Unary{Op: STARSTAR, X: *Ident}                 **kwargs
    fn parseParams(p: *Parser) !Node.Index {
        std.debug.print("parseParams {}\n", .{p.currentTag()});
        unreachable;
    }

    // parseExprs parses a comma-separated list of expressions, starting with the comma.
    // It is used to parse tuples and list elements.
    // expr_list = (',' expr)* ','?
    fn parseExprs(p: *Parser, tag: Ast.Node.Tag, first: Node.Index, in_parens: bool) !Node.Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        const main_token = p.nodes.items(.main_token)[first]; // hacky

        try p.scratch.append(p.gpa, first);
        while (p.token_tags[p.tok_i] == .comma) {
            _ = p.eatToken(.comma);
            if (terminatesExprList(p.currentTag())) {
                // not allowed trailing commma outside parens
                if (!in_parens) {
                    return p.fail(.unexpected_trailing_comma);
                }
                break;
            }
            std.debug.print("starting expr {}", .{p.currentTag()});

            const expr = try p.parseTest();
            try p.scratch.append(p.gpa, expr);
        }

        const stmts = p.scratch.items[scratch_top..];
        switch (stmts.len) {
            0 => return p.addNode(.{
                .tag = tag,
                .main_token = main_token,
                .data = .{
                    .lhs = 0,
                    .rhs = 0,
                },
            }),
            1 => return p.addNode(.{
                .tag = tag,
                .main_token = main_token,
                .data = .{
                    .lhs = stmts[0],
                    .rhs = 0,
                },
            }),
            2 => return p.addNode(.{
                .tag = tag,
                .main_token = main_token,
                .data = .{
                    .lhs = stmts[0],
                    .rhs = stmts[1],
                },
            }),
            else => {
                const span = try p.listToSpan(stmts);
                return p.addNode(.{
                    .tag = tag,
                    .main_token = main_token,
                    .data = .{
                        .lhs = span.start,
                        .rhs = span.end,
                    },
                });
            },
        }
    }

    fn parseLoadStmt(p: *Parser) !Node.Index {
        const load_token = p.eatToken(.keyword_load) orelse return null_node;
        std.debug.print("parseLoad {}\n", .{load_token});

        unreachable;
    }

    fn expectToken(p: *Parser, tag: Token.Tag) Error!TokenIndex {
        if (p.token_tags[p.tok_i] != tag) {
            return p.failMsg(.{
                .tag = .expected_token,
                .token = p.tok_i,
                .extra = .{ .expected_tag = tag },
            });
        }
        return p.nextToken();
    }

    fn eatToken(p: *Parser, tag: Token.Tag) ?TokenIndex {
        return if (p.token_tags[p.tok_i] == tag) p.nextToken() else null;
    }

    fn assertToken(p: *Parser, tag: Token.Tag) TokenIndex {
        const token = p.nextToken();
        assert(p.token_tags[token] == tag);
        return token;
    }

    fn nextToken(p: *Parser) TokenIndex {
        const result = p.tok_i;
        p.tok_i += 1;
        return result;
    }
    fn currentTag(p: *Parser) Token.Tag {
        return p.token_tags[p.tok_i];
    }

    fn terminatesExprList(tag: Token.Tag) bool {
        return switch (tag) {
            .eof, .newline, .eq, .rbrace, .rbrack, .rparen, .semi => true,
            else => false,
        };
    }
};

test "assignment pass" {
    try testAst(
        \\x = 2
    , &.{
        .root,
        .ident,
        .literal,
        .assignment_stmt,
        .simple_stmt,
    });
}

fn testAst(source: [:0]const u8, expected_tags: []const Ast.Node.Tag) !void {
    const tgpa = std.testing.allocator;

    var ast = try parse(tgpa, source);
    defer ast.deinit(tgpa);

    var i: usize = 0;
    const tags = ast.nodes.items(.tag);
    for (expected_tags) |expected_tag| {
        const tag = tags[i];
        if (tag != expected_tag) {
            std.debug.panic("expected {s}, found {s}\n", .{
                @tagName(expected_tag), @tagName(tag),
            });
        }
        i += 1;
    }
    try std.testing.expectEqual(tags.len, expected_tags.len);
}
