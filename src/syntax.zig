const std = @import("std");
const mem = std.mem;

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const keywords = std.ComptimeStringMap(Tag, .{
        .{ "and", .keyword_and },
        .{ "break", .keyword_break },
        .{ "continue", .keyword_continue },
        .{ "def", .keyword_def },
        .{ "elif", .keyword_elif },
        .{ "else", .keyword_else },
        .{ "for", .keyword_for },
        .{ "if", .keyword_if },
        .{ "in", .keyword_in },
        .{ "lambda", .keyword_lambda },
        .{ "load", .keyword_load },
        .{ "not", .keyword_not },
        .{ "not_in", .keyword_not_in },
        .{ "or", .keyword_or },
        .{ "pass", .keyword_pass },
        .{ "return", .keyword_return },
        .{ "while", .keyword_while },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const Tag = enum {
        invalid,
        eof,

        newline,
        indent,
        outdent,

        // TOKENS WITH VALUES
        ident, // X
        int, // 123
        float, // 1.23E45
        string, // "FOO" OR 'FOO' OR '''FOO''' OR R'FOO' OR R"FOO"
        bytes, // B"FOO", ETC

        // PUNCTUATION
        plus, // +
        minus, // -
        star, // *
        slash, // /
        slashslash, // //
        percent, // %
        amp, // &
        pipe, // |
        circumflex, // ^
        ltlt, // <<
        gtgt, // >>
        tilde, // ~
        dot, // .
        comma, // ,
        eq, // =
        semi, // ;
        colon, // :
        lparen, // (
        rparen, // )
        lbrack, // [
        rbrack, // ]
        lbrace, // {
        rbrace, // }
        lt, // <
        gt, // >
        lt_eq, // <=
        gt_eq, // >=
        eql, // ==
        neq, // !=
        plus_eq, // +=    (KEEP ORDER CONSISTENT WITH plus..gtgt)
        minus_eq, // -=
        star_eq, // *=
        slash_eq, // /=
        slashslash_eq, // //=
        percent_eq, // %=
        amp_eq, // &=
        pipe_eq, // |=
        circumflex_eq, // ^=
        ltlt_eq, // <<=
        gtgt_eq, // >>=
        starstar, // **

        // keywords
        keyword_and,
        keyword_break,
        keyword_continue,
        keyword_def,
        keyword_elif,
        keyword_else,
        keyword_for,
        keyword_if,
        keyword_in,
        keyword_lambda,
        keyword_load,
        keyword_not,
        keyword_not_in, // SYNTHESIZED BY PARSER FROM not in
        keyword_or,
        keyword_pass,
        keyword_return,
        keyword_while,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .invalid,
                .eof,
                .newline,
                .indent,
                .outdent,
                .ident,
                .int,
                .float,
                .string,
                .bytes,
                => null,

                .plus => "+",
                .minus => "-",
                .star => "*",
                .slash => "/",
                .slashslash => "//",
                .percent => "%",
                .amp => "&",
                .pipe => "|",
                .circumflex => "^",
                .ltlt => "<<",
                .gtgt => ">>",
                .tilde => "~",
                .dot => ".",
                .comma => ",",
                .eq => "=",
                .semi => ";",
                .colon => ":",
                .lparen => "(",
                .rparen => ")",
                .lbrack => "[",
                .rbrack => "]",
                .lbrace => "{",
                .rbrace => "}",
                .lt => "<",
                .gt => ">",
                .ge => ">=",
                .le => "<=",
                .eql => "==",
                .neq => "!=",
                .plus_eq => "+=",
                .minus_eq => "-=",
                .star_eq => "*=",
                .slash_eq => "/=",
                .slashslash_eq => "//=",
                .percent_eq => "%=",
                .amp_eq => "&=",
                .pipe_eq => "|=",
                .circumflex_eq => "^=",
                .ltlt_eq => "<<=",
                .gtgt_eq => ">>=",
                .starstar => "**",
                .keyword_and => "and",
                .keyword_break => "break",
                .keyword_continue => "continue",
                .keyword_def => "def",
                .keyword_elif => "elif",
                .keyword_else => "else",
                .keyword_for => "for",
                .keyword_if => "if",
                .keyword_in => "in",
                .keyword_lambda => "lambda",
                .keyword_load => "load",
                .keyword_not => "not",
                .keyword_not_in => "not_in ", // SYNTHESIZED BY PARSER FROM not in
                .keyword_or => "or",
                .keyword_pass => "pass",
                .keyword_return => "return",
                .keyword_while => "while",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .invalid => "illegal ",
                .eof => "eof",
                .newline => "newline",
                .indent => "indent",
                .outdent => "outdent",
                .ident => "ident",
                .int => "int",
                .float => "float",
                .string => "string",
                .bytes => "bytes",
                else => unreachable,
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,
    pending_invalid_token: ?Token,

    /// For debugging purposes
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // Skip the UTF-8 BOM if present
        const src_start = if (mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else @as(usize, 0);
        return Tokenizer{
            .buffer = buffer,
            .index = src_start,
            .pending_invalid_token = null,
        };
    }

    const State = enum {
        start,
        ident,
        ident_1, // could be string literal.
        string,
        string_backslash,
        bytes,
        bytes_backslash,

        plus,
        minus,
        star,
        slash,
        slashslash,
        percent,
        amp,
        pipe,
        circumflex,
        lt,
        ltlt,
        gt,
        gtgt,
        eq,

        // Zig numbers
        zero,
        int_dec,
        int_dec_no_underscore,
        int_bin,
        int_bin_no_underscore,
        int_oct,
        int_oct_no_underscore,
        int_hex,
        int_hex_no_underscore,
        num_dot_dec,
        num_dot_hex,
        float_dec,
        float_dec_no_underscore,
        float_hex,
        float_hex_no_underscore,
        float_exponent_unsigned,
        float_exponent_num,
        float_exponent_num_no_underscore,
    };

    pub fn next(self: *Tokenizer) Token {
        if (self.pending_invalid_token) |token| {
            self.pending_invalid_token = null;
            return token;
        }
        var state: State = .start;
        var result = Token{
            .tag = .eof,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };
        //var seen_escape_digits: usize = undefined;
        //var remaining_code_units: usize = undefined;
        while (true) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    0 => break,
                    ' ', '\n', '\t', '\r' => {
                        result.loc.start = self.index + 1;
                    },
                    '"' => {
                        state = .string;
                        result.tag = .string;
                    },
                    '0' => {
                        state = .zero;
                        result.tag = .int;
                    },
                    '1'...'9' => {
                        state = .int_dec;
                        result.tag = .int;
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        state = .ident_1;
                        result.tag = .ident;
                    },
                    // PUNCTUATION
                    '+' => {
                        state = .plus;
                    },
                    '-' => {
                        state = .minus;
                    },
                    '*' => {
                        state = .star;
                    },
                    '/' => {
                        state = .slash;
                    },
                    '%' => {
                        state = .percent;
                    },
                    '&' => {
                        state = .amp;
                    },
                    '|' => {
                        state = .pipe;
                    },
                    '^' => {
                        state = .circumflex;
                    },
                    '<' => {
                        state = .lt;
                    },
                    '>' => {
                        state = .gt;
                    },
                    '~' => {
                        result.tag = .tilde;
                        self.index += 1;
                        break;
                    },
                    '.' => {
                        result.tag = .dot;
                        self.index += 1;
                        break;
                    },
                    ',' => {
                        result.tag = .comma;
                        self.index += 1;
                        break;
                    },
                    '=' => {
                        state = .eq;
                    },
                    ';' => {
                        result.tag = .semi;
                        self.index += 1;
                        break;
                    },
                    ':' => {
                        result.tag = .colon;
                        self.index += 1;
                        break;
                    },
                    '(' => {
                        result.tag = .lparen;
                        self.index += 1;
                        break;
                    },
                    ')' => {
                        result.tag = .rparen;
                        self.index += 1;
                        break;
                    },
                    '[' => {
                        result.tag = .lbrack;
                        self.index += 1;
                        break;
                    },
                    ']' => {
                        result.tag = .rbrack;
                        self.index += 1;
                        break;
                    },
                    '{' => {
                        result.tag = .lbrace;
                        self.index += 1;
                        break;
                    },
                    '}' => {
                        result.tag = .rbrace;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .invalid;
                        result.loc.end = self.index;
                        self.index += 1;
                        return result;
                    },
                },

                .ident => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {
                        if (Token.getKeyword(self.buffer[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                },

                .ident_1 => switch (c) {
                    '"', '\'' => switch (self.buffer[self.index - 1]) {
                        'b' => {
                            result.tag = .bytes;
                            state = .bytes;
                        },
                        // TODO: 'r' raw.
                        else => {
                            result.tag = .invalid;
                            break;
                        },
                    },
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {
                        state = .ident;
                    },
                    else => {
                        if (Token.getKeyword(self.buffer[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                },

                .plus => switch (c) {
                    '=' => {
                        result.tag = .plus_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .plus;
                        break;
                    },
                },

                .minus => switch (c) {
                    '=' => {
                        result.tag = .minus_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .minus;
                        break;
                    },
                },

                .star => switch (c) {
                    '*' => {
                        result.tag = .starstar;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .star;
                        break;
                    },
                },

                .slash => switch (c) {
                    '/' => {
                        state = .slashslash;
                    },
                    else => {
                        result.tag = .slash;
                        break;
                    },
                },

                .slashslash => switch (c) {
                    '=' => {
                        result.tag = .slashslash_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .slashslash;
                        break;
                    },
                },

                .percent => switch (c) {
                    '=' => {
                        result.tag = .percent_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .percent;
                        break;
                    },
                },

                .amp => switch (c) {
                    '=' => {
                        result.tag = .amp_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .amp;
                        break;
                    },
                },

                .pipe => switch (c) {
                    '=' => {
                        result.tag = .pipe_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .pipe;
                        break;
                    },
                },

                .circumflex => switch (c) {
                    '=' => {
                        result.tag = .circumflex_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .circumflex;
                        break;
                    },
                },

                .eq => switch (c) {
                    '=' => {
                        result.tag = .eql;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .eq;
                        break;
                    },
                },

                .lt => switch (c) {
                    '=' => {
                        result.tag = .lt_eq;
                        self.index += 1;
                        break;
                    },
                    '<' => {
                        state = .ltlt;
                    },
                    else => {
                        result.tag = .lt;
                        break;
                    },
                },

                .ltlt => switch (c) {
                    '=' => {
                        result.tag = .ltlt_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .ltlt;
                        break;
                    },
                },

                .gt => switch (c) {
                    '=' => {
                        result.tag = .gt_eq;
                        self.index += 1;
                        break;
                    },
                    '>' => {
                        state = .gtgt;
                    },
                    else => {
                        result.tag = .gt;
                        break;
                    },
                },

                .gtgt => switch (c) {
                    '=' => {
                        result.tag = .gtgt_eq;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .gtgt;
                        break;
                    },
                },

                .bytes => switch (c) {
                    '\\' => {
                        state = .bytes_backslash;
                    },
                    '"' => {
                        self.index += 1;
                        break;
                    },
                    0 => {
                        if (self.index == self.buffer.len) {
                            break;
                        } else {
                            self.checkLiteralCharacter();
                        }
                    },
                    '\n' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => self.checkLiteralCharacter(),
                },
                .bytes_backslash => switch (c) {
                    0, '\n' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => {
                        state = .bytes;
                    },
                },

                .string => switch (c) {
                    '\\' => {
                        state = .string_backslash;
                    },
                    '"' => {
                        self.index += 1;
                        break;
                    },
                    0 => {
                        if (self.index == self.buffer.len) {
                            break;
                        } else {
                            self.checkLiteralCharacter();
                        }
                    },
                    '\n' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => self.checkLiteralCharacter(),
                },
                .string_backslash => switch (c) {
                    0, '\n' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => {
                        state = .string;
                    },
                },

                .zero => switch (c) {
                    'b' => {
                        state = .int_bin_no_underscore;
                    },
                    'o' => {
                        state = .int_oct_no_underscore;
                    },
                    'x' => {
                        state = .int_hex_no_underscore;
                    },
                    '0'...'9', '_', '.', 'e', 'E' => {
                        // reinterpret as a decimal number
                        self.index -= 1;
                        state = .int_dec;
                    },
                    'a', 'c', 'd', 'f'...'n', 'p'...'w', 'y', 'z', 'A'...'D', 'F'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .int_bin_no_underscore => switch (c) {
                    '0'...'1' => {
                        state = .int_bin;
                    },
                    else => {
                        result.tag = .invalid;
                        break;
                    },
                },
                .int_bin => switch (c) {
                    '_' => {
                        state = .int_bin_no_underscore;
                    },
                    '0'...'1' => {},
                    '2'...'9', 'a'...'z', 'A'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .int_oct_no_underscore => switch (c) {
                    '0'...'7' => {
                        state = .int_oct;
                    },
                    else => {
                        result.tag = .invalid;
                        break;
                    },
                },
                .int_oct => switch (c) {
                    '_' => {
                        state = .int_oct_no_underscore;
                    },
                    '0'...'7' => {},
                    '8', '9', 'a'...'z', 'A'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .int_dec_no_underscore => switch (c) {
                    '0'...'9' => {
                        state = .int_dec;
                    },
                    else => {
                        result.tag = .invalid;
                        break;
                    },
                },
                .int_dec => switch (c) {
                    '_' => {
                        state = .int_dec_no_underscore;
                    },
                    '.' => {
                        state = .num_dot_dec;
                        result.tag = .invalid;
                    },
                    'e', 'E' => {
                        state = .float_exponent_unsigned;
                        result.tag = .float;
                    },
                    '0'...'9' => {},
                    'a'...'d', 'f'...'z', 'A'...'D', 'F'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .int_hex_no_underscore => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        state = .int_hex;
                    },
                    else => {
                        result.tag = .invalid;
                        break;
                    },
                },
                .int_hex => switch (c) {
                    '_' => {
                        state = .int_hex_no_underscore;
                    },
                    '.' => {
                        state = .num_dot_hex;
                        result.tag = .invalid;
                    },
                    'p', 'P' => {
                        state = .float_exponent_unsigned;
                        result.tag = .float;
                    },
                    '0'...'9', 'a'...'f', 'A'...'F' => {},
                    'g'...'o', 'q'...'z', 'G'...'O', 'Q'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .num_dot_dec => switch (c) {
                    '.' => {
                        result.tag = .int;
                        self.index -= 1;
                        state = .start;
                        break;
                    },
                    '0'...'9' => {
                        result.tag = .float;
                        state = .float_dec;
                    },
                    '_', 'a'...'z', 'A'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .num_dot_hex => switch (c) {
                    '.' => {
                        result.tag = .string;
                        self.index -= 1;
                        state = .start;
                        break;
                    },
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        result.tag = .float;
                        state = .float_hex;
                    },
                    '_', 'g'...'z', 'G'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .float_dec_no_underscore => switch (c) {
                    '0'...'9' => {
                        state = .float_dec;
                    },
                    else => {
                        result.tag = .invalid;
                        break;
                    },
                },
                .float_dec => switch (c) {
                    '_' => {
                        state = .float_dec_no_underscore;
                    },
                    'e', 'E' => {
                        state = .float_exponent_unsigned;
                    },
                    '0'...'9' => {},
                    'a'...'d', 'f'...'z', 'A'...'D', 'F'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .float_hex_no_underscore => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        state = .float_hex;
                    },
                    else => {
                        result.tag = .invalid;
                        break;
                    },
                },
                .float_hex => switch (c) {
                    '_' => {
                        state = .float_hex_no_underscore;
                    },
                    'p', 'P' => {
                        state = .float_exponent_unsigned;
                    },
                    '0'...'9', 'a'...'f', 'A'...'F' => {},
                    'g'...'o', 'q'...'z', 'G'...'O', 'Q'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
                .float_exponent_unsigned => switch (c) {
                    '+', '-' => {
                        state = .float_exponent_num_no_underscore;
                    },
                    else => {
                        // reinterpret as a normal exponent number
                        self.index -= 1;
                        state = .float_exponent_num_no_underscore;
                    },
                },
                .float_exponent_num_no_underscore => switch (c) {
                    '0'...'9' => {
                        state = .float_exponent_num;
                    },
                    else => {
                        result.tag = .invalid;
                        break;
                    },
                },
                .float_exponent_num => switch (c) {
                    '_' => {
                        state = .float_exponent_num_no_underscore;
                    },
                    '0'...'9' => {},
                    'a'...'z', 'A'...'Z' => {
                        result.tag = .invalid;
                        break;
                    },
                    else => break,
                },
            }
        }

        if (result.tag == .eof) {
            if (self.pending_invalid_token) |token| {
                self.pending_invalid_token = null;
                return token;
            }
            result.loc.start = self.index;
        }

        result.loc.end = self.index;
        return result;
    }

    fn checkLiteralCharacter(self: *Tokenizer) void {
        if (self.pending_invalid_token != null) return;
        const invalid_length = self.getInvalidCharacterLength();
        if (invalid_length == 0) return;
        self.pending_invalid_token = .{
            .tag = .invalid,
            .loc = .{
                .start = self.index,
                .end = self.index + invalid_length,
            },
        };
    }

    fn getInvalidCharacterLength(self: *Tokenizer) u3 {
        const c0 = self.buffer[self.index];
        if (c0 < 0x80) {
            if (c0 < 0x20 or c0 == 0x7f) {
                // ascii control codes are never allowed
                // (note that \n was checked before we got here)
                return 1;
            }
            // looks fine to me.
            return 0;
        } else {
            // check utf8-encoded character.
            const length = std.unicode.utf8ByteSequenceLength(c0) catch return 1;
            if (self.index + length > self.buffer.len) {
                return @intCast(u3, self.buffer.len - self.index);
            }
            const bytes = self.buffer[self.index .. self.index + length];
            switch (length) {
                2 => {
                    const value = std.unicode.utf8Decode2(bytes) catch return length;
                    if (value == 0x85) return length; // U+0085 (NEL)
                },
                3 => {
                    const value = std.unicode.utf8Decode3(bytes) catch return length;
                    if (value == 0x2028) return length; // U+2028 (LS)
                    if (value == 0x2029) return length; // U+2029 (PS)
                },
                4 => {
                    _ = std.unicode.utf8Decode4(bytes) catch return length;
                },
                else => unreachable,
            }
            self.index += length - 1;
            return 0;
        }
    }
};

test "tokenizer" {
    try testTokenize("pass", &.{.keyword_pass});
}

test "function definition pass" {
    try testTokenize(
        \\def name():
        \\    pass
    , &.{
        .keyword_def,
        .ident,
        .lparen,
        .rparen,
        .colon,
        .keyword_pass,
    });
}

test "list of ints" {
    try testTokenize(
        \\[1,2]
    , &.{
        .lbrack,
        .int,
        .comma,
        .int,
        .rbrack,
    });
}

test "tokenizer code point literal with unicode code point" {
    try testTokenize(
        \\"😀"
    , &.{.string});
}

fn testTokenize(source: [:0]const u8, expected_tokens: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected_tokens) |expected_token_id| {
        const token = tokenizer.next();
        if (token.tag != expected_token_id) {
            std.debug.panic("expected {s}, found {s}\n", .{
                @tagName(expected_token_id), @tagName(token.tag),
            });
        }
    }
    const last_token = tokenizer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.loc.start);
}
