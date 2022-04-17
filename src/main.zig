const std = @import("std");
const print = std.debug.print;
const syntax = @import("./syntax.zig");
const parse = @import("./parse.zig");

pub fn main() anyerror!void {
    const source =
        \\x = 2
        //\\def fib(x):
        //\\  if x < 2:
        //\\    return x
        //\\  return f(x-1) + f(x-2)
    ;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    defer {
        const leaked = gpa.deinit();
        if (leaked) print("leaked", .{});
    }

    var ast = try parse.parse(allocator, source);
    defer ast.deinit(allocator);

    for (ast.nodes.items(.tag)) |nodetag| {
        print("node: {}\n", .{nodetag});
    }

    // walk nodes...

    //var tokenizer = syntax.Tokenizer.init(source);

    //var token = tokenizer.next();
    //while (token.tag != syntax.Token.Tag.invalid and token.tag != syntax.Token.Tag.eof) {
    //    print("token: {}\n", .{token});
    //    token = tokenizer.next();
    //}
    //print("token: {}\n", .{token});

}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
