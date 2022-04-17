# [wip] starlark-zig

Implementation of starlark in zig, translated from starlark-go.

First zig project, please leave feedback :D.

## devlog

- AST impl https://github.com/ziglang/zig/pull/7920


## ideas

### Parser

Currently the parser is based on the zig parser.
We could use something prebuilt like treesitter: https://github.com/tree-sitter/py-tree-sitter
