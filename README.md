
# tempe-lang

_tempe-lang_ is a toy project started after purchasing the book __Writing An Interpreter In Go__ from _Thorsten Ball_.
I have two goals in doing this project: learning how parsers and interpreters work, and practicing programming in Rust.

# The language

_tempe-lang_ is very similar to the _Monkey_ programming language described in the book, except that its keywords do not come from English but from Indonesian (because it's more fun!).
It has keywords such as __fungsi__ (to declare functions), __diketahui__ (to bind a value to a variable name), __kembalikan__ (to return a value from a function), and others.
Apart from that, it's mostly a C-like language.

Why naming it _tempe-lang_? Because [tempe](https://en.wikipedia.org/wiki/Tempeh) is delicious!

An example of _tempe-lang_ code:

```
diketahui twice = fn(f, x) {
  kembalikan f(f(x));
};
```

# Disclaimer

I'm aware that the source code of tempe-lang is ugly, bloated and that there is a lot of room for improvements. My objective was to get more used to writing Rust code and especially code dealing with lifetimes, and to write my first parser and interpreter. Writing elegant and concise code will be my goal for another project ;).

A non-exhaustive list of what I could improve:
 * I've hesitated a lot between using traits or enums to represent my AST and objects. In the end I went with enums all over, but I'm still not completely sure it was the best choice in all cases.
 * My unit tests in some modules (e.g. parser) are actually more integration tests than unit tests (I'm using the Lexer in the Parser tests, if I wanted to really do unit-testing, I should write myself lists of tokens but it would take me a lot of time).
 * Some methods of my parser will advance the token iterator to the end of the parsed expression, some will advance it to the next token after the end of the parsed expression, and it's unclear which does which. I could fix this by using a naming convention (e.g. rename parse_grouped_expression to parse_grouped_expression_and_advance_to_next_token or a better name) or by other means.
 * I could extend my Lexer to add context informations to each token (e.g. add its line and column numbers, to make error messages more helpful)
