
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
 * Some methods of my parser will advance the token iterator to the end of the parsed expression, some will advance it to the next token after the end of the parsed expression, and it's unclear which does which. I could fix this by using a naming convention (e.g. rename parse_grouped_expression to parse_grouped_expression_and_advance_to_next_token or a better name) or by other means.
 * Error kinds from interpreter mod use variables of the Object type to contain information about mismatching types and operators, which means that they keep information about the evaluated value, whereas only the type should be interesting to them. A way to improve this would be to define an additional enum only for object types (e.g. create ObjectType::Integer for Object::Integer(some_value) and so on) but it would be very cumbersome.
 * Storing EarlyReturnedObject in Object, while very practical to stop interpreting and transmit the encountered return statement up the interpreting chain, is not very elegant, as Object should only be used to represent values. Some other ways to do it would be for eval_* functions to return an enum of either Object or Return (to be defined), or to wrap the Object type in another type that would contain information telling if the evaluation should proceed or if it should stop there (in the case of a return statement).
 * Memory management is prehistoric at best: there is no garbage collection at all in the interpreter, and each time I define or call a function, I integrally clone the environment. I thought of using Weak references to circumvent the difficulties arising from defining a recursive data structure with lifetimes (a function contains an environment which contains functions which contain environments and so on...)