
# tempe-lang

_tempe-lang_ is a toy project started after purchasing the book __Writing An Interpreter In Go__ from _Thorsten Ball_.
I have two goals in doing this project: learning how interpreters work, and practicing programming in Rust.

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

