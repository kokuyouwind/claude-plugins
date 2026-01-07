---
description: Prolog-style backtracking - Negation as failure (\+)
argument-hint: ""
---

Emulate the following Prolog-style code internally (without using external tools or interpreter). Output only what write() commands would output. Do not show any explanations, code, variables, or other messages.

```prolog
foo(bar).
foo(baz).
qux(baz).

test(X) :- foo(X), \+ qux(X).

?- test(X), write(X), nl, fail.
```
