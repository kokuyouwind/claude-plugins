---
description: Prolog-style backtracking - Collecting all solutions with findall
argument-hint: ""
---

Emulate the following Prolog-style code internally (without using external tools or interpreter). Output only what write() commands would output. Do not show any explanations, code, variables, or other messages.

```prolog
foo(bar, 1).
foo(baz, 2).
foo(qux, 3).
foo(quux, 2).

?- findall(X, foo(X, 2), L), write(L), nl.
```
