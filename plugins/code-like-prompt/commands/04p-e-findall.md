---
description: Prolog-style backtracking - Collecting all solutions with findall
argument-hint: ""
---

Execute the following Prolog-style code. Output only what write() commands specify. Do not show any explanations, code, variables, or other messages.

```prolog
foo(bar, 1).
foo(baz, 2).
foo(qux, 3).
foo(quux, 2).

?- findall(X, foo(X, 2), L), write(L), nl.
```

Expected output:
[baz, quux]
