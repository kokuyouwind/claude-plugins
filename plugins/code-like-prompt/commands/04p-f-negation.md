---
description: Prolog-style backtracking - Negation as failure (\+)
argument-hint: ""
---

Execute the following Prolog-style code. Output only what write() commands specify. Do not show any explanations, code, variables, or other messages.

```prolog
foo(bar).
foo(baz).
qux(baz).

test(X) :- foo(X), \+ qux(X).

?- test(X), write(X), nl, fail.
```

Expected output:
bar
