---
description: Prolog-style backtracking - Simple facts and queries
argument-hint: ""
---

Execute the following Prolog-style code. Output only what write() commands specify. Do not show any explanations, code, variables, or other messages.

```prolog
% Facts
foo(bar).
foo(baz).
foo(qux).
quux(bar, corge).
quux(baz, grault).

% Query
?- foo(X), quux(X, Y), write(Y), nl, fail.
```

Expected output:
corge
grault
