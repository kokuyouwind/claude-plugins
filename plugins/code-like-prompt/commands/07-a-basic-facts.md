---
description: Prolog-style backtracking - Simple facts and queries
argument-hint: ""
---

Emulate the following Prolog-style code internally (without using external tools or interpreter). Output only what write() commands would output. Do not show any explanations, code, variables, or other messages.

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
