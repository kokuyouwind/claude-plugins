---
description: Prolog-style backtracking - Cut operator preventing backtracking (first solution only)
argument-hint: ""
---

Emulate the following Prolog-style code internally (without using external tools or interpreter). Output only what write() commands would output. Do not show any explanations, code, variables, or other messages.

```prolog
foo(a, bar).
foo(a, baz).
foo(b, qux).
foo(b, quux).

test(X, Y) :- foo(X, Y), !.
test(_, corge).

?- test(a, R), write(R), nl.

?- test(c, R), write(R), nl.
```
