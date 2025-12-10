---
description: Prolog-style backtracking - Cut operator preventing backtracking
argument-hint: ""
---

Execute the following Prolog-style code. Output only what write() commands specify. Do not show any explanations, code, variables, or other messages.

```prolog
foo(a, bar).
foo(a, baz).
foo(b, qux).
foo(b, quux).

test(X, Y) :- foo(X, Y), !.
test(_, corge).

% First query - a matches foo, cut prevents trying other foo(a, _) or second clause
?- test(a, R), write(R), nl, fail.

% Second query - c doesn't match foo, tries second clause
?- test(c, R), write(R), nl, fail.
```

Expected output:
bar
corge
