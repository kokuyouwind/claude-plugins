---
description: Prolog-style backtracking - Path finding through multiple clauses
argument-hint: ""
---

Execute the following Prolog-style code. Output only what write() commands specify. Do not show any explanations, code, variables, or other messages.

```prolog
path(a, b).
path(b, c).
path(c, d).
path(a, c).

connected(X, Y) :- path(X, Y).
connected(X, Y) :- path(X, Z), connected(Z, Y).

?- connected(a, X), write(X), write(' '), fail.
```

Expected output (all reachable nodes from 'a'):
b c c d d
