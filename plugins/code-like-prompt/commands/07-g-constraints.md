---
description: Prolog-style backtracking - Graph coloring with constraint propagation
argument-hint: ""
---

Emulate the following Prolog-style code internally (without using external tools or interpreter). Output only what write() commands would output. Do not show any explanations, code, variables, or other messages.

```prolog
color(foo).
color(bar).
color(baz).

adjacent(1, 2).
adjacent(2, 3).
adjacent(3, 1).

coloring(R1, R2, R3) :-
    color(R1), color(R2), color(R3),
    adjacent(1, 2), R1 \= R2,
    adjacent(2, 3), R2 \= R3,
    adjacent(3, 1), R3 \= R1.

?- coloring(A, B, C), write(A), write('-'), write(B), write('-'), write(C), nl, fail.
```
