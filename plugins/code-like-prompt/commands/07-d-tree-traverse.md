---
description: Prolog-style backtracking - Recursive tree traversal with DFS order
argument-hint: ""
---

Emulate the following Prolog-style code internally (without using external tools or interpreter). Output only what write() commands would output. Do not show any explanations, code, variables, or other messages.

```prolog
% Tree structure: node(Value, Left, Right) or leaf(Value)
tree(node(foo,
         node(bar, leaf(baz), leaf(qux)),
         node(quux, leaf(corge), leaf(grault)))).

% Traverse and collect all leaf values
leaf_value(leaf(V), V).
leaf_value(node(_, L, _), V) :- leaf_value(L, V).
leaf_value(node(_, _, R), V) :- leaf_value(R, V).

?- tree(T), leaf_value(T, V), write(V), write(' '), fail.
```
