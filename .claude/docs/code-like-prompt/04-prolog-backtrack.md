# 04-prolog-backtrack Specification

## Concept

Test Claude's ability to interpret Prolog-style logic programming constructs, focusing on:
1. Unification and pattern matching
2. Backtracking on failure
3. Multiple solutions (findall)
4. Cut operator (!) to prevent backtracking
5. Tree traversal with recursive rules

## Test Scenarios

### 04p-a: Basic Facts and Queries

Test simple fact database and queries.

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

Expected behavior:
- foo(X) matches bar → quux(bar, Y) matches corge → outputs "corge"
- backtrack → foo(X) matches baz → quux(baz, Y) matches grault → outputs "grault"
- backtrack → foo(X) matches qux → quux(qux, Y) fails → backtrack
- no more foo(X) solutions → query fails

Output: corge, grault

### 04p-b: Backtracking with Multiple Clauses

Test backtracking through alternative rule definitions.

```prolog
path(a, b).
path(b, c).
path(c, d).
path(a, c).

connected(X, Y) :- path(X, Y).
connected(X, Y) :- path(X, Z), connected(Z, Y).

?- connected(a, X), write(X), write(' '), fail.
```

Expected: All reachable nodes from 'a' via backtracking.
- a→b (direct)
- a→c (direct)
- a→b→c (via b)
- a→c→d (via c)
- a→b→c→d (via b, c)

Output should include: b c c d d (order depends on search order)

### 04p-c: Cut Operator

Test cut (!) preventing backtracking.

```prolog
foo(a, bar).
foo(a, baz).
foo(b, qux).
foo(b, quux).

test(X, Y) :- foo(X, Y), !.
test(_, corge).

?- test(a, R), write(R), nl, fail.
?- test(c, R), write(R), nl, fail.
```

Expected:
- test(a, R): foo(a, R) succeeds with bar, cut prevents backtracking to baz or corge → "bar"
- test(c, R): foo(c, R) fails, tries second clause → "corge"

### 04p-d: Tree Structure Traversal

Test recursive tree traversal with backtracking.

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

Expected: All leaf values in DFS order
Output: baz qux corge grault

### 04p-e: Findall - Collecting All Solutions

Test collecting all solutions without manual backtracking.

```prolog
foo(bar, 1).
foo(baz, 2).
foo(qux, 3).
foo(quux, 2).

?- findall(X, foo(X, 2), L), write(L), nl.
```

Expected: [baz, quux]

### 04p-f: Negation as Failure

Test `\+` (not provable) operator.

```prolog
foo(bar).
foo(baz).
qux(baz).

test(X) :- foo(X), \+ qux(X).

?- test(X), write(X), nl, fail.
```

Expected: bar (only bar satisfies foo(X) and not qux(X))

### 04p-g: Complex Backtracking with Constraints

Test backtracking with accumulating constraints.

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

Expected: All valid 3-colorings of a triangle
- foo-bar-baz, foo-baz-bar, bar-foo-baz, bar-baz-foo, baz-foo-bar, baz-bar-foo

## Command Design

### Arguments

- `--query`: The Prolog query to execute
- `--facts`: Additional facts to add to the knowledge base (for dynamic testing)

### Variants

| Command | Focus |
|---------|-------|
| `04p-a-basic-facts` | Simple facts and queries |
| `04p-b-multi-clause` | Backtracking through multiple clauses |
| `04p-c-cut` | Cut operator behavior |
| `04p-d-tree-traverse` | Recursive tree traversal |
| `04p-e-findall` | Collecting all solutions |
| `04p-f-negation` | Negation as failure |
| `04p-g-constraints` | Graph coloring with constraints |

## Expected Behaviors

1. **Unification**: Variables unify with terms correctly
2. **Backtracking**: On failure, previous choice points are retried
3. **Cut**: Prevents backtracking past the cut point
4. **DFS order**: Solutions found in depth-first search order
5. **Negation**: `\+` succeeds if goal cannot be proven

## Key Test Points

### Backtracking Order (04p-a, 04p-b)
Solutions should appear in the order determined by clause ordering and DFS traversal.

### Cut Semantics (04p-c)
- Cut commits to current choice for the clause containing the cut
- Alternative clauses for the same predicate are not tried after cut
- Cut does NOT affect callers

### Tree Traversal Order (04p-d)
Left subtree fully explored before right subtree (DFS).

### Constraint Propagation (04p-g)
Tests whether Claude understands that constraints eliminate invalid combinations during backtracking.

## Implementation Priority

1. `04p-d-tree-traverse` - Core backtracking on recursive structures
2. `04p-c-cut` - Tests understanding of cut semantics
3. `04p-g-constraints` - Complex constraint satisfaction
4. `04p-b-multi-clause` - Path finding with backtracking
5. `04p-e-findall` - Solution collection
6. `04p-f-negation` - Negation as failure
7. `04p-a-basic-facts` - Baseline verification
