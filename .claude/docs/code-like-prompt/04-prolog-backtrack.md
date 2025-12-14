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

## Test Results

### 2025-12-14 (Claude Sonnet 4.5) - JSON Environment Format

Tests executed with JSON environment argument format (no arguments needed for these tests).

#### Test Results Summary

| Command | Expected Output | Actual Output | Pass |
|---------|-----------------|---------------|------|
| 04p-a-basic-facts | corge<br>grault | corge<br>grault | ✓ |
| 04p-b-multi-clause | b c c d d | b c c d d | ✓ |
| 04p-c-cut | bar<br>corge | bar<br>corge | ✓ |
| 04p-d-tree-traverse | baz qux corge grault | baz qux corge grault | ✓ |
| 04p-e-findall | [baz, quux] | [baz, quux] | ✓ |
| 04p-f-negation | bar | bar | ✓ |
| 04p-g-constraints | foo-bar-baz<br>foo-baz-bar<br>bar-foo-baz<br>bar-baz-foo<br>baz-foo-bar<br>baz-bar-foo | foo-bar-baz<br>foo-baz-bar<br>bar-foo-baz<br>bar-baz-foo<br>baz-foo-bar<br>baz-bar-foo | ✓ |

**Pass Rate: 7/7 (100%)**

#### Detailed Analysis

**04p-a-basic-facts**: Perfect. Claude correctly:
- Queries the fact database
- Unifies variables across multiple predicates
- Backtracks to find all solutions
- Outputs only matching results (corge, grault)

**04p-b-multi-clause**: Perfect. Claude correctly:
- Uses multiple clauses for the same predicate
- Implements recursive rules (`connected(X, Y) :- path(X, Z), connected(Z, Y)`)
- Finds all reachable nodes through depth-first search
- Outputs solutions in DFS order (b c c d d)

**04p-c-cut**: Perfect. Claude correctly:
- Understands cut (!) semantics
- Commits to first matching clause when cut is executed
- Prevents backtracking to alternative clauses after cut
- Falls through to second clause when first clause fails entirely

**04p-d-tree-traverse**: Perfect. Claude correctly:
- Recursively traverses tree structures
- Processes nodes in depth-first order (left subtree before right)
- Collects leaf values through backtracking
- Outputs: baz qux corge grault (DFS order)

**04p-e-findall**: Perfect. Claude correctly:
- Collects all solutions without manual backtracking
- Filters by predicate conditions (foo(X, 2))
- Returns list of matching values: [baz, quux]

**04p-f-negation**: Perfect. Claude correctly:
- Implements negation as failure (`\+`)
- Only outputs values that satisfy foo(X) but not qux(X)
- Correctly outputs: bar (satisfies foo(bar) and not qux(bar))

**04p-g-constraints**: Perfect. Claude correctly:
- Generates all possible combinations
- Applies inequality constraints (R1 ≠ R2, R2 ≠ R3, R3 ≠ R1)
- Eliminates invalid colorings during backtracking
- Outputs all 6 valid 3-colorings of a triangle

### Key Findings

1. **Perfect Prolog interpretation**: Claude demonstrates flawless understanding of logic programming concepts
2. **Correct backtracking behavior**: All backtracking patterns work exactly as expected
3. **Cut semantics understood**: The cut operator (!) correctly prevents backtracking
4. **Recursive rules work**: Complex recursive predicates are properly evaluated
5. **Constraint satisfaction works**: Inequality constraints are correctly applied during search
6. **DFS order preserved**: Solutions are found in depth-first search order
7. **Negation as failure works**: `\+` correctly succeeds when goal cannot be proven
8. **Output format perfect**: No explanatory text added - pure output only

### Comparison with Other Test Series

04p series achieved **100% pass rate**, significantly better than:
- 01 series: 50% (condition evaluation issues)
- 02 series: 76% (nested conditional issues)
- 03 series: 67% (loop boundary and state issues)
- 04 series: 83% (output format violations)

### Why Prolog Tests Succeeded

Several factors likely contribute to the perfect pass rate:

1. **Declarative nature**: Prolog is declarative rather than imperative, which may align better with Claude's reasoning
2. **Pattern matching focus**: Prolog heavily uses pattern matching, which Claude handles excellently (as shown in 04 series)
3. **No mutable state**: Prolog avoids mutable state, eliminating state-tracking issues seen in loop tests
4. **Clear semantics**: Backtracking and unification have well-defined semantics that Claude appears to model accurately
5. **No complex nesting issues**: While recursive, Prolog rules don't have the scope ambiguity problems of nested conditionals

### Conclusion

Claude demonstrates **exceptional capability** in interpreting Prolog-style logic programming:
- Perfect understanding of unification and backtracking
- Accurate implementation of cut, negation, and constraint satisfaction
- Correct depth-first search ordering
- No output format violations

This suggests that **declarative, pattern-matching-based paradigms** may be more naturally suited to Claude's interpretation capabilities than imperative, state-based programming with nested conditionals.

**Recommendation**: For complex code-like prompts requiring multiple solutions or exhaustive search, consider using Prolog-style declarative patterns rather than imperative loops and nested conditionals.
