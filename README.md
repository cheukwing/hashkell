# simple-haskell

Parser for a simple subset of Haskell with time complexity annotations and dependency graphs.

## Example of Supported Code

```haskell
fib ## 2^n;
fib n =
    if n == 0
        then 1
        else if n == 1
            then 1
            else let {
                a = fib (n - 1);
                b = fib (n - 2);
            } in a + b;
```

### Generated Dependency Graph

![Dependency graph for the naive fib function](/imgs/naivefibdefgraph.svg "Dependency Graph")

## Time Complexity Annotations

Time complexity annotations are used by the compiler to generate branching paths for parallelisation.
Annotations are written using `##`, as so:

```haskell
<function-name> ## <time-complexity>;
```

The following annotations are supported:

```haskell
-- Constant time
func ## 1;

-- Polynomial time
func ## n;

func ## n^2;

func ## n^100;

-- Exponential time
func ## 2^n;

func ## 100^n;

-- Logarithmic time
func ## log n;
```

The given identifier name in the annotation must correspond to an argument in the function declaration, and only one such name is allowed in any annotation.
