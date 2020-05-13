# hashkell

Parser for a simple subset of Haskell with time complexity annotations and dependency graphs.

## Compilation, running, and testing

This project uses [Stack](https://docs.haskellstack.org/en/stable/README/):

```bash
stack build # to build

stack run -- <args> # or ./hashkell-exe <args>, to run with <args>

stack test # to test
```

## Usage

To parallelise an input file `code.hs` with the _function-only_ strategy:

```bash
stack run -- parallelise function code.hs
```

To graph all the functions in an input file `code.hs`:

```bash
stack run -- graph -A code.hs
```

For all options, refer to the help manual:

```bash
stack run -- --help
```

## Example of Supported Code

```haskell
fib ## 2^x;
fib :: Int -> Int;
fib x =
  if x < 1
    then 0
    else if x < 2
           then 1
           else fib (x - 1) + fib (x - 2)

```

### Generated Dependency Graph

![Dependency graph for the naive fib function](/imgs/naivefib.svg "Dependency Graph")

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

-- Factorial time
func ## fac n;
```

The given identifier name in the annotation must correspond to an argument in the function declaration, and only one such name is allowed in any annotation.
