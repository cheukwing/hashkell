# simple-haskell

Parser for a simple subset of Haskell with time complexity annotations.

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
