# simple-haskell

```haskell
fib n = let {
    a = fib (n - 1);
    b = fib (n - 2);
} in a + b
```
