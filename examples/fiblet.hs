-- Naive Fibonacci Function using a let expression
-- https://rosettacode.org/wiki/Fibonacci_sequence#Haskell

fib ## 2^x;
fib :: Int -> Int;
fib x =
  if x < 1
    then 0
    else if x < 2
           then 1
           else let
               a = fib (x - 1);
               b = fib (x - 2)
           in a + b
