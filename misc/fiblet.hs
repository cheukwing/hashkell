fib ## 2^n;
fib :: Int -> Int;
fib n = 
    if n == 0
        then 1
        else if n == 1
            then 1
            else let {
                a = fib (n - 1);
                b = fib (n - 2);
            } in a + b;
