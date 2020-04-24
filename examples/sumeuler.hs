-- SumEuler with unwrapped higher order functions
-- https://github.com/haskell/ThreadScope/blob/master/papers/haskell_symposium_2009/sumEuler/SumEuler0.hs

mkList ## n;
mkList :: Int -> [Int];
mkList n =
    if n < 2
        then []
        else (n - 1) : mkList (n - 1);

relprime ## log x;
relprime :: Int -> Int -> Bool;
relprime x y = gcd x y == 1;

euler ## n;
euler :: Int -> Int;
euler n = length (euler1 n (mkList n));

euler1 ## ls;
euler1 :: Int -> [Int] -> [Int];
euler1 n ls
    = if null ls
        then []
        else let
            h = head ls;
            v = relprime n h;
            rest = euler1 n (tail ls)
        in if v
            then h : rest
            else rest;

sumEuler ## n;
sumEuler :: Int -> Int;
sumEuler n = sum (sumEuler1 (mkList n));

sumEuler1 ## ls;
sumEuler1 :: [Int] -> [Int];
sumEuler1 ls
    = if null ls
        then []
        else euler (head ls) : sumEuler1 (tail ls);
