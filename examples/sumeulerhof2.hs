-- SumEuler
-- https://github.com/haskell/ThreadScope/blob/master/papers/haskell_symposium_2009/sumEuler/SumEuler0.hs

mkList ## n;
mkList :: Int -> [Int];
mkList n =
    if n < 2
        then []
        else (n - 1) : mkList (n - 1);

-------------------------------------------------------------------------------

relprime ## log x;
relprime :: Int -> Int -> Bool;
relprime x y = gcd x y == 1;

-------------------------------------------------------------------------------

euler ## n;
euler :: Int -> Int;
euler n = length (euler1 n (mkList n));

euler1 ## ls;
euler1 :: Int -> [Int] -> [Int];
euler1 n ls
    = if null ls
        then []
        else if relprime n (head ls)
            then head ls : euler1 n (tail ls)
            else euler1 n (tail ls);

-------------------------------------------------------------------------------

sumEuler ## n;
sumEuler :: Int -> Int;
sumEuler n = sum (map euler (mkList n));
