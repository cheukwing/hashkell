-- Matrix multiplication with unwrapped higher order functions
-- https://rosettacode.org/wiki/Matrix_multiplication#Haskell

multiply ## us^3;
multiply :: [[Int]] -> [[Int]] -> [[Int]];
multiply us vs 
  = if null us 
        then [] 
        else mult [] vs (head us) : multiply (tail us) vs;


mult ## zss^2;
mult :: [Int] -> [[Int]] -> [Int] -> [Int];
mult xs zss ys
  = if null zss || null ys
        then xs
        else if null xs
                then mult (mult1 (head ys) (head zss)) (tail zss) (tail ys)
                else mult (mult2 (head ys) xs (head zss)) (tail zss) (tail ys);

mult1 ## zs;
mult1 :: Int -> [Int] -> [Int];
mult1 y zs
  = if null zs
        then []
        else y * head zs : mult1 y (tail zs);

mult2 ## xs;
mult2 :: Int -> [Int] -> [Int] -> [Int];
mult2 y xs zs
  = if null xs || null zs
        then []
        else head xs + head zs * y : mult2 y (tail xs) (tail zs);

