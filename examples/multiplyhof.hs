
multiply ## us^3;
multiply :: [[Int]] -> [[Int]] -> [[Int]];
multiply us vs = map (mult [] vs) us;

mult ## zss^2;
mult :: [Int] -> [[Int]] -> [Int] -> [Int];
mult xs zss ys
    = if null zss || null ys
        then xs
        else if null xs
            then mult (map (mul (head ys)) (head zss)) (tail zss) (tail ys)
            else mult (zipWith (agg (head ys)) xs (head zss)) (tail zss) (tail ys);

mul ## 1;
mul a b = a * b;

agg ## 2;
agg a b c = b + c * a