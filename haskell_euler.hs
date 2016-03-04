is_div x y = x `mod` y == 0

i (a,b,c) x = if x == 1 then a else if x == 2 then b else if x == 3 then c else 0

prob1 = sum [x | x <- [1..999], x `is_div` 3 || x `is_div` 5]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

right_triangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

good_triangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

factorial x = product [1..x]

next_fib list = list ++ [list !! (length list - 1) + list !! (length list -2)]

fib ls n = if last (next_fib ls) < n then fib (next_fib ls) n else ls

even_fibs = [x | x <- fib [1,1] (4*10^6), x `mod` 2 == 0]

prob2 = sum even_fibs

is_prime :: Int -> Bool

is_prime n = if n == 1 then False else sum [x | x <- [2..floor(sqrt (fromIntegral n :: Double))], n `mod` x == 0] == 0

smallest_fact :: Int -> Int
smallest_fact n = if is_prime n then n else smallest_fact (head ([x | x <- [2..floor(sqrt (fromIntegral n :: Double))], n `mod` x == 0]))

div_by_sf :: Int -> Int
div_by_sf n = n `div` (smallest_fact n)

big_prime_fact :: Int -> Int
big_prime_fact n = if is_prime n then n else big_prime_fact (div_by_sf n)

prob3 = big_prime_fact 600851475143
