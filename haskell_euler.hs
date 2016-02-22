is_div x y = x `mod` y == 0

prob1 = sum [x | x <- [1..999], x `is_div` 3 || x `is_div` 5] 
