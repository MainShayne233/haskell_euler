is_prime :: Int -> Bool
is_prime 5 = True
is_prime n
  | n <= 3          = n >= 2
  | 30 `gcd` n == 1 = is_prime' n 7 (floor(sqrt(fromIntegral(n))))
  | otherwise = False

is_prime' :: Int -> Int -> Int -> Bool
is_prime' n inc sqrt_n
  | inc > sqrt_n = True
  | n `rem` inc      == 0 || n `rem` (inc+4)  == 0 || n `rem` (inc+6)  == 0 ||
    n `rem` (inc+10) == 0 || n `rem` (inc+12) == 0 || n `rem` (inc+16) == 0 ||
    n `rem` (inc+22) == 0 || n `rem` (inc+24) == 0 = False
  | otherwise = is_prime' n (inc + 7) sqrt_n

next_prime :: Int -> Int
next_prime n
  | n < 2          = 2
  | n `rem` 2 == 0 = next_prime' (n+1)
  | otherwise      = next_prime' (n+2)

next_prime' :: Int -> Int
next_prime' n
  | is_prime n = n
  | otherwise  = next_prime' (n + 2)

largest_prime_factor :: Int -> Int
largest_prime_factor n
  | is_prime n = n
  | otherwise  = largest_prime_factor' 2 n

largest_prime_factor' :: Int -> Int -> Int
largest_prime_factor' prime 1 = prime
largest_prime_factor' prime n
  | n `rem` prime == 0 = largest_prime_factor' prime (n `div` prime)
  | otherwise          = largest_prime_factor' (next_prime prime) n

solve_problem_3 :: Int
solve_problem_3 = largest_prime_factor 600851475143
