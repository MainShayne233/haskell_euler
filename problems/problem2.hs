fib_seq_to_max :: Int -> [Int]
fib_seq_to_max n = fib_seq_to_max' (n, 3, 2) [1,2]

fib_seq_to_max' :: (Int, Int, Int) -> [Int] -> [Int]
fib_seq_to_max' (n, m, l) list
  | m > n = list
  | otherwise = fib_seq_to_max' (n, m + list !! (l - 1), l + 1) (list ++ [m])

even_fib_numbers :: [Int]
even_fib_numbers = [x | x <- fib_seq_to_max 4000000, x `rem` 2 == 0]

solve_problem_2 :: Int
solve_problem_2 = sum even_fib_numbers
