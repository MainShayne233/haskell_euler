sum_of_last_two :: [Int] -> Int -> Int
sum_of_last_two [x,y] 2 = x + y
sum_of_last_two list l = (list !! (l - 2)) + (list !! (l - 1))

fib_seq_to_max :: Int -> [Int]
fib_seq_to_max n = fib_seq_to_max' (n, 3, 2) [1,2]

fib_seq_to_max' :: (Int, Int, Int) -> [Int] -> [Int]
fib_seq_to_max' (n, m, l) list
  | m > n = list
  | otherwise = fib_seq_to_max' ( n, (sum_of_last_two (list ++ [m]) (l + 1)), (l + 1) ) (list ++ [m])

even_fib_numbers :: [Int]
even_fib_numbers = [x | x <- fib_seq_to_max 4000000, x `rem` 2 == 0]

solve_problem_1 :: Int
solve_problem_2 solve = sum even_fib_numbers
