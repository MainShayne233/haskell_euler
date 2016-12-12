even_fib_sum_up_to :: Int -> Int
even_fib_sum_up_to n = even_fib_sum_up_to' n 1 1 0

even_fib_sum_up_to' :: Int -> Int -> Int -> Int -> Int
even_fib_sum_up_to' n previous next acc
  | next > n = acc
  | next `rem` 2 == 0 = even_fib_sum_up_to' n next (previous + next) (acc + next)
  | otherwise = even_fib_sum_up_to' n next (previous + next) acc

solve_problem_2 :: Int
solve_problem2 = even_fib_sum_up_to 4000000
