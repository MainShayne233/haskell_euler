numbers :: [Int]
numbers = [1..999]

multiples :: [Int]
multiples = [ x | x <- numbers, x `rem` 3 == 0 || x `rem` 5 == 0]

solve_problem_1 :: Int
solve_problem_1 = sum multiples
