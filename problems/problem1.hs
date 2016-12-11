numbers = [1..999]

multiples = [ x | x <- numbers, x `rem` 3 == 0 || x `rem` 5 == 0]

solve = sum multiples
