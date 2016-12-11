numbers :: [Integer]
numbers = [1..999]

multiples :: [Integer]
multiples = [ x | x <- numbers, x `rem` 3 == 0 || x `rem` 5 == 0]

solve :: Integer
solve = sum multiples
