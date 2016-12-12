is_palindrome :: Int -> Bool
is_palindrome n = reverse (show(n)) == show(n)

largest_palindrome_product :: Int
largest_palindrome_product= largest_palindrome_product' 100 100 1000

largest_palindrome_product' :: Int -> Int -> Int -> Int
largest_palindrome_product' 999 999 prod = prod
largest_palindrome_product' fac1 999 prod = largest_palindrome_product' (fac1+1) 10 prod
largest_palindrome_product' fac1 fac2 prod
  | is_palindrome (fac1 * fac2) && (fac1 * fac2 > prod) = largest_palindrome_product' fac1 (fac2+1) (fac1*fac2)
  | otherwise = largest_palindrome_product' fac1 (fac2+1) prod

solve_problem_4 :: Int
solve_problem_4 = largest_palindrome_product
