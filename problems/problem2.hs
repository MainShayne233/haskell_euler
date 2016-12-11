fib_seq_to_nth :: Integer -> [Integer]
fib_seq_to_nth n = fib_seq_to_nth' n  2  [1,2]

last_two_elems_sum :: [Integer] -> Integer
last_two_elems_sum []   = 0
last_two_elems_sum [x]  = x
last_two_elems_sum list = (list !! length list - 2) + (list !! length list - 1)

fib_seq_to_nth' :: Integer -> Integer -> [Integer] -> [Integer]
fib_seq_to_nth' n count list | n < count  = list
                             | otherwise  = fib_seq_to_nth' n (count + 1) (list ++ [last_two_elems_sum list])
