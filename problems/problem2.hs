add_last_two list = list ++ [list !! length list - 1, list !! length list - 2]

fib_sequence_to_nth n = fib_sequence_to_nth n 2 [1,2]
