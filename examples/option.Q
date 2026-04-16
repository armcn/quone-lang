# Option type with a map function


type Option a
    <- None
    | Some a


inc : Integer -> Integer
inc x <-
    x + 1


map_option : (Integer -> Integer) -> Option Integer -> Option Integer
map_option f opt <-
    case opt of
        None ->
            None

        Some x ->
            Some (f x)


result : Option Integer
result <-
    map_option inc (Some 41)
