sayAmount n =
    case n of
        0 -> "Zero"
        1 -> "One"
        2 -> "Two"
        _ -> "A lot"

sayAmount_v2 0 = "Zero"
sayAmount_v2 1 = "One"
sayAmount_v2 2 = "Two"
sayAmount_v2 _ = "A lot"

isEmpty [] = True
isEmpty _ = False

myHead (x : _) = x
myHead [] = error "Error: empty list"

myTail (_ : xs) = xs
myTail [] = error "Error: empty list"