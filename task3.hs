import Data.List

revRange :: (Char, Char) -> [Char]
revRange (a, b) = unfoldr fun (b, False)
    where fun (x, isEnd) 
        | x < a || isEnd = Nothing
        | x > a = Just (x, (pred x, False))
        | x == a = Just (x, (x, True))