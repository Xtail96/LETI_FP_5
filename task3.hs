import Data.List

revRange :: (Char, Char) -> [Char]
revRange = unfoldr fun
fun (first, last)
    | first > last = Nothing
    | last == minBound = Just (last, (succ first, last))
    | otherwise = Just (last, (first, pred last))