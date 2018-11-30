newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (SortedList a) where 
    mempty = SortedList []
    
instance Ord a => Semigroup (SortedList a) where 
    (SortedList sl1) <> (SortedList sl2) = SortedList (mergeSortedLists sl1 sl2)

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists sl [] = sl
mergeSortedLists [] sl = sl
mergeSortedLists (i1 : sl1) (i2 : sl2) = 
    if i1 <= i2 
    then i1 : mergeSortedLists sl1 (i2 : sl2)
    else i2 : mergeSortedLists (i1 : sl1) sl2

msort :: Ord a => [a] -> SortedList a
msort [] = mempty
msort [x] = SortedList [x]
msort list = mappend (msort part1) (msort part2) where
    (part1, part2) = splitAt (div (length list) 2) list 