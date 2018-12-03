import Data.List

-- task1
circShiftL :: Int -> [a] -> [a]
circShiftL step list
    | (length list) > 0 =
        let step_mod = mod step (length list) in
             drop step_mod list ++ take step_mod list
    | otherwise = []

-- task2

indices :: [a] -> [(Integer, a)]
indices l = zip [0..] l

zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy list predicate = map (\a -> if predicate a then a else mempty) list

triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum l1 l2 l3 = zipWith (+) l1 (zipWith (+) l2 l3)

-- task3

revRange :: (Char, Char) -> [Char]
revRange = unfoldr fun
fun (first, last)
    | first > last = Nothing
    | last == minBound = Just (last, (succ first, last))
    | otherwise = Just (last, (first, pred last))

-- task4

seriesK :: Int -> [Rational]
seriesK k = let r_k = toRational k in knot (r_k, 0)

knot :: (Rational, Int) -> [Rational]
knot (x, s)  = 1 / y : result_list
    where y = x ^ s
          result_list = knot (x, s + 1)

-- task5

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

-- task6

msort :: Ord a => [a] -> SortedList a
msort [] = mempty
msort [x] = SortedList [x]
msort list = mappend (msort part1) (msort part2) where
    (part1, part2) = splitAt (div (length list) 2) list 

-- task7

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Tree where
    foldMap f Nil = mempty
    foldMap f (Node left node right) = (foldMap f left) <> f node <> (foldMap f right)
    
--Pre-order (NLR) Node - Left - Right
instance Foldable Preorder where
    foldMap f (PreO Nil) = mempty
    foldMap f (PreO (Node left node right)) = f node <> (foldMap f (PreO left)) <> (foldMap f (PreO right))

--Post-order (LRN) Left - Right - Node
instance Foldable Postorder where
    foldMap f (PostO Nil) = mempty
    foldMap f (PostO (Node left node right)) = (foldMap f (PostO left)) <> (foldMap f (PostO right)) <> f node 

instance Foldable Levelorder where
    foldMap f tree = foldMap_ f (tbf [tree]) where --добавляем в очередь узел
        foldMap_ f [] = mempty
        foldMap_ f (x:xs) = f x <> foldMap_ f xs
--функция создает список вершин при обходе дерева в порядке LevelOrder
        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs)) where --map nodeValue xs значения узлов этого уровня добавляются в результирующий список
        --объединяем с другими узлами а затем рекурсивно вызываем tbf, чтобы пройти все уровни дерева.
        --LeftAndRightNodes возвращает список левого и / или правого узлов.
            nodeValue (LevelO (Node _ a _)) = a
            leftAndRightNodes (LevelO (Node Nil _ Nil)) = []
            leftAndRightNodes (LevelO (Node Nil _ b))   = [LevelO b]
            leftAndRightNodes (LevelO (Node a _ Nil))   = [LevelO a]
            leftAndRightNodes (LevelO (Node a _ b))     = [LevelO a, LevelO b]

treeExample = Node (Node (Node Nil [4] Nil) [2] (Node Nil [5] Nil)) [1] (Node Nil [3] Nil) -- дерево взято с курса на Stepik.org

preTree = PreO (treeExample)

posTree = PostO (treeExample)

levelTree = LevelO (treeExample)

nodeSum fold = sum(foldMap id fold)
