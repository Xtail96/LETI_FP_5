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