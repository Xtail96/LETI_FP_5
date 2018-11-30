indices :: [a] -> [(Integer, a)]
indices l = zip [0..] l

zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy list predicate = map (\a -> if predicate a then mempty else a) list

triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum l1 l2 l3 = zipWith (+) l1 (zipWith (+) l2 l3)