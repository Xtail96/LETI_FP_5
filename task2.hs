indices :: [a] -> [(Integer, a)]
indices l = zip [0..] l

--zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]


triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum l1 l2 l3 = zipWith (+) l1 (zipWith (+) l2 l3)