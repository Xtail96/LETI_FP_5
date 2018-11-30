circShiftL :: Int -> [a] -> [a]
circShiftL step list = let step_mod = mod step (length list)
    in drop step_mod list ++ take step_mod list