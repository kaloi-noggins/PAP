mapear :: (a -> b) -> [a] -> [b]
mapear f xs = [f x | x <- xs]