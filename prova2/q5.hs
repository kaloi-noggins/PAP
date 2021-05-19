primeiro :: (a -> Bool) -> [a] -> Maybe a
primeiro _ [] = Nothing
primeiro f (x:xs)
    | f x = Just x
    | otherwise = primeiro f xs