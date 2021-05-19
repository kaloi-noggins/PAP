dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir f a [] = a
dobrar_dir f a (x:xs) = f x (dobrar_dir f a xs)