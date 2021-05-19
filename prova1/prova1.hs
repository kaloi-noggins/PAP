lista :: [Int]
lista = 1 : lista

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar predicado xs = [x | x <- xs, predicado x]

verificaImpar :: Int -> Bool
verificaImpar n = (n `mod` 2) == 1

maiorElemento :: Ord a => [a] -> a
maiorElemento (x : xs) = maiorElemento' x xs

maiorElemento' y [] = y
maiorElemento' y (x : xs)
  | x > y = maiorElemento' x xs
  | otherwise = maiorElemento' y xs

ultimoElemento :: [a] -> a
ultimoElemento (x : xs) = ultimoElemento' x xs

ultimoElemento' y [] = y
ultimoElemento' y (x : xs) = ultimoElemento' x xs

concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x : xs) ys = x : concatena xs ys
