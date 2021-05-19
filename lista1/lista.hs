import Data.Char

concatenacao :: [a] -> [a] -> [a]
concatenacao [] [] = []
concatenacao (x : xs) [] = x : concatenacao xs []
concatenacao [] (y : ys) = y : concatenacao [] ys
concatenacao (x : xs) (y : ys) = x : y : concatenacao xs ys

pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
pertence a (x : xs)
  | x /= a = pertence a xs
  | x == a = True

intersecao :: Eq a => [a] -> [a] -> [a]
intersecao xs ys = [x | x <- xs, y <- ys, x == y]

inverso :: [a] -> [a]
inverso [] = []
inverso (x : xs) = inverso xs ++ [x]

primeiros :: Int -> [a] -> [a]
primeiros n [] = []
primeiros n (x : xs)
  | n == 0 = []
  | otherwise = x : primeiros (n -1) xs

ultimos :: Int -> [a] -> [a]
ultimos n [] = []
ultimos n xs
  | n == 0 = []
  | otherwise = elem : ultimos (n -1) list
  where
    elem = last xs
    list = init xs

binParaInt :: String -> Int
binParaInt [] = 0
binParaInt (x : xs) = digito * 2 ^ length xs + binParaInt xs
  where
    digito = digitToInt x

--binParaInt s = sum ([x * 2 ^ length digitos | x <- digitos])
--  where
--    digitos = map digitToInt s