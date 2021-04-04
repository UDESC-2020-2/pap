concatenacao :: [a] -> [a] -> [a]
-- base
concatenacao [] ys = ys
-- recursivo
concatenacao (x : xs) ys =
  x : concatenacao xs ys

pertence :: Eq a => a -> [a] -> Bool
-- base
pertence x [] = False
-- recursivo
pertence x (y : ys)
  | x == y = True
  | otherwise = pertence x ys

intersecao :: Eq a => [a] -> [a] -> [a]
-- base
intersecao x [] = []
intersecao [] y = []
-- recursivo
intersecao (x : xs) y
  | pertence x y = x : intersecao xs y
  | otherwise = intersecao xs y

inverso :: [a] -> [a]
-- base
inverso [] = []
-- recursivo
inverso (x : xs) =
  concatenacao (inverso xs) [x]

primeiros :: Int -> [a] -> [a]
-- base
primeiros n [] = []
primeiros 0 _ = []
-- recursivo
primeiros n (x : xs) =
  x : primeiros (n -1) xs

ultimos :: Int -> [a] -> [a]
-- base
ultimos n [] = []
ultimos 0 _ = []
-- recursivo
ultimos n x =
  inverso (primeiros n (inverso x))

binParaInt :: String -> Int
-- base
binParaInt "0" = 0
binParaInt "1" = 1
-- recursivo
binParaInt ('0' : xs) =
  binParaInt xs
binParaInt ('1' : xs) =
  2 ^ length xs + binParaInt xs
binParaInt xs =
  error "Valor nao é binario"

intParaBin :: Int -> String
-- base
intParaBin 0 = "0"
intParaBin 1 = "1"
intParaBin n
  | even n = concatenacao (intParaBin (div n 2)) "0"
  | otherwise = concatenacao (intParaBin (div n 2)) "1"

menorValor :: Ord a => [a] -> a
-- base
menorValor [x] = x
-- recursivo
menorValor (x : y : xs)
  | x < y = menorValor $ x : xs
  | otherwise = menorValor $ y : xs
-- erro
menorValor [] = error "Lista nao pode ser vazia"

removerPrimeiro :: Eq a => [a] -> a -> [a]
-- base
removerPrimeiro [] _ = []
-- recursivo
removerPrimeiro (x : xs) y
  | x == y = xs
  | otherwise = concatenacao [x] (removerPrimeiro xs y)

ordenar :: Ord a => [a] -> [a]
-- base
ordenar [x] = [x]
-- recursivo
ordenar xs =
  menorValor xs : ordenar (removerPrimeiro xs (menorValor xs))

dobrarDir :: (a -> b -> b) -> b -> [a] -> b
--base
dobrarDir f x [] =
  x
-- recursivo
dobrarDir f x (y : ys) =
  f y (dobrarDir f x ys)

dobrarEsq :: (b -> a -> b) -> b -> [a] -> b
--base
dobrarEsq f x [] =
  x
--recursivo
dobrarEsq f x (y : ys) =
  dobrarEsq f (f x y) ys

filtrar :: (a -> Bool) -> [a] -> [a]
--base
filtrar _ [] =
  []
-- recursivo
filtrar f (x : xs)
  | f x = x : filtrar f xs
  | otherwise = filtrar f xs

verificaImpar :: Int -> Bool
verificaImpar n = (n `mod` 2) == 1

impares :: [Int] -> [Int]
impares = filtrar verificaImpar

mapear :: (a -> b) -> [a] -> [b]
-- base
mapear f [x] = [f x]
-- recursivo
mapear f (x : xs) =
  f x : mapear f xs

primeiros2 :: [(a, b)] -> [a]
-- base
primeiros2 [(a, b)] = [a]
-- recursivo
primeiros2 ((a, b) : xs) =
  a : primeiros2 xs

todos :: [Bool] -> Bool
-- base
todos [x] = x
-- recursivo
todos x = dobrarDir verificaVerdade True x

verificaVerdade :: Bool -> Bool -> Bool
verificaVerdade x y = x && y

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

maior2 :: Ord a => Tree a -> a
-- base
maior2 (Leaf x) = x
-- recursivo
maior2 (Branch x y)
  | maior2 x > maior2 y = maior2 x
  | otherwise = maior2 y

altura :: Tree a -> Int
-- base
altura (Leaf x) = 0
-- recursivo
altura (Branch x y) = 1 + max (altura x) (altura y)