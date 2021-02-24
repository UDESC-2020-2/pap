{-
  Funções de dobra: colapsam uma lista em um único valor, associando uma
  operação binária pela direita ou pela esquerda (notem os parênteses).
-}

dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir f x [] =
  x
dobrar_dir f x (y:ys) =
  -- TODO: reimplementar sem foldr!
  foldr f x (y:ys)

--
-- dobrar_dir (+) 1 [10, 20, 30, 40] =
--   10 + (20 + (30 + (40 + 1)))
--

dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq f x [] =
  x
dobrar_esq f x (y:ys) =
  -- TODO: reimplementar sem foldl!
  foldl f x (y:ys)

--
-- dobrar_esq (+) 1 [10, 20, 30, 40] =
--   (((1 + 10) + 20) + 30) + 40
--

main :: IO ()
main = do
  let x = [10, 20, 30, 40, 50]

  print $ foldr (+) 0 x
  print $ foldl (flip (:)) [] x
