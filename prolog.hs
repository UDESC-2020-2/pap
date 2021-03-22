import Text.ParserCombinators.Parsec

data Term = Atom Name
          | Variable Name
          | Predicate Name [Term]
          deriving Show

type Name = String

-- O tipo Unifier representa uma lista de tuplas de Name e Term
-- Por exemplo, o unificador:
--   { A |-> foo, B |-> bar() }
-- É represenado como:
--   [("A", Atom "foo"), ("B", Predicate "bar" [])]
type Unifier = [(Name, Term)]

atom :: Parser Term
atom = do
  name <- many1 lower
  return (Atom name)

variable :: Parser Term
variable = do
  start <- upper
  remain <- many lower
  return (Variable (start:remain))

predicate :: Parser Term
predicate = do
  name <- many1 lower
  char '('
  subterms <- term `sepBy` (char ',')
  char ')'
  return (Predicate name subterms)

term :: Parser Term
term =
  try predicate <|> atom <|> variable

main :: IO ()
main = do
  putStrLn "Digite um termo:"
  a <- getLine
  -- Assume que o parsing deu certo!
  let Right term_a = parse term "<stdin>" a
  --
  putStrLn "Digite outro termo:"

  -- Bug do repl.it! Lê uma linha extra...
  getLine -- Remova se compilar localmente...

  b <- getLine
  -- Assume que o parsing deu certo!
  let Right term_b = parse term "<stdin>" b
  --
  putStrLn "Unificação:"
  -- print term_a
  -- print term_b
  print $ unify term_a term_b

-- Função de unificação!
unify :: Term -> Term -> Maybe Unifier

-- Regra (ATOM): átomos iguais unificam!
--
--   ---------------- (ATOM)
--      x ~ x = {}
unify (Atom x) (Atom y) | x == y =
  Just []

-- Regra (VAR): variáveis iguais unificam!
unify (Variable x) (Variable y) | x == y =
  Just []

-- Regra (LEFT): variável na esquerda!
--     A não aparece em b
--   ---------------------- (LEFT)
--     A ~ b = { A |-> b }
unify (Variable a) b =
  if occursCheck a b then
    Nothing
  else
    -- Troque a por b!
    Just [(a, b)]

-- Regra (RIGHT): variável na direita!
--     A não aparece em b
--   ---------------------- (RIGHT)
--     b ~ A = { A |-> b }
unify b (Variable a) =
  if occursCheck a b then
    Nothing
  else
    Just [(a, b)]

-- Regra (PRED): predicados!
--      [a1, ...] ~ [b1, ...] = t
--   ------------------------------- (PRED)
--     x(a1, ...) ~ x(b1, ...) = t
unify (Predicate a xs) (Predicate b ys) | a == b =
  -- Retorno o mesmo t :)
  unifyList xs ys

-- Caso padrão: não podemos unificar!
unify _ _ =
  Nothing

-- Unifica listas de predicado!
unifyList :: [Term] -> [Term] -> Maybe Unifier

-- Ambas as listas vazias
--
--   ----------------- (NIL)
--     [] ~ [] = {}
unifyList [] [] =
  Just []

-- Ambas as listas são células!
--     x ~ y = t1     (t1)xs ~ (t1)ys = t2
--   --------------------------------------- (PRED)
--         (x:xs) ~ (y:ys) = t2 * t1
unifyList (x:xs) (y:ys) =
  case unify x y of
    -- Sucesso na primeira hipótese!
    Just t1 ->
      -- Usamos o fmap para aplicar t1 em xs e ys!
      case unifyList (fmap (subst t1) xs) (fmap (subst t1) ys) of
        -- Sucesso na primeira hipótese!
        Just t2 ->
          -- Apenas compõe os resultados :)
          Just (compose t2 t1)
        -- Deu ruim na segunda hipótese
        Nothing ->
          Nothing
    -- Deu ruim a primeira hipótese
    Nothing ->
      Nothing

-- Cas padrão: não podemos unificar!
unifyList _ _ =
  Nothing

-- Verifica se um nome aparece em uma fórmula!
occursCheck :: Name -> Term -> Bool

-- Verifica se a variável X existe no átomo y
occursCheck x (Atom y) =
  False -- Não pode existir :)

-- Verifica se a variável X existe na variável y
occursCheck x (Variable y) =
  x == y -- Apenas se forem iguais!

-- Verifica se a variável X existe no predicado y(xs...)
occursCheck x (Predicate y xs) =
  -- X existe em qualquer um dos termos em xs?
  -- Notem que occursCheck x tem tipo Term -> Bool
  any (occursCheck x) xs

-- Compõe dois unificadores
compose :: Unifier -> Unifier -> Unifier
compose xs ys =
  -- Usa a função "mapear" da lista! :D
  xs ++ applyOnSubst xs ys

-- Applica uma substituição em outra (xs em ys)
applyOnSubst :: Unifier -> Unifier -> Unifier

-- Exemplo:
--   { X |-> foo } * { Y |-> X } = { Y |-> foo }
applyOnSubst xs ys =
  -- Pra cada troca A |-> b, vire A |-> (xs)b...
  let substOnTuple (name, term) =
        (name, subst xs term)

  in fmap substOnTuple ys

-- Aplica uma substituição em um termo!
subst :: Unifier -> Term -> Term

-- Não dá pra substituir nada em um átomo
subst xs (Atom a) =
  Atom a

-- Devemos substituir uma variável A se ela existir dentro da substituição
subst xs (Variable a) =
  -- Usamos a função lookup da biblitoca padrão! Temos { ..., A |-> b, ...}?
  case lookup a xs of
    -- Se A |-> b existe em xs, retorna b...
    Just b ->
      b
    -- Se não, ficamos com A sem ser alterado!
    Nothing ->
      Variable a

-- Para predicados, precisamos aplicar recursivamente :)
subst xs (Predicate a ys) =
  -- Lembrando que fmap f [a, b, ...] vira [f a, f b, ...]
  Predicate a (fmap (subst xs) ys)
