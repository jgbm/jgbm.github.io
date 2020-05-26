module Day5 where

------------------------------------------------
-- Dragon eggs (dragons hatch later in the file)
import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
------------------------------------------------

data Expr = Const Integer | Plus Expr Expr | Times Expr Expr
          | Divide Expr Expr
          | IfZ Expr Expr Expr
  deriving (Eq, Show)


--   (18 + 5) * 2
example1 = Times (Plus (Const 18) (Const 5)) (Const 2)
example1a = (Const 18 `Plus` Const 5) `Times` Const 2
example1b = parse "(18 + 5) * 2"

evalst :: Expr -> String
evalst (Const m) = show m
evalst (Plus e1 e2) = "(" ++ evalst e1 ++ "+" ++ evalst e2 ++ ")"
evalst (Times e1 e2) = "(" ++ evalst e1 ++ "*" ++ evalst e2 ++ ")"
evalst (Divide e1 e2) = "(" ++ evalst e1 ++ "/" ++ evalst e2 ++ ")"

--------------------------------------------------------------------------------

data EvalResult = Fine Integer | ZeroDiv
  deriving (Eq, Show)

--------------------------------------------------------------------------------

eval :: Expr -> EvalResult
eval (Const m)      = Fine m
eval (Plus e1 e2)   =
    case eval e1 of
      ZeroDiv -> ZeroDiv
      Fine z1   ->
          case eval e2 of
            ZeroDiv -> ZeroDiv
            Fine z2 -> Fine (z1 + z2)
eval (Times e1 e2)  =
    case eval e1 of
      ZeroDiv -> ZeroDiv
      Fine z1   ->
          case eval e2 of
            ZeroDiv -> ZeroDiv
            Fine z2 -> Fine (z1 * z2)
eval (Divide e1 e2) =
    case eval e1 of
      ZeroDiv -> ZeroDiv
      Fine z1   ->
          case eval e2 of
            ZeroDiv -> ZeroDiv
            Fine z2 | z2 == 0 -> ZeroDiv
                    | otherwise -> Fine (z1 `div` z2)
eval (IfZ e1 e2 e3) =
    case eval e1 of
      ZeroDiv -> ZeroDiv
      Fine 0  -> eval e2
      Fine _  -> eval e3

--------------------------------------------------------------------------------

binOp :: Expr -> Expr
      -> (Integer -> Integer -> EvalResult)
      -> EvalResult
binOp e1 e2 f =
    case eval1 e1 of
      ZeroDiv -> ZeroDiv
      Fine z1   ->
          case eval1 e2 of
            ZeroDiv -> ZeroDiv
            Fine z2 -> f z1 z2

eval1 :: Expr -> EvalResult
eval1 (Const m) = Fine m
eval1 (Plus e1 e2) = binOp e1 e2 (\x y -> Fine (x + y))
eval1 (Times e1 e2) = binOp e1 e2 (\x y -> Fine (x * y))
eval1 (Divide e1 e2) = binOp e1 e2 divide
    where divide m 0 = ZeroDiv
          divide m n = Fine (m `div` n)

--------------------------------------------------------------------------------

andThen :: EvalResult -> (Integer -> EvalResult) -> EvalResult
andThen (Fine z) k = k z
andThen ZeroDiv k   = ZeroDiv

eval2 (Const m) = Fine m
eval2 (Plus e1 e2) =
    eval2 e1 `andThen` (\z1 ->
    eval2 e2 `andThen` (\z2 ->
    Fine (z1 + z2)))
eval2 (Times e1 e2) =
    eval2 e1 `andThen` \z1 ->
    eval2 e2 `andThen` \z2 ->
    Fine (z1 * z2)
eval2 (Divide e1 e2) =
    eval2 e1 `andThen` \z1 ->
    eval2 e2 `andThen` \z2 ->
    if z2 == 0 then ZeroDiv else Fine (z1 `div` z2)
eval2 (IfZ e1 e2 e3) =
    eval2 e1 `andThen` \z1 ->
    if z1 == 0 then eval2 e2 else eval2 e3

--------------------------------------------------------------------------------

-- furthermore :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
furthermore (Just z) k = k z
furthermore Nothing  k = Nothing

eval3 :: Expr -> Maybe Integer
eval3 (Const m) = Just m
eval3 (Divide e1 e2) =
    eval3 e1 `furthermore` \z1 ->
    eval3 e2 `furthermore` \z2 ->
    if z2 == 0 then Nothing else Just (z1 `div` z2)




{-
foldExpr :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr const plus times (Const m) = const m
foldExpr const plus times (Plus e1 e2) = plus (foldExpr const plus times e1)
                                              (foldExpr const plus times e2)
foldExpr const plus times (Times e1 e2) = times (foldExpr const plus times e1)
                                                (foldExpr const plus times e2)

eval' = foldExpr (\m -> m) (+) (*)

data Tree a = Leaf a | Branch a [Tree a]
  deriving (Show)
data EvalJ  = Evals Expr Integer
  deriving (Show)

treeResult (Leaf (Evals _ m))     = m
treeResult (Branch (Evals _ m) _) = m

showTree n (Leaf (Evals e m)) = replicate n ' ' ++ evalst e ++ " ~~> " ++ show m
showTree n (Branch (Evals e m) ts) =
    unlines (map (showTree (n + 2)) ts) ++
    replicate n ' ' ++ evalst e ++ " ~~> " ++ show m

exampleT = Branch (Evals (parse "4+6") 10)
                  [Leaf (Evals (parse "4") 4),
                   Leaf (Evals (parse "6") 6)]

evalt :: Expr -> Tree EvalJ
evalt (Const m) = Leaf (Evals (Const m) m)
evalt (Plus e1 e2) = Branch (Evals (Plus e1 e2) (treeResult t1 + treeResult t2))
                            [t1, t2]
    where t1 = evalt e1
          t2 = evalt e2
evalt (Times e1 e2) = Branch (Evals (Times e1 e2) (treeResult t1 * treeResult t2))
                            [t1, t2]
    where t1 = evalt e1
          t2 = evalt e2

-}





































































--------------------------------------------------------------------------------
-- Here be dragons
--------------------------------------------------------------------------------

parse :: String -> Expr
parse s = case runParser (terminal exprp) () "" s of
          Left err -> error (show err)
          Right t  -> t
    where l = makeTokenParser $
              haskellDef { reservedNames = ["True", "False", "if", "let", "in", "Int", "Bool"]
                         , reservedOpNames = ["+", "*", "\\", "->", ":", ",", "/"] }

          terminal p = do x <- p
                          eof
                          return x
          identifier = T.identifier l
          reserved = T.reserved l
          reservedOp = T.reservedOp l
          parens = T.parens l
          lexeme = T.lexeme l
          comma = T.comma l

          exprp = ifp

          ifp = choice [do reserved "if"
                           e1 <- ifp
                           e2 <- ifp
                           e3 <- ifp
                           return (IfZ e1 e2 e3),
                        addp]
          addp = chainl1 multp (reservedOp "+" >> return Plus)
          multp = chainl1 atomp ((reservedOp "*" >> return Times) <|> (reservedOp "/" >> return Divide))


          atomp = choice [ Const `fmap` lexeme intConst
                         , parens exprp ]

          intConst = do ds <- many1 digit
                        return (read ds)
