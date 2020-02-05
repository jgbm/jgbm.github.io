module Day3 where

------------------------------------------------
-- Dragon eggs (dragons hatch later in the file)
import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
------------------------------------------------

data Expr = Const Integer | Plus Expr Expr | Times Expr Expr
  deriving (Eq, Show)


--   (18 + 5) * 2
example1 = Times (Plus (Const 18) (Const 5)) (Const 2)
example1a = (Const 18 `Plus` Const 5) `Times` Const 2
example1b = parse "(18 + 5) * 2"

eval :: Expr -> Integer
eval (Const m)     = m
eval (Plus e1 e2)  = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2

evalst :: Expr -> String
evalst (Const m) = show m
evalst (Plus e1 e2) = "(" ++ evalst e1 ++ "+" ++ evalst e2 ++ ")"
evalst (Times e1 e2) = "(" ++ evalst e1 ++ "*" ++ evalst e2 ++ ")"


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







































































--------------------------------------------------------------------------------
-- Here be dragons
--------------------------------------------------------------------------------

parse :: String -> Expr
parse s = case runParser (terminal exprp) () "" s of
          Left err -> error (show err)
          Right t  -> t
    where l = makeTokenParser $
              haskellDef { reservedNames = ["True", "False", "if", "let", "in", "Int", "Bool"]
                         , reservedOpNames = ["+", "*", "\\", "->", ":", ","] }

          terminal p = do x <- p
                          eof
                          return x
          identifier = T.identifier l
          reserved = T.reserved l
          reservedOp = T.reservedOp l
          parens = T.parens l
          lexeme = T.lexeme l
          comma = T.comma l

          exprp = addp

          addp = chainl1 multp (reservedOp "+" >> return Plus)
          multp = chainl1 atomp (reservedOp "*" >> return Times)

          atomp = choice [ Const `fmap` lexeme intConst
                         , parens exprp ]

          intConst = do ds <- many1 digit
                        return (read ds)
