module Arith0 where

------------------------------------------------
-- Dragon eggs (dragons hatch later in the file)
import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
------------------------------------------------

data Term = Const Int | Plus Term Term | Times Term Term


example1 = Times (Plus (Const 18) (Const 5)) (Const 2)
example1a = (Const 18 `Plus` Const 5) `Times` Const 2


pp :: Term -> String
pp (Const z)     = show z
pp (Plus t1 t2)  = "(" ++ pp t1 ++ ") + (" ++ pp t2 ++ ")"
pp (Times t1 t2) = "(" ++ pp t1 ++ ") * (" ++ pp t2 ++ ")"

eval :: Term -> Int
eval (Const z)     = z
eval (Plus t1 t2)  = eval t1 + eval t2
eval (Times t1 t2) = eval t1 * eval t2




































































--------------------------------------------------------------------------------
-- Here be dragons
--------------------------------------------------------------------------------

parse :: String -> Term
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
