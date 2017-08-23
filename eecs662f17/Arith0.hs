module Arith0 where

import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
import Test.HUnit


--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data Expr = EAdd Expr Expr | EMult Expr Expr | EInt Integer
          | ESubt Expr Expr | ENegate Expr
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Desugaring
--------------------------------------------------------------------------------


desugaringTests =
    TestList [ "desugaring subtraction" ~:
               desugar (ESubt (EInt 2) (EInt 1)) ~?= EAdd (EInt 2) (EMult (EInt 1) (EInt (-1)))
             , "desugaring nested subtraction" ~:
               desugar (ESubt (ESubt (EInt 3) (EInt 2)) (EInt 2)) ~?=
                 EAdd (EAdd (EInt 3) (EMult (EInt 2) (EInt (-1))))
                      (EMult (EInt 2) (EInt (-1)))
             , "desugaring negation" ~:
               desugar (ENegate (EInt 3)) ~?= EAdd (EInt 0) (EMult (EInt 3) (EInt (-1))) ]

desugar :: Expr -> Expr
desugar (ESubt e1 e2) =
    EAdd (desugar e1) (EMult (desugar e2) (EInt (-1)))
desugar (ENegate e) =
    desugar (ESubt (EInt 0) e)
desugar (EAdd e1 e2) =
    EAdd (desugar e1) (desugar e2)
desugar (EMult e1 e2) =
    EMult (desugar e1) (desugar e2)
desugar (EInt i) =
    EInt i

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

evalTests =
    TestList [ "seven" ~: eval (desugar (EAdd (EInt 1) (EMult (EInt 2) (EInt 3)))) ~?= 7
             , "nine" ~: eval (desugar (EMult (EAdd (EInt 1) (EInt 2)) (EInt 3))) ~?= 9
             , "one" ~: eval (desugar (ESubt (EInt 2) (EInt 1))) ~?= 1
             ]


eval :: Expr -> Integer
eval (EAdd e1 e2) =
    let v1 = eval e1
        v2 = eval e2 in
    v1 + v2
eval (EMult e1 e2) =
    eval e1 * eval e2
eval (EInt i) =
    i

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parse :: String -> Either ParseError Expr
parse = runParser (terminal exprp) () ""
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

          addp = chainl1 multp ((reservedOp "+" >> return EAdd) <|>
                                (reservedOp "-" >> return ESubt))

          multp = chainl1 atomp (reservedOp "*" >> return EMult)

          atomp = choice [ EInt `fmap` lexeme intConst
                         , do reservedOp "-"
                              ENegate `fmap` atomp
                         , parens exprp ]

          intConst = do ds <- many1 digit
                        return (read ds)

--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

go s = do e <- parse' s
          return (show (eval (desugar e)))
    where parse' e = case parse e of
                       Left e -> Left (show e)
                       Right e -> Right e

tests = runTestTT (TestList [desugaringTests, evaluationTests])
