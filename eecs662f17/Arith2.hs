module Arith2 where

import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
import Test.HUnit


--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

type Ident = String
data Expr = EAdd Expr Expr | EMult Expr Expr | EInt Integer
          | ESubt Expr Expr | ENegate Expr
          | EBool Bool | EIf Expr Expr Expr | EIs0 Expr
          | EVar Ident | ELet Ident Expr Expr
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Desugaring
--------------------------------------------------------------------------------

desugaringTests =
    TestList [ "desugaring subtraction" ~:
               desugar (ESubt (EInt 2) (EInt 1)) ~?=
               EAdd (EInt 2) (EMult (EInt 1) (EInt (-1)))
             , "desugaring nested subtraction" ~:
               desugar (ESubt (ESubt (EInt 3) (EInt 2)) (EInt 2)) ~?=
                 EAdd (EAdd (EInt 3) (EMult (EInt 2) (EInt (-1))))
                      (EMult (EInt 2) (EInt (-1)))
             , "desugaring negation" ~:
               desugar (ENegate (EInt 3)) ~?=
               EAdd (EInt 0) (EMult (EInt 3) (EInt (-1))) ]

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
desugar (EBool b) =
    EBool b
desugar (EIs0 e) =
    EIs0 (desugar e)
desugar (EIf e1 e2 e3) =
    EIf (desugar e1) (desugar e2) (desugar e3)
desugar (EVar x) =
    EVar x
desugar (ELet x e1 e2) =
    ELet x (desugar e1) (desugar e2)

--------------------------------------------------------------------------------
-- Type checking
--------------------------------------------------------------------------------

data Type = TInt | TBool
  deriving (Eq, Show)

checkingTests =
    TestList [ "addition is okay" ~:
               check (EAdd (EInt 3) (EInt 3)) ~?= Just TInt
             , "integers aren't Boolean" ~:
               check (EAdd (EInt 3) (EBool True)) ~?= Nothing ]

check :: Expr -> Maybe Type
check (EInt i) =
    return TInt
check (EAdd e1 e2) =
    do TInt <- check e1
       TInt <- check e2
       return TInt
check (EMult e1 e2) =
    do TInt <- check e1
       TInt <- check e2
       return TInt
check (EBool b) =
    return TBool
check (EIs0 e) =
    do TInt <- check e
       return TBool
check (EIf e1 e2 e3) =
    do TBool <- check e1
       t2 <- check e2
       t3 <- check e3
       if t2 == t3 then return t2 else Nothing

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

evalTests =
    TestList [ "seven" ~: eval (desugar (EAdd (EInt 1) (EMult (EInt 2) (EInt 3)))) ~?= VInt 7
             , "nine" ~: eval (desugar (EMult (EAdd (EInt 1) (EInt 2)) (EInt 3))) ~?= VInt 9
             , "one" ~: eval (desugar (ESubt (EInt 2) (EInt 1))) ~?= VInt 1
             ]

data Value = VInt Integer | VBool Bool
  deriving (Eq, Show)

subst :: Ident -> Expr -> Expr -> Expr
subst _ _ (EInt v) =
    EInt v
subst x w (EAdd e1 e2) =
    EAdd (subst x w e1) (subst x w e2)
subst x w (EMult e1 e2) =
    EMult (subst x w e1) (subst x w e2)
subst _ _ (EBool b) =
    EBool b
subst x w (EIs0 e) =
    EIs0 (subst x w e)
subst x w (EIf e1 e2 e3) =
    EIf (subst x w e1) (subst x w e2) (subst x w e3)
subst x w (EVar y)
    | x == y = w
    | otherwise = EVar y
{-
Equivalent formulations:

subst x w (EVar y) =
  if x == y then w else EVar y

subst x w (EVar y)
  | x == y = w
subst x w (EVar y)  =
  EVar y
-}
subst x w (ELet y e1 e2)
    | x /= y = ELet y (subst x w e1) (subst x w e2)
    | otherwise = ELet y (subst x w e1) e2

eval :: Expr -> Value
eval (EInt x) =
    VInt x
eval (EAdd e1 e2) =
    let VInt x1 = eval e1
        VInt x2 = eval e2 in
    VInt (x1 + x2)
eval (EMult e1 e2) =
    let VInt x1 = eval e1
        VInt x2 = eval e2 in
    VInt (x1 * x2)
eval (EBool b) =
    VBool b
eval (EIs0 e) =
    let VInt x = eval e in
    VBool (x == 0)
eval (EIf e1 e2 e3) =
    let VBool b = eval e1 in
    if b then eval e2 else eval e3
eval (ELet x e1 e2) =
    eval (subst x e1 e2)
eval (EVar x) =
    error "Unbound variable"

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

          exprp = (do reserved "if"
                      e1 <- exprp
                      reserved "then"
                      e2 <- exprp
                      reserved "else"
                      e3 <- exprp
                      return (EIf e1 e2 e3)) <|>
                  (do reserved "let"
                      x <- identifier
                      reservedOp "="
                      e1 <- exprp
                      reserved "in"
                      e2 <- exprp
                      return (ELet x e1 e2)) <|>
                  addp

          addp = chainl1 multp ((reservedOp "+" >> return EAdd) <|>
                                (reservedOp "-" >> return ESubt))

          multp = chainl1 atomp (reservedOp "*" >> return EMult)

          atomp = choice [ do reserved "isz"
                              EIs0 `fmap` exprp
                         , EInt `fmap` lexeme intConst
                         , do reservedOp "-"
                              ENegate `fmap` atomp
                         , EBool `fmap` boolConst
                         , EVar `fmap` identifier
                         , parens exprp ]

          intConst = do ds <- many1 digit
                        return (read ds)

          boolConst = (reserved "True" >> return True) <|>
                      (reserved "False" >> return False)

--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

go s = do e <- parse' s
          return (show (eval (desugar e)))
    where parse' e = case parse e of
                       Left e -> Left (show e)
                       Right e -> Right e

tests = runTestTT (TestList [desugaringTests, evalTests])
