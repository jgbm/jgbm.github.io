module Ulc0 where

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
          | ELam Ident Expr | EApp Expr Expr
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
    desugar (EApp (ELam x e2) e1)
desugar (ELam x e) = ELam x (desugar e)
desugar (EApp e1 e2) = EApp (desugar e1) (desugar e2)

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

evalTests =
    TestList [ "seven" ~: eval (desugar (EAdd (EInt 1) (EMult (EInt 2) (EInt 3)))) ~?= VInt 7
             , "nine" ~: eval (desugar (EMult (EAdd (EInt 1) (EInt 2)) (EInt 3))) ~?= VInt 9
             , "one" ~: eval (desugar (ESubt (EInt 2) (EInt 1))) ~?= VInt 1
             , "substitution" ~: eval (desugar (EApp (ELam "x" (EAdd (EVar "x") (EVar "x")))
                                                     (EInt 5))) ~?= VInt 10
             ]

{-

An evaluation test we won't include:

   (\f -> f f) (\f -> f f)

Because it takes a long time to finish evaluating

-}

data Value = VInt Integer | VBool Bool | VFun Ident Expr
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

{-
Don't need substution for ELet -- because we desugared it away!  But, I've left it here for the
parallel to substitution on EApp/ELam

subst x w (ELet y e1 e2)
    | x /= y = ELet y (subst x w e1) (subst x w e2)
    | otherwise = ELet y (subst x w e1) e2
-}

subst x w (EApp e1 e2) =
    EApp (subst x w e1) (subst x w e2)
subst x w (ELam y e)
    | x == y = ELam y e
    | otherwise = ELam y (subst x w e)

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
{-

Don't need evaluation for ELet either, but again, I've left the rule here for comparison to
evaluation of ELam/EApp

eval (ELet x e1 e2) =
    eval (subst x e1 e2)
-}
eval (EVar x) =
    error "Unbound variable"
eval (ELam x e) =
    VFun x e
eval (EApp e0 e2) =
    let VFun x e1 = eval e0 in
    eval (subst x e2 e1)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parse :: String -> Either ParseError Expr
parse = runParser (terminal exprp) () ""
    where l = makeTokenParser $
              haskellDef { reservedNames = ["True", "False", "if", "then", "else",
                                            "let", "in", "Int", "Bool"]
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

          exprp = lamp

          lamp = (do reservedOp "\\"
                     x <- identifier
                     reservedOp "->"
                     e <- exprp
                     return (ELam x e)) <|>
                 (do reserved "if"
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

          addp = chainl1 multp (reservedOp "+" >> return EAdd)

          multp = chainl1 applp (reservedOp "*" >> return EMult)

          applp = do es <- many1 atomp
                     case es of
                       [EVar "isz", e] -> return (EIs0 e)
                       _ -> return (foldl1 EApp es)

          atomp = choice [ EInt `fmap` lexeme intConst
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
