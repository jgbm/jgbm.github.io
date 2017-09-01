module Stlc0 where

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
          | EVar Ident | ELam Ident Type Expr | EApp Expr Expr
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Typing and type checking
--------------------------------------------------------------------------------

data Type = TInt | TBool | TFun Type Type
  deriving (Eq, Show)

type TEnv = [(Ident, Type)]

checkingTests =
    TestList [ "addition is okay" ~:
               check [] (EAdd (EInt 3) (EInt 3)) ~?= Just TInt
             , "integers aren't Boolean" ~:
               check [] (EAdd (EInt 3) (EBool True)) ~?= Nothing
             , "application needs functions" ~:
               check [] (EApp (EInt 1) (EInt 2)) ~?= Nothing
             , "function arguments match" ~:
               check [] (EApp (ELam "x" TInt (EVar "x")) (EBool True)) ~?= Nothing
             , "function results match" ~:
               check [] (EAdd (EApp (ELam "x" TBool (EVar "x")) (EBool True))
                              (EInt 4)) ~?= Nothing ]


check :: TEnv -> Expr -> Maybe Type
check _ (EInt _) =
    return TInt
check g (EAdd e1 e2) =
    do TInt <- check g e1
       TInt <- check g e2
       return TInt
check g (EMult e1 e2) =
    do TInt <- check g e1
       TInt <- check g e2
       return TInt
check _ (EBool _) =
    return TBool
check g (EIs0 e) =
    do TInt <- check g e
       return TBool
check g (EIf e1 e2 e3) =
    do TBool <- check g e1
       t2 <- check g e2
       t3 <- check g e3
       if t2 == t3 then return t2 else Nothing
check g (EVar x) =
    lookup x g
    -- lookup :: a -> [(a,b)] -> Maybe b
check g (EApp e0 e1) =
    do TFun u t <- check g e0
       u' <- check g e1
       if u == u' then return t else Nothing
check g (ELam x t e) =
    do u <- check ((x,t) : g) e
       return (TFun t u)

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

evalTests =
    TestList [ "seven" ~: eval (EAdd (EInt 1) (EMult (EInt 2) (EInt 3))) ~?= VInt 7
             , "nine" ~: eval (EMult (EAdd (EInt 1) (EInt 2)) (EInt 3)) ~?= VInt 9
             , "one" ~: eval (ESubt (EInt 2) (EInt 1)) ~?= VInt 1
             , "substitution" ~: eval (EApp (ELam "x" TInt (EAdd (EVar "x") (EVar "x")))
                                            (EInt 5)) ~?= VInt 10
             ]

{-

An evaluation test we won't include:

   (\f -> f f) (\f -> f f)

Because it takes a long time to finish evaluating

-}

data Value = VInt Integer | VBool Bool | VFun Ident Type Expr
  deriving (Eq, Show)

exprOf (VInt i)   = EInt i
exprOf (VBool b)  = EBool b
exprOf (VFun x t e) = ELam x t e

subst :: Ident -> Value -> Expr -> Expr
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
    | x == y = exprOf w
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
subst x w (ELam y t e)
    | x == y = ELam y t e
    | otherwise = ELam y t (subst x w e)

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
eval (ELam x t e) =
    VFun x t e
eval (EApp e0 e2) =
    let VFun x _ e1 = eval e0
        v2 = eval e2 in
    eval (subst x v2 e1)

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
                     reservedOp ":"
                     t <- atomtp
                     reservedOp "->"
                     e <- exprp
                     return (ELam x t e)) <|>
                 (do reserved "if"
                     e1 <- exprp
                     reserved "then"
                     e2 <- exprp
                     reserved "else"
                     e3 <- exprp
                     return (EIf e1 e2 e3)) <|>
{-
                 (do try (reserved "let")
                     reservedOp "("
                     x1 <- identifier
                     reservedOp ","
                     x2 <- identifier
                     reservedOp ")"
                     reservedOp "="
                     e1 <- exprp
                     reserved "in"
                     e2 <- exprp
                     return (ELetPair x1 x2 e1 e2))
-}
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
{-
                         , do es <- parens (sepBy1 exprp comma)
                              case es of
                                [e] -> return e
                                [e1, e2] -> return (EPair e1 e2)
                                _ -> fail "Tuple too big (or small)" ]
-}

          intConst = do ds <- many1 digit
                        return (read ds)

          boolConst = (reserved "True" >> return True) <|>
                      (reserved "False" >> return False)

          typep = chainr1 multyp (reservedOp "->" >> return TFun)

          multyp = atomtp -- chainl1 atomtp (reservedOp "*" >> return TPair)

          atomtp = (reserved "Int" >> return TInt) <|>
                   (reserved "Bool" >> return TBool) <|>
                   parens typep



--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

go s = do e <- parse' s
          _ <- check' e
          return (show (eval e))
    where parse' e = case parse e of
                       Left e -> Left (show e)
                       Right e -> Right e
          check' e = case check [] e of
                       Nothing -> Left ("type error")
                       Just t  -> Right t

tests = runTestTT (TestList [evalTests])
