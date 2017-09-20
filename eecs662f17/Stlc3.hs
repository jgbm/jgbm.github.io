module Stlc3 where

import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
import Test.HUnit

type Ident = String

data Expr = EInt Integer | EAdd Expr Expr | EMult Expr Expr | ESubt Expr Expr
          | EBool Bool | EIs0 Expr | EIf Expr Expr Expr
          | EVar Ident | ELam Ident Type Expr | EApp Expr Expr
          | ELet Ident Expr Expr
          | EPair Expr Expr | ELetPair Ident Ident Expr Expr
          | EUnit | ELetUnit Expr Expr
          | EFix Expr
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Typing and type checking
--------------------------------------------------------------------------------

data Type = TInt | TBool | TFun Type Type | TPair Type Type | TOne
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
check g (ESubt e1 e2) =
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
check g (ELam x t1 e) =
    do t2 <- check ((x, t1):g) e
       return (TFun t1 t2)
check g (EApp e1 e2) =
    do TFun t1 t2 <- check g e1
       t1' <- check g e2
       if t1 == t1' then return t2 else Nothing
check g (ELet x e1 e2) =
    do t <- check g e1
       check ((x, t) : g) e2
check g (EPair e1 e2) =
    do t1 <- check g e1
       t2 <- check g e2
       return (TPair t1 t2)
check g (ELetPair x1 x2 e1 e2) =
    do TPair t1 t2 <- check g e1
       check ((x1, t1) : (x2, t2) : g) e2
check g EUnit =
    return TOne
check g (ELetUnit e1 e2) =
    do TOne <- check g e1
       check g e2
check g (EFix f) =
    do TFun t u <- check g f
       if t == u then return t else Nothing

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

data Value = VInt Integer | VBool Bool | VFun VEnv Ident Expr | VPair Value Value | VUnit
  deriving (Eq, Show)

type VEnv = [(Ident, Value)]

evaluationTests =
    TestList [ "seven" ~: eval [] (EAdd (EInt 1) (EMult (EInt 2) (EInt 3))) ~?= VInt 7
             , "nine" ~: eval [] (EMult (EAdd (EInt 1) (EInt 2)) (EInt 3)) ~?= VInt 9
             , "sixteen" ~: eval [] (EApp (ELam "x" TInt (EMult (EVar "x") (EVar "x")))
                                          (EInt 4)) ~?= VInt 16]


eval :: VEnv -> Expr -> Value
eval _ (EInt i) =
    VInt i
eval h (EAdd e1 e2) =
    let VInt x1 = eval h e1
        VInt x2 = eval h e2 in
    VInt (x1 + x2)
eval h (ESubt e1 e2) =
    let VInt x1 = eval h e1
        VInt x2 = eval h e2 in
    VInt (x1 - x2)
eval h (EMult e1 e2) =
    let VInt x1 = eval h e1
        VInt x2 = eval h e2 in
    VInt (x1 * x2)
eval _ (EBool b) =
    VBool b
eval h (EIs0 e) =
    let VInt x = eval h e in
    VBool (x == 0)
eval h (EIf e1 e2 e3) =
    let VBool b = eval h e1 in
    if b then eval h e2 else eval h e3
eval h (EVar x) =
    let Just v = lookup x h in v
eval h (ELam x t e) =
    VFun h x e
eval h (EApp e1 e2) =
    let VFun h' x e = eval h e1
        v = eval h e2 in
    eval ((x, v) : h') e
eval h (ELet x e1 e2) =
    let v = eval h e1 in
    eval ((x, v) : h) e2
eval h (EPair e1 e2) =
    let v1 = eval h e1
        v2 = eval h e2 in
    VPair v1 v2
eval h (ELetPair x1 x2 e1 e2) =
    let VPair v1 v2 = eval h e1 in
    eval ((x1, v1) : (x2, v2) : h) e2
eval h EUnit =
    VUnit
eval h (ELetUnit e1 e2) =
    let VUnit = eval h e1 in
    eval h e2
eval h (EFix f) =
    VFun h "$x" (EApp (EApp f (EFix f)) (EVar "$x"))

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
                     t <- multyp
                     reservedOp "->"
                     e <- exprp
                     return (ELam x t e)) <|>
                 (do x <- try (reserved "let" >> identifier)
                     reservedOp "="
                     e1 <- exprp
                     reserved "in"
                     e2 <- exprp
                     return (ELet x e1 e2)) <|>
                 (do reserved "let"
                     reservedOp "("
                     x1 <- identifier
                     reservedOp ","
                     x2 <- identifier
                     reservedOp ")"
                     reservedOp "="
                     e1 <- exprp
                     reserved "in"
                     e2 <- exprp
                     return (ELetPair x1 x2 e1 e2)) <|>
                 (do reserved "if"
                     e1 <- exprp
                     reserved "then"
                     e2 <- exprp
                     reserved "else"
                     e3 <- exprp
                     return (EIf e1 e2 e3)) <|>
                 seqp

          seqp = chainl1 addp (reservedOp ";" >> return (\e1 e2 -> ELetUnit e1 e2))

          addp = chainl1 multp ((reservedOp "+" >> return EAdd) <|> (reservedOp "-" >> return ESubt))

          multp = chainl1 applp (reservedOp "*" >> return EMult)

          applp = do es <- many1 atomp
                     case es of
                       [EVar "isz", e] -> return (EIs0 e)
                       [EVar "fix", e] -> return (EFix e)
                       _ -> return (foldl1 EApp es)

          atomp = choice [ EInt `fmap` lexeme intConst
                         , EBool `fmap` boolConst
                         , EVar `fmap` identifier
                         , do es <- parens (sepBy exprp comma)
                              case es of
                                []  -> return EUnit
                                [e] -> return e
                                [e1, e2] -> return (EPair e1 e2)
                                _ -> fail "Tuple too big (or small)" ]

          intConst = do ds <- many1 digit
                        return (read ds)

          boolConst = (reserved "True" >> return True) <|>
                      (reserved "False" >> return False)

          typep = chainr1 multyp (reservedOp "->" >> return TFun)

          multyp = chainl1 atomtp (reservedOp "*" >> return TPair)

          atomtp = (reserved "Int" >> return TInt) <|>
                   (reserved "Bool" >> return TBool) <|>
                   parens typep

--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

go s = do e <- parse' s
          _ <- check' e
          let v = eval [] e
          case v of
            VInt x -> return (show x)
            VBool b -> return (show b)
            VFun _ _ _ -> return "<<fun>>"
    where parse' e = case parse e of
                       Left e -> Left (show e)
                       Right e -> Right e
          check' e = case check [] e of
                       Nothing -> Left "type error"
                       Just t  -> Right t

tests = runTestTT (TestList [checkingTests, evaluationTests])
