module Lcst0 where

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
          | EFix Expr
          | EUnit | ELetUnit Expr Expr
          | EGet | EPut Expr
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Typing and type checking
--------------------------------------------------------------------------------

data Type = TInt | TBool | TFun Type Type | TPair Type Type | TOne
  deriving (Eq, Show)

type TEnv = [(Ident, Type)]

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
    do TFun t (TFun u v) <- check g f
       if t == TFun u v then return t else Nothing
check g EGet =
    return TInt
check g (EPut e) =
    do TInt <- check g e
       return TOne

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

data Value = VInt Integer | VBool Bool | VFun VEnv Ident Expr | VPair Value Value | VUnit
  deriving (Eq, Show)
type Result = Integer -> (Value, Integer)

type VEnv = [(Ident, Value)]

eval :: VEnv -> Expr -> Result
eval _ (EInt x) =
    \s -> (VInt x, s)
eval h (EAdd e1 e2) =
    \s -> let (VInt x1, s1) = (eval h e1) s in
          let (VInt x2, s2) = (eval h e2) s1 in
          (VInt (x1 + x2), s2)
eval h EGet =
    \s -> (VInt s, s)
eval h (EPut e) =
    \s -> let (VInt v, _) = eval h e s in
          (VUnit, v)
eval h (ELetUnit e1 e2) =
    \s -> let (VUnit, s1) = eval h e1 s in
          eval h e2 s1

-- Remaining cases missing, because writing them is getting a bit tedious.
-- We'll return to that on Friday!

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
                       [EVar "get"] -> return EGet
                       [EVar "put", e] -> return (EPut e)
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
                   (reserved "1" >> return TOne) <|>
                   parens typep

--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

go s = do e <- parse' s
          _ <- check' e
          let v = eval [] e
          return (show (v 0))
    where parse' e = case parse e of
                       Left e -> Left (show e)
                       Right e -> Right e
          check' e = case check [] e of
                       Nothing -> Left "type error"
                       Just t  -> Right t
