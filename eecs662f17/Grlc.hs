module Grlc where

import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
import Test.HUnit

type Ident = String

data Expr = EVar Ident | ELam Ident Type Expr | EApp Expr Expr
          | ETyLam TIdent Expr | ETyApp Expr Type
          | ELet Ident Expr Expr | EFix Expr
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Typing and type checking
--------------------------------------------------------------------------------

type TIdent = String

data Type = TVar TIdent | TFun Type Type | TPi TIdent Type
  deriving (Eq, Show)

type TIdentEnv = [TIdent]
type TEnv = [(Ident, Type)]

checkType :: TIdentEnv -> Type -> Maybe ()
checkType d (TVar a)
    | a `elem` d = return ()
    | otherwise = Nothing
checkType d (TFun t u) =
    do checkType d t
       checkType d u
checkType d (TPi a t) =
    checkType (a : d) t

substType :: TIdent -> Type -> Type -> Type
substType a u (TVar b)
    | a == b = u
    | otherwise = TVar b
substType a u (TFun t t') =
    TFun (substType a u t) (substType a u t')
substType a u (TPi b t)
    | a == b = TPi b t
    | otherwise = TPi b (substType a u t)

check :: TIdentEnv -> TEnv -> Expr -> Maybe Type
check d g (EVar x) =
    lookup x g
check d g (ELam x t1 e) =
    do checkType d t1
       t2 <- check d ((x, t1):g) e
       return (TFun t1 t2)
check d g (EApp e1 e2) =
    do TFun t1 t2 <- check d g e1
       t1' <- check d g e2
       if t1 == t1' then return t2 else Nothing
check d g (ETyLam a e)
    | a `elem` d = Nothing
    | otherwise = do t <- check (a : d) g e
                     return (TPi a t)
check d g (ETyApp e u) =
    do checkType d u
       TPi a t <- check d g e
       return (substType a u t)
check d g (ELet x e1 e2) =
    do t <- check d g e1
       check d ((x, t) : g) e2
check d g (EFix f) =
    do TFun t u <- check d g f
       if t == u then return t else Nothing

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

data Value = VFun VEnv Ident Expr | VTyFun VEnv TIdent Expr
  deriving (Eq, Show)

type VEnv = [(Ident, Value)]

eval :: VEnv -> Expr -> Value
eval h (EVar x) =
    let Just v = lookup x h in v
eval h (ELam x t e) =
    VFun h x e
eval h (EApp e1 e2) =
    let VFun h' x e = eval h e1
        v = eval h e2 in
    eval ((x, v) : h') e
eval h (ETyLam a e) =
    VTyFun h a e
eval h (ETyApp e u) =
    let VTyFun h' a e' = eval h e in
    eval h' e'
eval h (ELet x e1 e2) =
    let v = eval h e1 in
    eval ((x, v) : h) e2
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
          brackets = T.brackets l
          lexeme = T.lexeme l
          comma = T.comma l
          dot = T.dot l

          exprp = lamp

          lamp = (do reservedOp "\\"
                     x <- identifier
                     reservedOp ":"
                     t <- atomtp
                     reservedOp "->"
                     e <- exprp
                     return (ELam x t e)) <|>
                 (do reservedOp "/\\"
                     a <- identifier
                     reservedOp "->"
                     e <- exprp
                     return (ETyLam a e)) <|>
                 (do x <- try (reserved "let" >> identifier)
                     reservedOp "="
                     e1 <- exprp
                     reserved "in"
                     e2 <- exprp
                     return (ELet x e1 e2)) <|>
                 {-
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
                 -}
                 applp
{-
          seqp = chainl1 addp (reservedOp ";" >> return (\e1 e2 -> ELetUnit e1 e2))

          addp = chainl1 multp ((reservedOp "+" >> return EAdd) <|>
                                (reservedOp "-" >> return (\e1 e2 -> EAdd e1 (EMult (EInt (-1)) e2))))

          multp = chainl1 applp (reservedOp "*" >> return EMult)
-}

          applp = do e <- atomp
                     es <- many (Left `fmap` atomp <|>
                                 Right `fmap` brackets typep)
                     let app e1 (Left e2) = EApp e1 e2
                         app e1 (Right t) = ETyApp e1 t
                     case (e, es) of
{-
                       (EVar "isz", [Left e']) -> return (EIs0 e')
                       (EVar "isz", _) -> fail "Wrong arguments to isz"
-}
                       (EVar "fix", (Left e' : es)) ->
                           return (foldl app (EFix e') es)
                       (EVar "fix", _) -> fail "Wrong arguments to fix"
                       _ -> return (foldl app e es)

          atomp = choice [ {- EInt `fmap` lexeme intConst
                         , EBool `fmap` boolConst
                         , -} EVar `fmap` identifier
                         , do es <- parens (sepBy exprp comma)
                              case es of
                                -- []  -> return EUnit
                                [e] -> return e
                                -- [e1, e2] -> return (EPair e1 e2)
                                _ -> fail "Tuple too big (or small)" ]

{-
          intConst = do ds <- many1 digit
                        return (read ds)

          boolConst = (reserved "True" >> return True) <|>
                      (reserved "False" >> return False)
-}
          typep = do vs <- many (try (do a <- identifier
                                         dot
                                         return a))
                     t <- chainr1 atomtp {- multyp -} (reservedOp "->" >> return TFun)
                     return (foldr TPi t vs)

          -- multyp = chainl1 atomtp (reservedOp "*" >> return TProd)

          atomtp = {- (reserved "Int" >> return TInt) <|>
                   (reserved "Bool" >> return TBool) <|> -}
                   (TVar `fmap` identifier) <|>
                   parens typep

--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

level n m s
    | m <= n = s
    | otherwise = "(" ++ s ++ ")"

-- showType _ TInt        = "Int"
-- showType _ TBool       = "Bool"
-- showType _ TOne        = "1"
showType _ (TVar a)    = a
showType n (TPi a t)   = level 0 n (a ++ "." ++ showType 0 t)
showType n (TFun t u)  = level 1 n (showType 2 t ++ " -> " ++ showType 1 u)

go s = let result = do e <- parse' s
                       t <- check' e
                       return (show (eval [] e) ++ "\n:: " ++ showType 0 t) in
       case result of
         Left err -> putStrLn ("ERROR: " ++ err)
         Right r  -> putStrLn r
    where parse' e = case parse e of
                       Left e -> Left (show e)
                       Right e -> Right e
          check' e = case check [] [] e of
                       Nothing -> Left "type error"
                       Just t  -> Right t
