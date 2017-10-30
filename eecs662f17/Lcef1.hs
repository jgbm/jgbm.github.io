module Lcef0 where

import Control.Monad (liftM2)
import Data.List (intercalate)

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
          | EThrow Type | ECatch Expr Expr
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Typing and type checking
--------------------------------------------------------------------------------

data Type = TPos | TInt | TBool | TFun Type EffSet Type | TPair Type Type | TOne
  deriving (Eq, Show)

data Effect = Read | Write | Ex
  deriving (Eq, Show)

type EffSet = [Effect]

type TEnv = [(Ident, Type)]

data TcM t = TcM (Maybe (t, EffSet))

runTcM :: (TcM t) -> Maybe (t, EffSet)
runTcM (TcM e) = e

typeError :: TcM a
typeError = TcM Nothing

effects :: TcM a -> TcM (a, EffSet)
effects (TcM Nothing) = TcM Nothing
effects (TcM (Just (v, z))) = TcM (Just ((v, z), []))

tell :: EffSet -> TcM ()
tell z = TcM (Just ((), z))

instance Monad TcM where
    return x = TcM (Just (x, []))

    TcM Nothing >>= f = TcM Nothing
    TcM (Just (v, z1)) >>= f =
        TcM (case runTcM (f v) of
               Nothing -> Nothing
               Just (w, z2) -> Just (w, z1 ++ z2))

union z1 z2 = z1 ++ z2

join :: Type -> Type -> TcM Type
join (TFun t1 z1 u1) (TFun t2 z2 u2) =
    do t <- meet t1 t2
       u <- join u1 u2
       return (TFun t (union z1 z2) u)
join (TPair t1 u1) (TPair t2 u2) =
    do t <- join t1 t2
       u <- join u1 u2
       return (TPair t u)
join t u
    | t == u        = return u
    | t `subtype` u = return u
    | u `subtype` t = return t
    | otherwise     = typeError

intersect z1 z2 = filter (`elem` z2) z1

meet :: Type -> Type -> TcM Type
meet (TFun t1 z1 u1) (TFun t2 z2 u2) =
    do t <- join t1 t2
       u <- meet u1 u2
       return (TFun t (intersect z1 z2) u)
meet (TPair t1 u1) (TPair t2 u2) =
    do t <- meet t1 t2
       u <- meet u1 u2
       return (TPair t u)
meet t u
    | t == u        = return u
    | t `subtype` u = return t
    | u `subtype` t = return u
    | otherwise     = typeError

subset z1 z2 = all (`elem` z2) z1

subtype :: Type -> Type -> Bool
subtype TPos TPos =
    True
subtype TPos TInt =
    True
subtype TInt TInt =
    True
subtype TBool TBool =
    True
subtype (TPair t1 u1) (TPair t2 u2) =
    t1 `subtype` t2 && u1 `subtype` u2
subtype TOne TOne =
    True
subtype (TFun t1 effs1 u1) (TFun t2 effs2 u2) =
    t2 `subtype` t1 && subset effs1 effs2 && u1 `subtype` u2
subtype _ _ =
    False

checkSubtype :: TEnv -> Expr -> Type -> TcM Type
checkSubtype g e u =
    do t <- check g e
       if t `subtype` u then return t else typeError

check :: TEnv -> Expr -> TcM Type
check _ (EInt i)
    | i > 0 = return TPos
    | otherwise = return TInt
check g (EAdd e1 e2) =
    do t <- checkSubtype g e1 TInt
       u <- checkSubtype g e2 TInt
       join t u
check g (ESubt e1 e2) =
    do checkSubtype g e1 TInt
       checkSubtype g e2 TInt
       return TInt
check g (EMult e1 e2) =
    do t <- checkSubtype g e1 TInt
       u <- checkSubtype g e2 TInt
       join t u
check _ (EBool _) =
    return TBool
check g (EIs0 e) =
    do checkSubtype g e TInt
       return TBool
check g (EIf e1 e2 e3) =
    do TBool <- check g e1
       t2 <- check g e2
       t3 <- check g e3
       join t2 t3
check g (EVar x) =
    case lookup x g of
      Nothing -> typeError
      Just t  -> return t
check g (ELam x t1 e) =
    do (t2, z) <- effects (check ((x, t1):g) e)
       return (TFun t1 z t2)
check g (EApp e1 e2) =
    do TFun t1 z t2 <- check g e1
       t1' <- check g e2
       tell z
       if t1' `subtype` t1 then return t2 else typeError
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
    do TFun t [] (TFun u z v) <- check g f
       if t == TFun u z v then return t else typeError
check g EGet =
    do tell [Read]
       return TInt
check g (EPut e) =
    do checkSubtype g e TInt
       tell [Write]
       return TOne
check g (EThrow t) =
    do tell [Ex]
       return t
check g (ECatch e1 e2) =
    do (t, z1) <- effects (check g e1)
       tell (filter (Ex /=)  z1)   -- (\eff -> Ex /= eff)
       u <- check g e2
       join t u

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

data Value = VInt Integer | VBool Bool | VFun VEnv Ident Expr | VPair Value Value | VUnit
  deriving (Eq, Show)

data Sx s t = Sx (s -> (Maybe t, s))

runSx :: Sx s t -> s -> (Maybe t, s)
runSx (Sx s) = s

instance Monad (Sx s) where
    return x = Sx (\s -> (Just x, s))

    -- Sx s t -> (t -> Sx s u) -> Sx s u
    Sx sf >>= vf = Sx (\s -> case sf s of
                               (Nothing, s1) -> (Nothing, s1)
                               (Just v, s1) -> runSx (vf v) s1)

type VEnv = [(Ident, Value)]

geval :: (Monad r) =>
         (VEnv -> Expr -> r Value) ->
         VEnv -> Expr -> r Value
geval recursiveEval h e = eval' h e
    where eval' _ (EInt x) =
              return (VInt x)
          eval' h (EAdd e1 e2) =
              do VInt x1 <- recursiveEval h e1
                 VInt x2 <- recursiveEval h e2
                 return (VInt (x1 + x2))
          eval' h (ESubt e1 e2) =
              do VInt x1 <- recursiveEval h e1
                 VInt x2 <- recursiveEval h e2
                 return (VInt (x1 - x2))
          eval' h (EMult e1 e2) =
              do VInt x1 <- recursiveEval h e1
                 VInt x2 <- recursiveEval h e2
                 return (VInt (x1 * x2))
          eval' _ (EBool b) =
              return (VBool b)
          eval' h (EIs0 e) =
              do VInt x <- recursiveEval h e
                 return (VBool (x == 0))
          eval' h (EIf e e1 e2) =
              do VBool b <- recursiveEval h e
                 if b then recursiveEval h e1 else recursiveEval h e2
          eval' h (EVar x) =
              let Just v = lookup x h in return v
          eval' h (ELam x t e) =
              return (VFun h x e)
          eval' h (EApp e1 e2) =
              do VFun h' x e <- recursiveEval h e1
                 v <- recursiveEval h e2
                 recursiveEval ((x, v) : h') e
          eval' h (ELet x e1 e2) =
              do v <- recursiveEval h e1
                 recursiveEval ((x, v) : h) e2
          eval' h (EPair e1 e2) =
              do v1 <- recursiveEval h e1
                 v2 <- recursiveEval h e2
                 return (VPair v1 v2)
          eval' h (ELetPair x1 x2 e1 e2) =
              do VPair v1 v2 <- recursiveEval h e1
                 recursiveEval ((x1, v1) : (x2, v2) : h) e2
          eval' h (EFix f) =
              return (VFun h "$x" (EApp (EApp f (EFix f)) (EVar "$x")))
          eval' h EUnit =
              return VUnit
          eval' h (ELetUnit e1 e2) =
              do VUnit <- recursiveEval h e1
                 recursiveEval h e2

eval :: VEnv -> Expr -> Sx Integer Value    -- Formerly StateResult
eval h EGet =
    Sx (\s -> (Just (VInt s), s))    -- :: St Integer VAlue
eval h (EPut e) =
    eval h e >>= \(VInt v) ->
        Sx (\_ -> (Just VUnit, v))
eval h (EThrow _) = Sx (\s -> (Nothing, s))
eval h (ECatch e1 e2) =
    Sx (\s -> case runSx (eval h e1) s of
                (Nothing, s1) -> runSx (eval h e2) s1
                (Just v, s1) -> (Just v, s1))
eval h e = geval eval h e

run :: Sx Integer Value -> (Maybe Value, Integer)
run v = runSx v 0

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parse :: String -> Either ParseError Expr
parse = runParser (terminal exprp) () ""
    where l = makeTokenParser $
              haskellDef { reservedNames = ["True", "False", "if", "then", "else",
                                            "let", "in", "Pos", "Int", "Bool", "try", "catch", "throw"]
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
                 (do reserved "try"
                     e1 <- exprp
                     reserved "catch"
                     e2 <- exprp
                     return (ECatch e1 e2)) <|>
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
                         , do reserved "throw"
                              t <- brackets typep
                              return (EThrow t)
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

          typep = chainr1 multyp (do reservedOp "-{"
                                     effs <- sepBy effp comma
                                     reservedOp "}->"
                                     return (\t u -> TFun t effs u))

          multyp = chainl1 atomtp (reservedOp "*" >> return TPair)

          atomtp = (reserved "Pos" >> return TPos) <|>
                   (reserved "Int" >> return TInt) <|>
                   (reserved "Bool" >> return TBool) <|>
                   (reserved "1" >> return TOne) <|>
                   parens typep

          effp = (reserved "read" >> return Read) <|>
                 (reserved "write" >> return Write) <|>
                 (reserved "ex" >> return Ex)



--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

go s = case result of
         Left e -> putStrLn ("ERROR: " ++ show e)
         Right s -> putStrLn s


    where parse' e = case parse e of
                       Left e -> Left (show e)
                       Right e -> Right e
          check' e = case runTcM (check [] e) of
                       Nothing -> Left "type error"
                       Just (t, eff)  -> Right (t, eff)
          result = do e <- parse' s
                      (t, effs) <- check' e
                      let effString | null effs = "{}"
                                    | otherwise = "{" ++ intercalate ", " (map show effs) ++ "}"
                      return (unlines [show (run (eval [] e)), ":: " ++ show t ++ " & " ++ effString])

instance Functor (Sx s) where
    fmap f = (>>= return . f)

instance Applicative (Sx s) where
    pure = return
    (<*>) = liftM2 ($)

instance Functor TcM where
    fmap f = (>>= return . f)

instance Applicative TcM where
    pure  = return
    (<*>) = liftM2 ($)
