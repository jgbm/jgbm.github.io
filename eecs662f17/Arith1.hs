module Arith1 where

import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
import Test.HUnit


--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data Expr = EAdd Expr Expr | EMult Expr Expr | EInt Integer
          | ESubt Expr Expr | ENegate Expr
          | EBool Bool | EIf Expr Expr Expr | EIs0 Expr
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

--------------------------------------------------------------------------------
-- Type checking
--------------------------------------------------------------------------------

-- data Maybe a = Nothing | Just a

data Type = TInt | TBool
  deriving (Eq, Show)

checkingTests =
    TestList [ "addition is okay" ~:
               check (EAdd (EInt 3) (EInt 3)) ~?= Just TInt
             , "integers aren't Boolean" ~:
               check (EAdd (EInt 3) (EBool True)) ~?= Nothing ]

check :: Expr -> Maybe Type
check (EInt i) =
    Just TInt
check (EAdd e1 e2) =
    case check e1 of
      Just TInt ->
          case check e2 of
            Just TInt -> Just TInt
            _ -> Nothing
      _ -> Nothing
-- The code below (for checking EMult) behaves identically to the code above (for checking EAdd);
-- for now, you can think of do notation as just syntactic sugar for nested cases.
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
    TestList [ "seven" ~: eval (desugar (EAdd (EInt 1) (EMult (EInt 2) (EInt 3)))) ~?= Just (VInt 7)
             , "nine" ~: eval (desugar (EMult (EAdd (EInt 1) (EInt 2)) (EInt 3))) ~?= Just (VInt 9)
             , "one" ~: eval (desugar (ESubt (EInt 2) (EInt 1))) ~?= Just (VInt 1)
             ]

data Value = VInt Integer | VBool Bool
  deriving (Eq, Show)

eval :: Expr -> Maybe Value
eval (EInt x) =
    return (VInt x)
eval (EAdd e1 e2) =
    do VInt x1 <- eval e1
       VInt x2 <- eval e2
       return (VInt (x1 + x2))
eval (EMult e1 e2) =
    do VInt x1 <- eval e1
       VInt x2 <- eval e2
       return (VInt (x1 * x2))
eval (EBool b) =
    return (VBool b)
eval (EIs0 e) =
    do VInt x <- eval e
       return (VBool (x == 0))
eval (EIf e1 e2 e3) =
    do VBool b <- eval e1
       if b then eval e2 else eval e3

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
                  addp

          addp = chainl1 multp ((reservedOp "+" >> return EAdd) <|>
                                (reservedOp "-" >> return ESubt))

          multp = chainl1 atomp (reservedOp "*" >> return EMult)

          atomp = choice [ do reserved "isz"
                              EIs0 `fmap` atomp
                         , EInt `fmap` lexeme intConst
                         , do reservedOp "-"
                              ENegate `fmap` atomp
                         , EBool `fmap` boolConst
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
