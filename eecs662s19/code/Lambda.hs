module Day9 where

------------------------------------------------
-- Dragon eggs (dragons hatch later in the file)
import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Token as T
------------------------------------------------

type Name = String
data Term = Var Name          -- "x"
          | Lam Name Term     -- "\x.t"
          | App Term Term     -- "t1 t2"
          | Const Int         -- "z"
          | Plus Term Term    -- "t1 + t2"
  deriving (Eq, Show)
data Value = VInt Int
           | VLam Name Term
  deriving (Eq, Show)
type Trace = [String]

indent :: Trace -> Trace
indent ss = ["  " ++ s | s <- ss]

thenDo :: Maybe (t, Trace)
       -> (t -> Maybe (u, Trace))
       -> Maybe (u, Trace)
Nothing `thenDo` f = Nothing
Just (x, w) `thenDo` f =
    case f x of
      Just (y, w') -> Just (y, w ++ w')
      Nothing      -> Nothing

x `andDo` y = x `thenDo` \() -> y

done :: t -> Maybe (t, Trace)
done t = Just (t, [])

eval'd :: Term -> Value -> String
eval'd t v = show t ++ " \\||/ " ++ show v

did :: String -> Maybe ((), Trace)
did s = Just ((), [s])

sub :: Maybe (t, Trace) -> Maybe (t, Trace)
sub Nothing = Nothing
sub (Just (v, w)) = Just (v, indent w)

oops :: Maybe (t, Trace)
oops = Nothing

-- Here be tiny dragons
go :: String -> IO ()
go s =
    case eval (parse s) of
      Nothing -> putStrLn "Nopers"
      Just (v, w) ->
          mapM_ putStrLn w

eval :: Term -> Maybe (Value, Trace)
eval (Const z) =
    did (eval'd (Const z) (VInt z)) `andDo`
    done (VInt z)
eval (Plus t1 t2) =
    sub (eval t1) `thenDo` \v1 ->
    sub (eval t2) `thenDo` \v2 ->
        case (v1, v2) of
          (VInt z1, VInt z2) -> did (eval'd (Plus t1 t2) (VInt (z1 + z2))) `andDo`
                                done (VInt (z1 + z2))
          _                  -> oops
eval (Var _) = oops
eval (Lam x t) =
    did (eval'd (Lam x t) (VLam x t)) `andDo`
    done (VLam x t)
{-------------------------------------------------------------------------------

    t1 ⇓ \x.t
    t2 ⇓ w
    t[w/x] ⇓ v
    ----------- (β)
    t1 t2 ⇓ v
-------------------------------------------------------------------------------}
eval (App t1 t2) =
    sub (eval t1) `thenDo` \v1 ->
    sub (eval t2) `thenDo` \v2 ->
        case v1 of
          VLam x t -> sub (eval (subst t v2 x)) `thenDo` \v ->
                      did (eval'd (App t1 t2) v) `andDo`
                      done v
          _        -> oops

subst :: Term -> Value -> Name -> Term
subst (Const z) _ _ = Const z
subst (Plus t1 t2) v x = Plus (subst t1 v x) (subst t2 v x)
subst (App t1 t2) v x = App (subst t1 v x) (subst t2 v x)
subst (Lam y t) v x
    | x == y = Lam y t
    | otherwise = Lam y (subst t v x)
subst (Var y) v x
    | x == y = termOf v
    | otherwise = Var y
    where termOf :: Value -> Term
          termOf (VInt z) = Const z
          termOf (VLam x t) = Lam x t



















































--------------------------------------------------------------------------------
-- Here be dragons
--------------------------------------------------------------------------------

parse :: String -> Term
parse s = case runParser (terminal exprp) () "" s of
          Left err -> error (show err)
          Right t  -> t
    where l = makeTokenParser $
              haskellDef { reservedNames = []
                         , reservedOpNames = ["+", "-", "*", "/" ] }

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


          lamp = choice [ do reservedOp "\\"
                             x <- identifier
                             reservedOp "."
                             t <- lamp
                             return (Lam x t)
                        , addp ]

          addp = chainl1 appp (reservedOp "+" >> return Plus)

          appp = do es <- many1 atomp
                    return (foldl1 App es)

          atomp = choice [ Const `fmap` lexeme intConst
                         , Var `fmap` identifier
                         , parens exprp ]

          intConst = do isNeg <- option False (char '-' >> return True)
                        ds <- many1 digit
                        return ((if isNeg then negate else id) (read ds))
