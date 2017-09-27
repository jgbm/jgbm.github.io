module Nat where

data Nat = Z | S Nat

one  = S Z
four = S (S (S (S Z)))

instance Eq Nat where
    Z == Z     = True
    S n == S m = n == m
    _ == _     = False

plusNat Z n     = n
plusNat (S m) n = plusNat m (S n)

type Integer' = (Nat, Nat)   --- (n, m)  means   n - m

negate (n, m)           = (m, n)
plusInt (n, m) (n', m') = (plusNat n n', plusNat m m')
isZero (n, m)           = eq m n

instance Eq Integer' where
    z == z' = isZero (plusInt z (negate z'))
  -- expands to: (n, m) == (n', m') = (isZero (plusInt (n, m) (negate (n', m')))
