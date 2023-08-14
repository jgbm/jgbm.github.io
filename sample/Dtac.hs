{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances, RankNTypes #-}
module Dtac where

data (f :+: g) e = Inl (f e) | Inr (g e)

(f <?> g) (Inl x) = f x
(f <?> g) (Inr x) = g x

instance (Functor f, Functor g) => Functor (f :+: g)
    where fmap f (Inl m) = Inl (fmap f m)
          fmap f (Inr m) = Inr (fmap f m)
data Fix e = Fix (e (Fix e))

instance (Show (f e), Show (g e)) => Show ((f :+: g) e) where
  show (Inl x) = show x
  show (Inr x) = show x

instance Show (e (Fix e)) => Show (Fix e) where
  show (Fix x) = show x

data Plus e = Plus e e
instance Functor Plus
    where fmap f (Plus m n) = Plus (f m) (f n)

data Times e = Times e e
instance Functor Times
    where fmap f (Times m n) = Times (f n) (f n)

data Const e = Const Int
instance Functor Const
    where fmap _ (Const x) = Const x

instance Show e => Show (Plus e) where
  show (Plus x y) = "(" ++ show x ++ " + " ++ show y ++ ")"

instance Show e => Show (Times e) where
  show (Times x y) = show x ++ " * " ++ show y

instance Show (Const e) where
  show (Const n) = show n

data Yep
data Nope

type family IsIn f g where
  IsIn f f         = Yep
  IsIn (f :+: f') (g :+: g') = Or (Or (IsIn (f :+: f') g) (IsIn (f :+: f') g')) (And (IsIn f (g :+: g')) (IsIn f' (g :+: g')))
  IsIn f (g :+: h) = Or (IsIn f g) (IsIn f h)
  IsIn f g         = Nope

type family Or b c where
  Or Nope Nope = Nope
  Or b c       = Yep

type family And b c  where
  And Yep Yep = Yep
  And b c     = Nope

data L x
data R x
data S x y
data End

type family Into f g where
  Into f f         = End
  Into (f :+: f') (g :+: g') = Ifii (Into (f :+: f') g) (IsIn (f :+: f') g')
                                    (Into (f :+: f') g') (IsIn (f :+: f') g)
                                    (Into f (g :+: g')) (Into f' (g :+: g'))
  Into f (g :+: h) = Ifi (Into f g) (IsIn f h) (Into f h) (IsIn f g)
  Into f g         = Nope

type family Ifi p a q b where
  Ifi Nope a Nope b = Nope
  Ifi Nope a q Nope = R q
  Ifi p Nope q b    = L p
  Ifi p a q b       = Nope

type family Ifii p a q b s r where
  Ifii Nope a Nope b Nope r = Nope
  Ifii Nope a Nope b s Nope = Nope
  Ifii Nope a Nope b s r    = S s r
  Ifii Nope a q Nope s r    = R q
  Ifii p Nope q b s r       = L p
  Ifii p a q b s r          = Nope

class Inj f g p where
  inj' :: p -> f e -> g e

instance Inj f f End where
  inj' _ = id

instance Inj f g p => Inj f (g :+: h) (L p) where
  inj' (_ :: L p) = Inl . inj' (undefined :: p)

instance Inj f h p => Inj f (g :+: h) (R p) where
  inj' (_ :: R p)  = Inr . inj' (undefined :: p)

instance (Inj f h s, Inj g h r) => Inj (f :+: g) h (S s r) where
  inj' (_ :: S s r) = inj' (undefined :: s) <?> inj' (undefined :: r)

inj :: forall f g e. (Inj f g (Into f g)) => f e -> g e
inj = inj' (undefined :: Into f g)

inj1 :: Const e -> ((Times :+: Const) :+: Plus) e
inj1 = inj

-- Type variables are hard (for GHC):

--inj2 :: Const e -> (Const :+: f) e
--inj2 = inj

--inj2' :: Const e -> (f :+: Const) e
--inj2' = inj'

--inj3 :: f e -> (f :+: g) e
--inj3 = inj'

--------------------------------------------------------------------------------

data Onl (x :: * -> *)
data Onr (x :: * -> *)
data Le (x :: * -> *) p
data Ri (x :: * -> *) p

type family Minus f g where
  Minus (f :+: g) f = Onl g
  Minus (f :+: g) g = Onr f
  Minus (f :+: g) h = Ifm g (Minus f h) (IsIn h g) f (Minus g h) (IsIn h f)

type family Ifm g p a f q b where
  Ifm g Nope a f Nope b = Nope
  Ifm g Nope a f q Nope = Ri f q
  Ifm g p Nope f q b    = Le g p

type family OutOf p where
  OutOf (Onl x) = x
  OutOf (Onr x) = x
  OutOf (Le f p) = OutOf p :+: f
  OutOf (Ri f p) = f :+: OutOf p

class Without f g p where
  (??) :: (g e -> r) -> (OutOf p e -> r) -> p -> f e -> r

instance Without (f :+: g) f (Onl g) where
  (m ?? n) _ (Inl x) = m x
  (m ?? n) _ (Inr x) = n x

instance Without (f :+: g) g (Onr f) where
  (m ?? n) _ (Inl x) = n x
  (m ?? n) _ (Inr x) = m x

instance Without f h p => Without (f :+: g) h (Le g p) where
  (m ?? n) (_ :: Le g p) (Inl x) = (m ?? (n . Inl)) (undefined :: p) x
  (m ?? n) (_ :: Le g p) (Inr x) = n (Inr x)

instance Without g h p => Without (f :+: g) h (Ri f p) where
  (m ?? n) (_ :: Ri f p) (Inl x) = n (Inl x)
  (m ?? n) (_ :: Ri f p) (Inr x) = (m ?? (n . Inr)) (undefined :: p) x

(?) :: forall f g e r. Without f g (Minus f g) => (g e -> r) -> (OutOf (Minus f g) e -> r) -> f e -> r
m ? n = (??) m n (undefined :: Minus f g)

-- Doesn't type:
-- qu m n = (m ?? n) undefined

ec (Const u) r = u
ec2 (Const _) r = 0
ep (Plus x y) r = r x + r y
et (Times x y) r = r x * r y

cases cs = f where f (Fix e) = cs e f

const_ i = Fix (inj (Const i))
plus_ e1 e2 = Fix (inj (Plus e1 e2))
times_ e1 e2 = Fix (inj (Times e1 e2))

type E1 = Fix (Const :+: (Plus :+: Times))
type E2 = Fix ((Const :+: Plus) :+: Times)

-- evalOne :: E1 -> Int
-- evalOne = cases ((ec ? et) ? ep)
--
-- evalTwo :: E2 -> Int
-- evalTwo = cases ((ec ? et) ? ep)


--------------------------------------------------------------------------------

data Double e = Double e


recursively f (Fix e) = ((f ? Fix) . fmap (recursively f)) e


desugar e = cases ((\(Double e) r -> Fix (inj (Plus (r e) (r e)))) ? (const . Fix . fmap desugar)) e
desugar' e = recursively (\(Double e) -> plus_ e e)

-- Types, but not usable.
-- evalTwo :: Fix (Const :+: Const) -> Int
-- evalTwo = cases (ec ? ec2)
