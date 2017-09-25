module Classes where

class MyEq t where
    eq :: t -> t -> Bool

instance MyEq Bool where
    eq True True   = True
    eq False False = True
    eq _ _         = False

instance MyEq t => MyEq [t] where
    eq [] []         = True
    eq (x:xs) (y:ys) = eq x y && eq xs ys
    eq _ _           = False
