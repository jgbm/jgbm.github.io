module Fix where

fix :: (t -> t) -> t
fix f = f (fix f)

fact :: Int -> Int
fact = fix (\f n -> if n == 0 then 1 else n * f (n - 1))

fib :: Int -> Int
fib = fix (\f n -> if n == 0 then 0
                   else if n == 1 then 1
                   else f (n - 1) + f (n - 2))

data Expr = Const Int | Plus Expr Expr | Times Expr Expr

eval :: Expr -> Int
eval = fix (\erec exp ->
                case exp of
                  Const i -> i
                  Plus e1 e2 -> erec e1 + erec e2
                  Times e1 e2 -> erec e1 * erec e2)

-- One test to rule them all

main = print (eval (Plus (Times (Const 2) (Const (fib 5))) (Const (fact 4))))
