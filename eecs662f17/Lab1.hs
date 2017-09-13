{-------------------------------------------------------------------------------

LAB 1

-------------------------------------------------------------------------------}

import Prelude hiding (reverse)

{-------------------------------------------------------------------------------

Part 1: Numbers and digits

-------------------------------------------------------------------------------}

-- Write a function which returns the last digit of its input number.  For
-- example, `lastDigit 89` should return `9`.
lastDigit :: Int -> Int
lastDigit = error "unimplemented"

-- Write a function which returns its input number, but missing its last digit.
-- For example, `lastDigit 289` should return `28`.
allButLastDigit :: Int -> Int
allButLastDigit = error "unimplemented"

-- Write a function which returns the digits of its input.  For example, `digits
-- 289` should return `[2,8,9]`
digits :: Int -> [Int]
digits = error "unimplemented"

-- Write a function which reverses its input list.  For example, `reverse
-- [2,8,9]` should return `[9,8,2]`.
reverse :: [Int] -> [Int]
reverse = error "unimplemented"

-- Write a function which returns true if its input number is a palindrome.  A
-- palindrome is something which is the same backwards and forwards.  In this
-- case, you should have `palindrome 2442` returning `True`, but `palindrome 24`
-- returning `False`.
palindrome :: Int -> Bool
palindrome = error "unimplemented"

-- Write a function which reverses the input digits of its argument.  For
-- example, `reverseDigits 289` should return `982`.
reverseDigits :: Int -> Int
reverseDigits = error "unimplemented"

{-------------------------------------------------------------------------------

Part 2: Sequences of numbers

-------------------------------------------------------------------------------}

-- Write a function which returns the n'th triangular numbers.  The n'th
-- triangular number is defined as the sum of 1 + 2 + ... + n.
tri :: Int -> Int
tri = error "unimplemented"

-- Write a function which returns the n'th Fibonacci number (counting from 0).
-- The n'th Fibonacci number is defined as:
--   0                        if n is 0
--   1                        if n is 1
--   fib (n-1) + fib (n-2)    if n > 1
-- Challenge: do this in time linear in n.
fib :: Int -> Int
fib = error "unimplemented"

-- Write a term which evaluates to an infinite list of 1s.
ones :: [Int]
ones = error "unimplemented"

-- Write a term which evaluates to the natural numbers.  Challenge: use zipWith
-- and ones.
nats :: [Int]
nats = error "unimplemented"

-- Write a term which evaluates to the sequence of triangular numbers.
-- Challenge: use zipWith, not your tri function.
tris :: [Int]
tris = error "unimplemented"

-- Write a term which evaluates to the sequence of Fibonacci numbers.
-- Challenge: use zipWith, not your fib function.  If you didn't get the linear
-- version of fib earlier, does this help?
fibs :: [Int]
fibs = error "unimplemented"


{-------------------------------------------------------------------------------

Part 3: Trees

-------------------------------------------------------------------------------}


data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

-- Write a function that finds if an element is within a given tree:
elemTree :: Eq a => a -> Tree a -> Bool
elemTree = error "unimplemented"

-- Write a function that finds if an element is within a given binary SEARCH
-- tree.  In a binary search tree, at a given node with value `n`, all values in
-- its left child tree should ne <= `n`, and all values in its right child tree
-- should be >= `n`.
elemSortedTree :: Ord a => a -> Tree a -> Bool
elemSortedTree = error "unimplemented"

-- Write a function that swaps the elements in a tuple in a tree.  Note that
-- this does not swap left children with right children.
swapTree :: Tree (a,b) -> Tree (b,a)
swapTree = error "unimplemented"

-- Write a function that computes the depth of the tree: that is, the largest
-- number of Nodes to traverse on the way to a Leaf.
depth :: Tree a -> Int
depth = error "unimplemented"

-- Write a function that inserts an element into a binary search tree,
-- maintaining the tree invariant.
insertSortedTree :: Ord a => a -> Tree a -> Tree a
insertSortedTree = error "unimplemented"
