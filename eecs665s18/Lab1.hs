module Lab1 where

import Test.HUnit
import Prelude hiding (reverse)

{-------------------------------------------------------------------------------

LAB 1: Haskell review

DUE: Thursday, February 1, 11:59 PM

This laboratory will allow you to review and demonstrate your familiarity with
Haskell.  You will be asked to write a number of small Haskell functions,
ranging from the simple to the not-as-simple.  Each function you are asked to
write will also come with two test cases; for full credit, you must provide at
least two more test cases.  (You may provide more than two if you find it
helpful when writing your solutions.)  The tests are written using HUnit, so you
will have to load ghci with -package HUnit.

-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------

Part 1: Numbers and digits

-------------------------------------------------------------------------------}

-- Write a function which returns the last digit of its input number.  For
-- example, `lastDigit 89` should return `9`.
lastDigit :: Int -> Int
lastDigit = error "unimplemented"

lastDigitTests =
    TestList [ "last digit of 100" ~: lastDigit 100 ~?= 0
             , "last digit of 0" ~: lastDigit 0 ~?= 0
             -- Your tests go here
             ]

-- Write a function which returns its input number, but missing its last digit.
-- For example, `lastDigit 289` should return `28`.
allButLastDigit :: Int -> Int
allButLastDigit = error "unimplemented"

allButLastDigitTests =
    TestList [ "all but last digit of 100" ~: allButLastDigit 100 ~?= 10
             , "all but last digit of 0" ~: allButLastDigit 0 ~?= 0
             -- Your tests go here
             ]

-- Write a function which returns the digits of its input.  For example, `digits
-- 289` should return `[2,8,9]`.  You may assume that the input list is
-- non-negative!
digits :: Int -> [Int]
digits = error "unimplemented"

digitsTests =
    TestList [ "digits of 100" ~: digits 100 ~?= [1, 0, 0]
             , "digits of 289" ~: digits 289 ~?= [2, 8, 9]
             -- Your tests go here
             ]

-- Write a function which reverses its input list.  For example, `reverse
-- [2,8,9]` should return `[9,8,2]`.
reverse :: [Int] -> [Int]
reverse = error "unimplemented"

reverseTests =
    TestList [ "reverse of [1, 0, 0]" ~: reverse [1, 0, 0] ~?= [0, 0, 1]
             , "reverse of []" ~: reverse [] ~?= []
             -- Your tests go here
             ]

-- Write a function which returns true if its input number is a palindrome.  A
-- palindrome is something which is the same backwards and forwards.  In this
-- case, you should have `palindrome 2442` returning `True`, but `palindrome 24`
-- returning `False`.
palindrome :: Int -> Bool
palindrome = error "unimplemented"

palindromeTests =
    TestList [ "2442 is a palindrome" ~: palindrome 2442 ~?= True
             , "89 is not a palindrome" ~: palindrome 89 ~?= False
             -- Your tests go here
             ]

-- Write a function which reverses the input digits of its argument.  For
-- example, `reverseDigits 289` should return `982`.
reverseDigits :: Int -> Int
reverseDigits = error "unimplemented"

reverseDigitsTests =
    TestList [ "289 reversed" ~: reverseDigits 289 ~?= 982
             , "101 reversed" ~: reverseDigits 101 ~?= 101
             -- Your tests go here
             ]

{-------------------------------------------------------------------------------

Part 2: Lists

--------------------------------------------------------------------------------}

-- Write a function which, given a list of integers, returns the sum of the
-- squares of those integers.  For example, sumSquares [3, 4, 5] should return
-- 50.

sumSquares :: [Int] -> Int
sumSquares = error "sumSquares not defined"

sumSquaresTests =
    TestList [ "squares of [1,2,3]" ~: sumSquares [1,2,3] ~?= 14
             , "squares of [2,3,4]" ~: sumSquares [2,3,4] ~?= 29
             -- Your tests go here
             ]

-- Write a function which, given a list of integers, returns the sum of the
-- squares of the even integers in its input list.  For example, sumSquaredEvens
-- [3, 4, 5] should return 16.

sumSquaredEvens :: [Int] -> Int
sumSquaredEvens = error "sumSquaredEvens not defined"

sumSquaredEvensTests =
    TestList [ "squares of evens from [1,2,3]" ~: sumSquaredEvens [1,2,3] ~?= 4
             , "squares of evens from [2,3,4]" ~: sumSquaredEvens [2,3,4] ~?= 20
             -- Your tests go here
             ]

-- Write a function which, given a list of integers, returns True if the list is
-- sorted in ascending order, and False otherwise.  For example, isSorted
-- [2,3,3,4] should return True, and isSorted [3,2,3,4] should return False.

isSorted :: [Int] -> Bool
isSorted = error "isSorted not defined"

isSortedTests =
    TestList [ "[1,2,3] is sorted" ~: isSorted [1,2,3] ~?= True
             , "[3,2,1] is not sorted" ~: isSorted [3,2,1] ~?= False
             -- Your tests go here
             ]

-- Write a function which, given a list of integers, returns the second largest
-- value in the list.  For example: secondLargest [3,6,5] should return 5.  You
-- may assume that the input list will contain at least 2 elements.  If all list
-- values are equal, you should return that value; however, otherwise, multiple
-- occurrences of the same value should not be counted multiple times.  For
-- example, secondLargest [6,5,6] should still return 5, not 6.

secondLargest :: [Int] -> Int
secondLargest = error "secondLargest not defined"

secondLargestTests =
    TestList [ "second largest of [4,2,5,1]" ~: secondLargest [4,2,5,1] ~?= 4
             , "second largest of [5,6,6]" ~: secondLargest [5,6,6] ~?= 5
             -- Your tests go here
             ]

{-------------------------------------------------------------------------------

Test drivers

-------------------------------------------------------------------------------}

allTests = TestList [ lastDigitTests, allButLastDigitTests, digitsTests,
                      reverseTests, palindromeTests, reverseDigitsTests,
                      sumSquaresTests, sumSquaredEvensTests, isSortedTests,
                      secondLargestTests ]
check = runTestTT allTests
