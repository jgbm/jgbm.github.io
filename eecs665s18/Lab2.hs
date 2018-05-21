module Lab1 where

import Test.HUnit

{-------------------------------------------------------------------------------

LAB 2: Haskell review

DUE: Thursday, February 8, 11:59 PM

This laboratory will allow you to review and demonstrate your familiarity with
Haskell.  You will be asked to write a number of small Haskell functions,
ranging from the simple to the not-as-simple.  Each function you are asked to
write will also come with two test cases; for full credit, you must provide at
least two more test cases.  (You may provide more than two if you find it
helpful when writing your solutions.)  The tests are written using HUnit, so you
will have to load ghci with -package HUnit.  Please consider filling out the
short survey at the end of the assignment; however, you do not have to fill it
out to receive full credit.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

BOOLEAN FORMULAS

We start with a Haskell datatype capturing Boolean formulas.  We include cases
for atomic propositions (Atom), negations (Not), conjunctions (:/\:), and
disjunctions (:\/:).  For example, the formula "A or not A" would be represented
by the Haskell value `Atom "A" :\/: Not (Atom "A")`.  We are not concerned with
quantifiers, and will assume that implications have already be rewritten in
terms of /\, \/, and ~.

The `printFormula` function produces a more readable textual representation of
Formula values.  Note that it doesn't group disjunctions or conjunctions, so
both `Atom "A" :/\: (Atom "A" :/\: Atom "A")` and `(Atom "A" :/\: Atom "A") :/\:
Atom "A"` will print as "A /\ A /\ A".

--------------------------------------------------------------------------------}

data Formula = Atom String | Not Formula | Formula :/\: Formula | Formula :\/: Formula
  deriving Show

-- Provided testing code

printFormula phi = putStrLn (p 0 phi)
    where p _ (Atom s) = s
          p _ (Not x) = "~" ++ p 3 x
          p n (phi :\/: psi)
              | n == 0 || n == 1 = p 1 phi ++ " \\/ " ++ p 1 psi
              | otherwise = "(" ++ p 1 phi ++ " \\/ " ++ p 1 psi ++ ")"
          p n (phi :/\: psi)
              | n == 0 || n == 2 = p 2 phi ++ " /\\ " ++ p 2 psi
              | otherwise = "(" ++ p 2 phi ++ " /\\ " ++ p 2 psi ++ ")"


{-

Your first task is to write a function that, given truth values for the atomic
propositions, produces a truth value for an entire formula.  The truth values
for the atomic propositions are provided as a list of string-Boolean pairs.  The
'lookup' function (from the standard Prelude) can be used to find values in this
list; your evaluation function should fail if it encounters an atomic
proposition whose truth value has not been provided.

-}

eval :: [(String, Bool)] -> Formula -> Bool
eval = error "eval not implemented"

evalTests =
    TestList [ "A /\\ (B \\/ ~B) with A = True" ~: eval [("A", True), ("B", True)] (Atom "A" :/\: (Atom "B" :\/: Not (Atom "B"))) ~?= True
             , "A /\\ (B \\/ ~B) with A = False" ~: eval [("A", False), ("B", True)] (Atom "A" :/\: (Atom "B" :\/: Not (Atom "B"))) ~?= False
             -- Your tests go here
             ]

{-------------------------------------------------------------------------------

Negation normal form (NNF) refers to a form of Boolean formulae in which
negation is applied only to atomic predicates, never to conjunctions,
disjunctions, or other negations.  For example, this formula is in NNF:

    A \/ (~B /\ ~C)

while this formula is not

    A \/ ~(B \/ C)

Your second task is to write a function ``nf` which transforms arbitrary input
Boolean formulae into NNF.  You will need the following equations (where A, B,
and C are arbitrary formulae):

  De Morgan's Laws:
    ~(A /\ B) == ~A \/ ~B
    ~(A \/ B) == ~A /\ ~B

  Double negation elimination:
    ~~A == A

You do not need to perform other simplifications (such as reducing A /\ A to A
or reducing A \/ ~A to True).  For testing, I have provided a function isNnf,
which returns True if its input is in disjunctive normal form and False
otherwise.  You should not need to use the isNnf function in your implementation
of nnf (although you are welcome to if you would like).

-------------------------------------------------------------------------------}

nnf :: Formula -> Formula
nnf = error "nnf not implemented"

isNnf (a :/\: b)     = isNnf a && isNnf b
isNnf (a :\/: b)     = isNnf a && isNnf b
isNnf (Not (Atom _)) = True
isNnf (Atom _)       = True
isNnf _              = False

nnfTests =
    TestList [ "double negation" ~: isNnf (nnf (Not (Not (Atom "x")))) ~?= True
             , "de Morgan's laws" ~: isNnf (nnf (Not (Atom "x" :\/: Atom "y"))) ~?= True
             -- Your tests go here
             ]

{-------------------------------------------------------------------------------

Disjunctive normal form (DNF) refers to a form of Boolean formulae which is in
negation normal form and in which no disjunction appears inside a conjunction.
For example, this formula is in DNF:

   A \/ (B /\ C) \/ (B /\ ~D)

while this formula is not:

   A \/ (B /\ (C \/ D))

nor is this one:

   A \/ (B /\ ~(C /\ D))

Your second task is to write a function dnf which transforms input Boolean
formulae *in NNF* into Boolean formulae in DNF.

Your implementation will need the following equations (A,B, and C are arbitrary
formulae)

  Distribution of /\ over \/:
    A /\ (B \/ C) == (A /\ B) \/ (A /\ C)
    (A \/ B) /\ C == (A /\ C) \/ (B /\ C)

You do not need to perform other simplifications (such as reducing A /\ A to A
or reducing A \/ ~A to True).  For testing, I have provided a function isDnf,
which returns True if its input is in disjunctive normal form and False
otherwise.  You should not need to use the isDnf function in your implementation
of dnf (although you are welcome to if you would like).

-------------------------------------------------------------------------------}


dnf :: Formula -> Formula
dnf = error "dnf not implemented"

isDnf phi = isDnf' 0 phi
    where isDnf' _ (Atom x)       = True
          isDnf' _ (Not (Atom x)) = True
          isDnf' 0 (phi :\/: psi) = isDnf' 0 phi && isDnf' 0 psi
          isDnf' 0 (phi :/\: psi) = isDnf' 1 phi && isDnf' 1 psi
          isDnf' 1 (phi :/\: psi) = isDnf' 1 phi && isDnf' 1 psi
          isDnf' _ _              = False

dnfTests =
    TestList [ "right distribution" ~: isDnf (dnf (Atom "A" :/\: (Atom "B" :\/: Atom "C"))) ~?= True
             , "left distribution" ~: isDnf (dnf ((Atom "A" :\/: Atom "B") :/\: Atom "C")) ~?= True
             -- Your tests go here
             ]


allTests = TestList [evalTests, nnfTests, dnfTests]
check = runTestTT allTests
