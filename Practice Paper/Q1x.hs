module Q1x where -- 8 marks

{-

Recall the definition of the `ProofLayout` type constructor:

-}
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
{-
Consider the definitions of the `Prelude` functions:
```haskell
head :: [a] -> a
head (x:_) = x      -- head.0

foldr :: (a->b->b) -> b -> [a] -> b
foldr _ z []     = z                  -- foldr.0 
foldr f z (x:xs) = f x (foldr f z xs) -- foldr.1

const :: a -> b -> a
const x y = x -- const.0
```
Define:
-}
caput :: [a] -> a
caput = foldr const undefined -- caput.0
{-
Give a value of type `ProofLayout a` that captures the proof of
```haskell
∀ x::a, xs :: [a], {caput (x:xs) == head (x:xs)}
```
For full marks, each step must be annotated with the rule that justifies the step.
-}
caputHead :: a -> [a] -> ProofLayout a
caputHead x xs =
  caput (x:xs)
  :=: -- caput.0
  foldr const undefined (x:xs)
  :=: -- foldr.1
  const x (foldr const undefined xs)
  :=: -- const.0
  x
  :=: -- head.0
  head (x:xs)
  :=: QED
