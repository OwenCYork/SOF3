module Q1viia where -- 1 mark

{-

Consider the type

-}

type BoolD a = (Bool, a) 

{-

A value `(True, x)` is to be interpreted as `x` is a valid result and `(False, x)` as `x` is invalid and should not be used.  A better way of representing such values is to use a `Maybe` type.  

Write a function `bd2m` that converts `BoolD a` to `Maybe a`.

Your solution should satisfy:

-}

testbd2m :: Bool 
testbd2m = 
 (bd2m (True, 55)      == Just 55)     &&
 (bd2m (True, "SOF3")  == Just "SOF3") &&
 (bd2m (False, "SOF1") == Nothing)     &&
 (bd2m (False, 34)     == Nothing)



bd2m :: BoolD a -> Maybe a
bd2m (True, x)  = Just x
bd2m _          = Nothing

testc' :: Bool
-- 1 mark
testc' = 
 (bd2m (True, 55)               == Just 55)     &&
 (bd2m (True, "SOF3")           == Just "SOF3") &&
 (bd2m (False, "SOF1")          == Nothing)     &&
 (bd2m (False, 34)              == Nothing)     &&
 (bd2m (True, [1..10])          == Just [1,2,3,4,5,6,7,8,9,10])  &&
 (bd2m (False, [1..10])         == Nothing)     &&
 (bd2m (True, (reverse "SOF3")) == Just "3FOS") &&
 (bd2m (True, [1])              == Just [1])    &&
 (bd2m (False, ['a'])           == Nothing)
 
