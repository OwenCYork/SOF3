module Q1i where -- 1 mark

{-

Write a function `isHaskell` that takes a string and returns `True` if the string is "Haskell" and `False` otherwise. 

Your solution should satisfy:

-}

testHaskell, testHaskell'' :: Bool
testHaskell =
 (isHaskell "Haskell"  == True)  &&
 (isHaskell "Software" == False) &&
 (isHaskell ""         == False) &&
 (isHaskell "haskell"  == False)


isHaskell :: String -> Bool 
isHaskell st = st == "Haskell"

-- 1 mark

testHaskell'' =
 (isHaskell "Haskell!" == False) &&
 (isHaskell "1Haskell" == False) &&
 (isHaskell "haskeLL"  == False) &&
 (isHaskell "HASKELL"  == False) &&
 (isHaskell " Haskell" == False) &&
 (isHaskell "Haskell " == False)
