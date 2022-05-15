module Q1viic where -- 1 mark

{-
You may use your answers to Q1viia and Q1viib for `mBSum`.
If you chose to do so, uncomment the following line
-}
import Q1viia
import Q1viib


{-

Write a function `mBSum` which uses the function `bd2m` and `bDSum`to return a `Maybe` type from the sum of all digits in a list of characters as described in question (b). 
Your solution should satisfy:
-}

testmBSum :: Bool 
testmBSum = (mBSum "sof3the3" == Just 6)  &&
  (mBSum "software"           == Nothing) &&
  (mBSum ""                   == Nothing) &&
  (mBSum "cos0"               == Just 0)


mBSum :: String -> Maybe Int 
mBSum = bd2m . bDSum  

testc1' :: Bool
-- 1 mark
testc1' = 
    (mBSum "sof3the3"                            == Just 6)  &&
    (mBSum "software"                            == Nothing) &&
    (mBSum ""                                    == Nothing) &&
    (mBSum "cos0"                                == Just 0)  && 
    (mBSum " "                                   == Nothing) &&
    (mBSum "000OOO"                              == Just 0)  &&
    (mBSum "This is one"                         == Nothing) &&
    (mBSum "1+2+3+4+5+6"                         == Just 21) &&
    (mBSum "Haskell300is45"                      == Just 12) &&
    (mBSum "[1..9]"                              == Just 10) &&
    (mBSum ['3','e','4','r','6']                 == Just 13) &&
    (mBSum ['!','8','£','7',')','e','4','r','6'] == Just 25) &&
    (mBSum "<>()*&^!ue"                          == Nothing)
