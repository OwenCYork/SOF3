module Q1viib where -- 3 marks
import Q1viia -- for type BoolD
{-

Write a function `bDSum` that returns a `BoolD Int`; the sum of all digits in a list of characters. Assume the sum is zero for a list without digits.

Your solution should satisfy:
-}

testbDSum :: Bool 
testbDSum = 
 (bDSum "sof3the3" == (True,6)) &&
 (bDSum "software" == (False,0)) &&
 (bDSum ""         == (False,0)) &&
 (bDSum "cos0"     == (True,0))


bDSum :: String -> BoolD Int 
bDSum ost
        |null(filterD ost) = (False , 0)
        |otherwise = (True,  addC $ filterD ost)

filterD :: String -> String
filterD st = [s | s <- st, isDigit s] 

addC :: String -> Int
addC = foldr (\s -> (+) (read [s] :: Int)) 0

isDigit :: Char -> Bool
isDigit n = n `elem` "0123456789"

testc1, testc2 :: Bool
-- 1 mark
testc1 = (bDSum "sof3the3" == (True,6)) &&
 (bDSum "software"         == (False,0)) &&
 (bDSum ""                 == (False,0)) &&
 (bDSum "cos0"             == (True,0))

-- 2 marks
testc2 = (bDSum " "              == (False,0)) &&
    (bDSum "000OOO"              == (True,0))  &&
    (bDSum "This is one"         == (False,0)) &&
    (bDSum "1+2+3+4+5+6"         == (True,21)) &&
    (bDSum "Haskell300is45"      == (True,12)) &&
    (bDSum "[1..9]"              == (True,10)) &&
    (bDSum ['3','e','4','r','6'] == (True,13)) &&
    (bDSum ['!','8','£','7',')','e','4','r','6'] 
                                 == (True,25))
