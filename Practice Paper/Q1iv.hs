module Q1iv where -- 2 marks

{-

Write a function `consDiff` that computes the difference between the number of consonants "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ" and the number of digit characters in a string. 
Your solution should satisfy:

-}
testconsDiff :: Bool 
testconsDiff = 
    (consDiff ""                        == 0)
    && (consDiff "SOf3in2021"           == -2)
    && (consDiff "Software123andTheory123" == 5)
    && (consDiff "HASkellprogramming2021"  == 9)


isDigit :: Char -> Bool
isDigit n = n `elem` "0123456789"

isCons :: Char -> Bool
isCons c = c `elem` "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"

consDiff :: String -> Int
consDiff cs = (length [ c | c <- cs, isCons c ]) 
  - (length [ c | c <- cs, isDigit c ])

testc', testc'' :: Bool
-- 1 mark
testc' = 
    (consDiff ""                           == 0)
    && (consDiff "SOf3in2021"              == -2)
    && (consDiff "Software123andTheory123" == 5)
    && (consDiff "HASkellprogramming2021"  == 9)
-- 1 mark   
testc'' = 
    (consDiff "abc12"                  == 0)
    && (consDiff "strength00122"       == 2)
    && (consDiff "fada95610asamoah6"   == -1)
    && (consDiff "27ho74keta3nyakwan"  == 3)
