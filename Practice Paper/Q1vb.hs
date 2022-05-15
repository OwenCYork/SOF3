module Q1vb where -- 2 mark

{-
You may use your answer to Q1va for `countIATA`.
If you chose to do so, uncomment the following line
-}
import Q1va (isIATA) 


{-

The International Air Transport Association's (IATA) Location Identifier is a unique and all uppercase 3-letter code used in aviation to identify an airport, for example LHR, JFK and BHX. 

For the purposes of this question, IATA location identifier should consist of only consonants "BCDFGHJKLMNPQRSTVWXYZ". 

Write a function `countIATA` that counts the number of IATA location identifiers in a list of strings. 

Your solution should satisfy:

-}

testcountIATA :: Bool 
testcountIATA = 
    (countIATA ["LHR"]                                     == 1) &&
    (countIATA ["LHR", "Lhr", "MAN", "JFK", "", "jfk"]     == 2) &&
    (countIATA ["LHR", "BHX", "MAN", "JFK", "ACC", "LRHT"] == 3)


countIATA :: [String] -> Int
countIATA ss = length [s | s <- ss, isIATA s]

-- 1 mark
testc', testc'' :: Bool
testc' = 
    (countIATA ["LHR"]                                     == 1) &&
    (countIATA ["LHR", "Lhr", "MAN", "JFK", "", "jfk"]     == 2) &&
    (countIATA ["LHR", "BHX", "MAN", "JFK", "ACC", "LRHT"] == 3)

-- 1 mark    
testc'' = 
    (countIATA ["LHR", "   "]                                  == 1) &&
    (countIATA ["M1K", "XYZ", "MYWHH", "lhr", "QFG", "JFK"]    == 3) &&
    (countIATA ["CDM", "TVW", "cmd", "NPS", "nps"]             == 3) &&
    (countIATA ["cmd", "twv", "YML", "ZZZ", "zzz", "BBBR", ""] == 2)

