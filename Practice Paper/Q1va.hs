module Q1va where -- 2 mark

{-

The International Air Transport Association's (IATA) Location Identifier is a unique and all uppercase 3-letter code used in aviation to identify an airport, for example LHR, JFK and BHX. 

For the purposes of this question, IATA location identifier should consist of only consonants "BCDFGHJKLMNPQRSTVWXYZ". 

Write a function `isIATA` that tests whether or not a string is an IATA location identifier. 

Your solution should satisfy:

-}
testisIATA :: Bool 
testisIATA =
    (isIATA ""     == False) &&
    (isIATA "MAN"  == False) &&
    (isIATA "LHR"  == True) &&
    (isIATA "LHRT" == False) &&
    (isIATA "lhr"  == False) &&
    (isIATA "JFK"  == True) &&
    (isIATA "BHX"  == True)  


isUpperCons :: Char -> Bool
isUpperCons c = c `elem` "BCDFGHJKLMNPQRSTVWXYZ"

isIATA :: String -> Bool 
isIATA airCode = length airCode == 3 && 
 and [isUpperCons c | c <- airCode]


testc', testc'' :: Bool
-- 1 mark
testc' =
    (isIATA ""     == False) &&
    (isIATA "MAN"  == False) &&
    (isIATA "LHR"  == True)  &&
    (isIATA "LHRT" == False) &&
    (isIATA "lhr"  == False) &&
    (isIATA "JFK"  == True)  &&
    (isIATA "BHX"  == True)  

-- 1 mark    
testc'' =
    (isIATA "   "     == False) &&
    (isIATA "M1K"     == False) &&
    (isIATA "XYZ"     == True)  &&
    (isIATA "MYWHH"   == False) &&
    (isIATA "lhr"     == False) &&
    (isIATA "QFG"     == True)  &&
    (isIATA "CDM"     == True)  &&
    (isIATA "TVW"     == True)  &&
    (isIATA "cmd"     == False) &&
    (isIATA "NPS"     == True)
