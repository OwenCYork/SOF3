module Q1ii where -- 1 mark

{-

Write a function `lowerVowel` that takes a string and returns `True` if the string has only lowercase vowels (a, e, i, o, u) and `False` otherwise.  

Your solution should satisfy:

-}
testlv, testlv' :: Bool
testlv = (lowerVowel "ue"  == True)  &&
         (lowerVowel "ueA" == False) &&
         (lowerVowel "uea" == True)


lowerVowel:: String -> Bool 
lowerVowel = all (`elem` enVowels)
      where
          enVowels = "aeiou"

-- 1 mark
testlv' = 
    (lowerVowel ""                  == True) &&
    (lowerVowel "uet"               == False) &&
    (lowerVowel " uea"              == False) &&
    (lowerVowel "aaa eee"           == False) &&
    (lowerVowel "abeIou"            == False) &&
    (lowerVowel "uoiea"             == True)  &&
    (lowerVowel "iiiiaaaaeeeooouuu" == True)  &&
    (lowerVowel "oooEaaauuu"        == False) &&
    (lowerVowel "iii0uuuu"          == False) &&
    (lowerVowel "eeeIooou"          == False) &&
    (lowerVowel "aaaeeeiiUuuo"      == False) &&
    (lowerVowel "uuuaaaeeeOooiiI"   == False)
