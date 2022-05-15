module Answers where

--Q1i

isHaskell :: String -> Bool
isHaskell "Haskell" = True
isHaskell _ = False

testHaskell :: Bool
testHaskell =
 (isHaskell "Haskell" == True) &&
 (isHaskell "Software" == False) &&
 (isHaskell "" == False) &&
 (isHaskell "haskell" == False)


--Q1ii
lowerVowel :: String -> Bool
lowerVowel = doLowerVowel
 where
  doLowerVowel [] = True
  doLowerVowel (x:xs) | x `elem` "aeiou" = doLowerVowel xs
                      | otherwise = False


testlv:: Bool
testlv = (lowerVowel "ue" == True) &&
 (lowerVowel "ueA" == False) &&
 (lowerVowel "uea" == True)


--Q1iii
prodCube :: [Int] -> Int
prodCube = doProdCube 1
 where
  doProdCube prod [] = prod
  doProdCube prod (x:xs) | x `mod` 4 == 0 = doProdCube prod xs
                         | x `mod` 2 == 0 = doProdCube (prod*(cube x)) xs
                         | otherwise = doProdCube prod xs
  cube x = x*x*x



testprodCube :: Bool
testprodCube = (prodCube [] == 1)
 && (prodCube [4, 8] == 1)
 && (prodCube [4, 6, 8] == 216)
 && (prodCube [4, 6, 8, 12] == 216)
 && (prodCube [2..11] == 1728000)


--Q1iv
consDiff :: String -> Int
consDiff = doConsDiff 0
 where
  doConsDiff diff [] = diff
  doConsDiff diff (x:xs) | x `elem` "QWRTYPSDFGHJKLZXCVBNMqwrtypsdfghjklzxcvbnm" = doConsDiff (diff+1) xs
                         | x `elem`"1234567890" = doConsDiff (diff-1) xs
                         | otherwise = doConsDiff diff xs


testconsDiff :: Bool
testconsDiff =
 (consDiff "" == 0)
 && (consDiff "SOf3in2021" == -2)
 && (consDiff "Software123andTheory123" == 5)
 && (consDiff "HASkellprogramming2021" == 9)

 --Q1va
isIATA :: String -> Bool
isIATA = doIsIATA 0
 where
  doIsIATA counter [] | counter == 3 = True
                      | otherwise = False
  doIsIATA counter (x:xs) | counter == 3 = False
                          | x `elem` "BCDFGHJKLMNPQRSTVWXYZ" = doIsIATA (counter + 1) xs
                          | otherwise = False

testisIATA :: Bool
testisIATA =
 (isIATA "" == False) &&
 (isIATA "MAN" == False) &&
 (isIATA "LHR" == True) &&
 (isIATA "LHRT" == False) &&
 (isIATA "lhr" == False) &&
 (isIATA "JFK" == True) &&
 (isIATA "BHX" == True)

--Q1vb

countIATA :: [String] -> Int
countIATA = doCountIATA 0
 where
  doCountIATA counter [] = counter
  doCountIATA counter (x:xs) | isIATA x = doCountIATA (counter+1) xs
                             | otherwise = doCountIATA counter xs

testcountIATA :: Bool
testcountIATA =
 (countIATA ["LHR"] == 1) &&
 (countIATA ["LHR", "Lhr", "MAN", "JFK", "", "jfk"] == 2) &&
 (countIATA ["LHR", "BHX", "MAN", "JFK", "ACC", "LRHT"] == 3)

 --Q1vi
type Students = [(Name, Age, College)]
type Name = String
type Age = Int
type College = String

onBus66 :: Students
onBus66 = [("Zain", 18, "Halifax"), ("Julia", 20, "Constantine"),
 ("Mandy", 22, "Goodricke"), ("Jack", 24, "Constantine"),
 ("Emma", 21, "Langwith"), ("Zack", 19, "Halifax"),
 ("Alice", 21, "Halifax"), ("Bob", 19, "Alcuin"),
 ("Lui", 22, "Goodricke")]

--a
colleges :: Age -> [College]
colleges = doColleges [] onBus66
 where
  doColleges colList [] checkAge = colList
  doColleges colList (x@(name,age,college):xs) checkAge | checkAge == age = doColleges (colList++[college]) xs checkAge
                                                        | otherwise = doColleges colList xs checkAge


--b
joinBus :: Students -> Name -> Age -> College -> Students
joinBus currentBus newName newAge newCollege = currentBus ++ [(newName,newAge,newCollege)]

--c
offBus :: Students -> Name -> Age -> College -> Students
offBus currentBus removeName removeAge removeCollege = doOffBus currentBus removeName removeAge removeCollege []
 where
  doOffBus [] removeName removeAge removeCollege newBus = newBus
  doOffBus (x@(name,age,college):xs) removeName removeAge removeCollege newBus | removeName == name && removeAge == age && removeCollege == college = doOffBus xs removeName removeAge removeCollege newBus
                                                                               | otherwise = doOffBus xs removeName removeAge removeCollege (newBus ++ [x])


--Qvii
type BoolD a = (Bool, a)

--a
bd2m :: BoolD a -> Maybe a
bd2m bd@(bool,x) | bool = Just x
                 | otherwise = Nothing

--b
bDSum :: String -> BoolD Int
bDSum = doBDSum 0
 where
  doBDSum total [] | total == 0 = (False,0)
                   | otherwise = (True, total)
  doBDSum total (x:xs) | x `elem` "1234567890" = doBDSum ((read [x] :: Int) + total) xs
                       | otherwise = doBDSum total xs

--c
mBSum :: String -> Maybe Int
mBSum x = bd2m (bDSum x)

--Q1viii
data TreeP a = Leaf Int | Node (TreeP a) a (TreeP a) deriving (Eq, Show)

emptyTreeP :: TreeP a
emptyTreeP = Leaf 0



--Q1x
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
caput :: [a] -> a
caput = foldr const undefined -- caput.0

caputHead :: a -> [a] -> ProofLayout a
caputHead x xs = 
 caput (x:xs)
 :=:
 foldr const undefined (x:xs)
 :=:
 const x (foldr const undefined xs)
 :=:
 x
 :=:
 head (x:xs)
 :=: QED