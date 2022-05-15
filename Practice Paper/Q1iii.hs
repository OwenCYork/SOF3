module Q1iii where -- 2 mark

{-

Write a function `prodCube` that computes the product of the cube of 
numbers in a list that are divisible by 2 but not divisible by 4. 


Your solution should satisfy:

-}
testprodCube :: Bool
testprodCube = (prodCube []     == 1)
     && (prodCube [4, 8]        == 1)
     && (prodCube [4, 6, 8]     == 216)
     && (prodCube [4, 6, 8, 12] == 216)
     && (prodCube [2..11]       == 1728000)


prodCube :: [Int] -> Int
prodCube xs = product [ x^3 | x <- xs, even x, x `mod` 4 /= 0 ]

-- 1 mark
testc', testc'' :: Bool
testc' = (prodCube []           == 1)
     && (prodCube [4, 8]        == 1)
     && (prodCube [4, 6, 8]     == 216)
     && (prodCube [4, 6, 8, 12] == 216)
     && (prodCube [2..11]       == 1728000)  

-- 1 mark
testc'' = (prodCube [1,3,4,5]       == 1)
     && (prodCube [-5..5]           == -64)
     && (prodCube [-10..5]          == -13824000)
     && (prodCube [-5..10]          == -13824000)
     && (prodCube (map (*2) [2..5]) == 216000)  
