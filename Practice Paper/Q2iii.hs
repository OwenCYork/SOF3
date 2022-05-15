module Q2iii where
{-
### Q2iii [5 marks]
Implement a function, `insertMod`, to insert an element into a list at
a given position. If the given position is larger than the length of
the list then insertion should be at the position found by taking the
input modulo the number of insertion positions.

Your solution should satisfy:
-}
insertModTest :: Bool
insertModTest =    insertMod    0 "hello" 'x' == "xhello"
                && insertMod    5 "hello" 'x' == "hellox"
                && insertMod    3 [0..4]  9   == [0, 1, 2, 9, 3, 4]
                && insertMod   24 "hello" 'x' == "xhello"
                && insertMod  131 "hello" 'x' == "hellox"
                && insertMod 1011 [0..4]  9   == [0, 1 ,2, 9, 3 ,4]

insertMod :: Int -> [a] -> a -> [a]
-- solution
insertMod n xs x = left ++ x:right
  where
    (left, right) = splitAt (n `mod` (length xs + 1)) xs

tests :: [(Int, Bool)] 
tests = [(2, insertMod 0 ""    'x' == "x"),
         (1, insertMod 0 "abc" 'x' == "xabc"),
         (1, insertMod 1 "abc" 'x' == "axbc"),
         (1, insertMod 9 "abc" 'x' == "axbc")]
