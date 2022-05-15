module Q2v where

import Q2ii (psrn) -- for `shuffleTest` only
import Q2iv (shuffleStep)

{-

### 2v [10 marks]
We can get the effect of randomising, or _shuffling_, of a list, by
repeatedly taking an element of a list and inserting it into a second,
initially empty, list at a random position.  To obtain the stream of
random numbers, a _seed_, or starting point, and a _next-number_
function are parameters of the function. Implement the `shuffle`
function.

Your solution should satisfy:
-}
shuffleTest :: Bool
shuffleTest =    shuffle psrn 1234567890 [0 .. 9] == [9,4,1,0,3,7,6,5,8,2]
              && shuffle psrn 2468013579 [0 .. 9] == [2,4,1,3,5,8,9,6,0,7]
              && shuffle id            0 [0 .. 9] == [9,8,7,6,5,4,3,2,1,0]
              && shuffle (+1)          0 [0 .. 9] == [0,1,2,3,4,5,6,7,8,9]

shuffle :: (Int -> Int) -- function to update integer
           -> Int       -- seed
           -> [a]       -- list to shuffle
           -> [a]       -- result

shuffle next seed = snd . (foldl (shuffleStep next) (seed, []))

tests :: [(Int, Bool)] 
tests = [(2, shuffle id 0 ['a' .. 'e'] == "edcba"),
         (2, shuffle succ 5 ['a' .. 'e'] == "dbcae"),
         (2, shuffle pred 9 [0 .. 9] == [9,4,8,1,7,2,6,3,5,0]),
         (4, shuffle succ 6 "" == "")]
