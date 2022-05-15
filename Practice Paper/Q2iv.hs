module Q2iv where
import Q2ii (psrn)
import Q2iii (insertMod)
{-
### Q2iv [6 marks] 
Implement a function, `shuffleStep` that calculates a single step of a
shuffle (the next question asks for the whole shuffle). The function
`shuffleStep` takes a function over numbers, a number-list pair, and
an element.  The result is also a number-list pair. The number in the
output pair is given by applying the first argument to the input
number. The list in the output pair is obtained by inserting the
potential element into the list at the position given by the number in
the input pair, following the same rules that `insertMod` follows.

Your solution should satisfy:
-}
shuffleStepTest :: Bool
shuffleStepTest =     shuffleStep id   (         3, "hello")  'x' == (         3, "helxlo")
                   && shuffleStep psrn (1234567890, [0 .. 4]) 9   == ( 395529916, [9,0,1,2,3,4])
                   && shuffleStep psrn (2468013579, [0 .. 4]) 9   == (1257580448, [0,1,2,9,3,4])

shuffleStep :: (Int -> Int) -- function to update integer
            -> (Int, [a]) -- starting integer & list
            -> a -- value to insert in list
            -> (Int, [a]) -- result

shuffleStep next (n, xs) x = (next n, insertMod n xs x) 


tests :: [(Int, Bool)] 
tests = [(2, shuffleStep id (0, "abcde") 'x' == (0,"xabcde")),
         (2, shuffleStep succ (10, "abcde") 'x' == (11,"abcdxe")),
         (2, shuffleStep pred (negate 1, "abcde") 'x' == (negate 2, "abcdex"))]
