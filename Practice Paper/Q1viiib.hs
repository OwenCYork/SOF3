module Q1viiib where -- 3 marks
import Q1viiia -- for data TreeP
{-

Write a function `treeList` that returns a list with the values of a regular, ordered
tree in ascending order. 

Your solution should satisfy:
-}
testList :: Bool
testList = (treeList (Node (Node (Leaf 2) '3' (Node (Leaf 3) '4' (Node (Leaf 4) '6' (Leaf 4)))) 
 '7' (Leaf 1)) == "3467") && (treeList (Node (Node (Node (Node (Leaf 4) 'a' (Node (Leaf 5) 'e' 
 (Leaf 5))) 'f' (Leaf 3)) 'o' (Node (Leaf 3) 'r' (Leaf 3))) 's' (Node (Leaf 2) 't' (Node (Leaf 3) 
 'w' (Leaf 3)))) == "aeforstw")


treeList :: TreeP a -> [a]
treeList (Leaf i) = []
treeList tree@(Node left y right) = treeList left ++ [y] ++ treeList right

testc1', testc2'' :: Bool
-- 1 mark
testc1' = 
      (treeList (Node (Node (Leaf 2) '3' (Node (Leaf 3) '4' (Node (Leaf 4) '6' (Leaf 4)))) '7' (Leaf 1)) 
                       == "3467")      && 
      (treeList (Node (Node (Node (Node (Leaf 4) 'a' (Node (Leaf 5) 'e' (Leaf 5))) 'f' (Leaf 3)) 'o' (Node (Leaf 3) 'r' (Leaf 3))) 's' (Node (Leaf 2) 't' (Node (Leaf 3) 'w' (Leaf 3)))) 
                       == "aeforstw")
-- 2 marks 
testc2'' = 
      (treeList (Node (Node (Node (Leaf 3) ' ' (Node (Node (Leaf 5) '!' (Leaf 5)) 'd' (Leaf 4))) 'e' (Leaf 2)) 'h' (Node (Leaf 2) 'l' (Node (Leaf 3) 'o' (Node (Node (Leaf 5) 'r' (Leaf 5)) 'w' (Leaf 4))))) 
                       == " !dehlorw") &&
      (treeList sTree  == [6])         &&
      (treeList sTree' == [6,9])       &&
      (treeList cTree  == "adefhj")


--sTree = Node (Leaf 1) 6 (Leaf 1)
--sTree' = Node (Leaf 1) 6 (Node (Leaf 2) 9 (Leaf 2))
--cTree = Node (Node (Node (Leaf 3) 'a' (Leaf 3)) 'd' (Node (Leaf 3) 'e' (Node (Leaf 4) 'f' (Leaf 4)))) 'h' (Node (Leaf 2) 'j' (Leaf 2))
