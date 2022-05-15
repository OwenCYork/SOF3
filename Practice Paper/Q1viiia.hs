module Q1viiia where -- 6 marks

{-
Consider
-}

data TreeP a = Leaf Int | Node (TreeP a) a (TreeP a) deriving (Eq, Show)

{-

In a _regular tree_ the value in a leaf is the length of the path from
the root to the leaf.  Hence we define:

-}

emptyTreeP :: TreeP a
emptyTreeP = Leaf 0

{-

In an _ordered_ tree, all the values in the left-hand subtree are
strictly smaller than the value at the root, all the values in the
right-hand subtree are strictly greater than the value at the root,
and both subtrees are ordered.


Write a function `itp` to insert a value into a regular, ordered
tree.

Your solution should satisfy:
-}
testItp :: Bool
testItp = foldl (flip itp) emptyTreeP "hello world!"
          == Node (Node (Node (Leaf 3)
                              ' '
                              (Node (Node (Leaf 5)
                                          '!'
                                          (Leaf 5))
                                    'd'
                                    (Leaf 4)))
                        'e'
                        (Leaf 2))
                  'h'
                  (Node (Leaf 2)
                        'l'
                        (Node (Leaf 3)
                              'o'
                              (Node (Node (Leaf 5)
                                          'r'
                                          (Leaf 5))
                                    'w'
                                    (Leaf 4))))



itp :: Ord a => a -> TreeP a -> TreeP a
itp x = itpx
  where
    itpx tree@(Node left y right)
      | x==y  = tree
      | x < y = Node (itp x left) y right
      | x > y = Node left y (itp x right)
    itpx (Leaf n) = Node lsuccn x lsuccn
      where
        lsuccn = Leaf (n+1)


testc', testc'', testc''' :: Bool
-- 1 mark
testc' = foldl (flip itp) emptyTreeP "hello world!"
          == Node (Node (Node (Leaf 3)
                              ' '
                              (Node (Node (Leaf 5)
                                          '!'
                                          (Leaf 5))
                                    'd'
                                    (Leaf 4)))
                        'e'
                        (Leaf 2))
                  'h'
                  (Node (Leaf 2)
                        'l'
                        (Node (Leaf 3)
                              'o'
                              (Node (Node (Leaf 5)
                                          'r'
                                          (Leaf 5))
                                    'w'
                                    (Leaf 4))))

-- 2 marks
testc'' = 
     (itp 2 sTree      == Node (Node (Leaf 2) 2 (Leaf 2)) 6 (Leaf 1)) &&
     (itp 9 sTree      == Node (Leaf 1) 6 (Node (Leaf 2) 9 (Leaf 2))) &&
     (itp 4 emptyTreeP == Node (Leaf 1) 4 (Leaf 1))

-- 3 marks
testc''' = 
     (itp 't' emptyTreeP == Node (Leaf 1) 't' (Leaf 1))                                   &&
     (itp 5 sTree'       == Node (Node (Leaf 2) 5 (Leaf 2)) 6 (Node (Leaf 2) 9 (Leaf 2))) &&
     (itp 'm' cTree      == Node (Node (Node (Leaf 3) 'a' (Leaf 3)) 'd' (Node (Leaf 3) 'e' (Node (Leaf 4) 'f' (Leaf 4)))) 'h' (Node (Leaf 2) 'j' (Node (Leaf 3) 'm' (Leaf 3)))) &&
     (foldl (flip itp) emptyTreeP [2..6] == Node (Leaf 1) 2 (Node (Leaf 2) 3 (Node (Leaf 3) 4 (Node (Leaf 4) 5 (Node (Leaf 5) 6 (Leaf 5))))))
        
--sample TreeP's 
sTree  = Node (Leaf 1) 6 (Leaf 1)
sTree' = Node (Leaf 1) 6 (Node (Leaf 2) 9 (Leaf 2))
cTree  = Node (Node (Node (Leaf 3) 'a' (Leaf 3)) 'd' (Node (Leaf 3) 'e' (Node (Leaf 4) 'f' (Leaf 4)))) 'h' (Node (Leaf 2) 'j' (Leaf 2))


