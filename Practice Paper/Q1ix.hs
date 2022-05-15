module Q1ix where -- 8 marks

{-

Consider

-}

data DBTree = DBLeaf | DBNode DBTree (Int, String) DBTree deriving (Eq, Show)

smallDB :: DBTree
smallDB = DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Maxwell") (DBNode DBLeaf (6,"Helen") DBLeaf)

{-

In a _binary search tree_ if *k* is the key in a node, then all keys in the left subtree must be less than *k*, and all the keys in the right subtree must be greater than *k*. Consider a database table (like smallDB) with student-id and name pair, represented as a binary search tree. 


Write a function `stdUpdate` which takes a student-id, a new name and a database organised as a binary search tree, and returns a new tree with the student's new name. However, you are expected to return an error message if a valid record with the provided student-id does not exist in the database. You will need to define an appropriate type for 'Error', as part of your solution.

Your solution should satisfy:
-}

testUpdate :: Bool
testUpdate =
  (stdUpdate 6 "Abi" smallDB == Right (DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Maxwell") (DBNode DBLeaf (6,"Abi") DBLeaf))) &&
  (stdUpdate 8 "Mandy" smallDB == Left "There is no such student with ID: 8")

type Error a = Either String a

stdUpdate :: Int -> String -> DBTree -> Error DBTree
stdUpdate sid sname treeDb = 
  if stdSearch sid treeDb then 
    Right (idCheck treeDb) else 
      Left ("There is no such student with ID: " ++ show sid)
                      where
                     idCheck xDb@(DBNode xlf (x, y) xrt)
                         | sid == x = DBNode xlf (x, sname) xrt
                         | sid < x = DBNode (idCheck xlf) (x, y) xrt
                         | sid > x = DBNode xlf (x, y) (idCheck xrt)

stdSearch :: Int -> DBTree -> Bool 
stdSearch sid DBLeaf = False
stdSearch sid (DBNode left (a, b) right) 
              | sid == a = True 
              | sid < a = stdSearch sid left
              | otherwise = stdSearch sid right
              
bigDB :: DBTree
bigDB = DBNode (DBNode (DBNode DBLeaf (0, "Juliana") DBLeaf) (2,"Dubios") DBLeaf) (3,"Caroline") 
   (DBNode (DBNode DBLeaf (5, "Ella") DBLeaf) (6,"Barns") (DBNode DBLeaf (10, "Adam") (DBNode DBLeaf (34, "Fred") DBLeaf)))
   
emptyDB = DBLeaf

testc', testc'' :: Bool
-- 2 marks
testc'=
  (stdUpdate 6 "Abi" smallDB   == 
  Right (DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Maxwell") (DBNode DBLeaf (6,"Abi") DBLeaf))) &&
  (stdUpdate 8 "Mandy" smallDB == 
  Left "There is no such student with ID: 8")

-- 6 marks
testc'' = 
    (stdUpdate 6 "Garry" bigDB        == 
    Right (DBNode (DBNode (DBNode DBLeaf (0,"Juliana") DBLeaf) (2,"Dubios") DBLeaf) (3,"Caroline") (DBNode (DBNode DBLeaf (5,"Ella") DBLeaf) (6,"Garry") (DBNode DBLeaf (10,"Adam") (DBNode DBLeaf (34,"Fred") DBLeaf))))) &&
    (stdUpdate 4 "Martine" bigDB      == 
    Left "There is no such student with ID: 4") &&
    (stdUpdate 0 "Nanny" emptyDB      == 
    Left "There is no such student with ID: 0") &&
    (stdUpdate 3 "Murray" smallDB     == 
    Right (DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Murray") (DBNode DBLeaf (6,"Helen") DBLeaf))) &&
    (stdUpdate (-3) "Murray" smallDB  == 
    Left "There is no such student with ID: -3") && 
    (stdUpdate (2*17) "Patrick" bigDB == 
    Right (DBNode (DBNode (DBNode DBLeaf (0,"Juliana") DBLeaf) (2,"Dubios") DBLeaf) (3,"Caroline") (DBNode (DBNode DBLeaf (5,"Ella") DBLeaf) (6,"Barns") (DBNode DBLeaf (10,"Adam") (DBNode DBLeaf (34,"Patrick") DBLeaf)))))
    
