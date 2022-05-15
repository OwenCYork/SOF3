module Q1vib where -- 2 mark

import Q1via -- for the onBus66 database
{-
You may use your answer to Q1via for `joinBus`.
If you chose to do so, uncomment the following line
-}
-- import Q1via
{-

Write a function `joinBus` that adds the record of a student to the database once the student gets onboard the bus. 

Your solution should satisfy:
-}

testjoinBus :: Bool 
testjoinBus = 
 (joinBus onBus66 "Adam" 19 "Constantine"  == 
 [("Adam",19,"Constantine"),("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")])



joinBus :: Students -> Name -> Age -> College -> Students
joinBus busJ stN stA stC = (stN, stA, stC) : busJ

testc2 :: Bool
-- 2 marks 
testc2 = 
 (joinBus onBus66 "Adam" 19 "Constantine"  == 
 [("Adam",19,"Constantine"),("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")]) &&
 (joinBus onBusxy "Jenny" 22 "Alcuin"      == 
 [("Jenny",22,"Alcuin"),("Peter",78,"Halifax")])                        &&
 (joinBus onEmptyBus "Matt" 77 "Langwith"  == [("Matt",77,"Langwith")]) &&
 (joinBus onBus66 "Simon" 23 "Langwith"    == 
 [("Simon",23,"Langwith"),("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")])
 
  
  
onBusxy, onEmptyBus :: Students
onBusxy = [("Peter", 78, "Halifax")]
onEmptyBus = []
