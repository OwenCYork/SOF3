module Q1vic where -- 2 marks

import Q1via -- for the onBus66 database
{-

Write a function `offBus` that removes the record of a student from the database once he/she gets off the bus. There shouldn't be any change to the database if the record does not exist. 

Your solution should satisfy:
-}

testoffBus :: Bool 
testoffBus = 
 (offBus onBus66 "Jack" 24 "Constantine" == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")])

offBus :: Students -> Name -> Age -> College -> Students
offBus busJ stN stA stC = [sTuple | sTuple <- busJ, sTuple /= (stN, stA, stC)]

-- 1 mark
testc1, testc2 :: Bool
testc1= 
 (offBus onBus66 "Jack" 24 "Constantine" == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")]) &&
 (offBus onBusxy "Peter" 78 "Halifax"    == []) &&
 (offBus onBusxy "Peter" 70 "Halifax"    == [("Peter",78,"Halifax")]) &&
 (offBus onEmptyBus "Peter" 78 "Halifax" == []) &&
 (offBus onBus66 "Peter" 78 "Halifax"    == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")]) &&
 (offBus onBus66 "Bob" 19 "Alcuin"       == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Lui",22,"Goodricke")])

-- 1 mark
testc2 = (offBus onBus66 "Jack" 24 "Constantine" == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")])
 


onBusxy, onEmptyBus :: Students
onBusxy = [("Peter", 78, "Halifax")]
onEmptyBus = []
