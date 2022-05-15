module Q1via where -- 1 mark

{-

Consider the type of students record `Students`: 

-}

type Students = [(Name, Age, College)]
type Name = String 
type Age = Int 
type College = String 

{-

A bus will normally carry students between campuses and records of all the students travelling on a particular bus are maintained. If a student boards the bus, his/her record is added to the bus database and the record is removed when the student gets off the bus. Below is the current state of bus 66 in a form of a database with students records `onBus66`. 

-}

onBus66 :: Students
onBus66 = [("Zain", 18, "Halifax"), ("Julia", 20, "Constantine"), ("Mandy", 22, "Goodricke"),
          ("Jack", 24, "Constantine"), ("Emma", 21, "Langwith"), ("Zack", 19, "Halifax"),
          ("Alice", 21, "Halifax"), ("Bob", 19, "Alcuin"), ("Lui", 22, "Goodricke")]

{-
Write a function `colleges` that takes an age and returns a list of all colleges with students currently onboard bus 66 with that age. 

Your solution should satisfy:
-}

testcolleges :: Bool 
testcolleges = 
  (colleges onBus66 19 == ["Halifax","Alcuin"]) &&
  (colleges onBus66 20 == ["Constantine"]) &&
  (colleges onBus66 21 == ["Langwith","Halifax"])
  

colleges :: Students -> Age -> [College] 
colleges busJ stAge = [c | (n, a, c) <- busJ, a == stAge]

-- 1 mark
testc' :: Bool
testc'  = 
  (colleges onBus66 19 == ["Halifax","Alcuin"]) &&
  (colleges onBus66 20 == ["Constantine"]) &&
  (colleges onBus66 21 == ["Langwith","Halifax"]) &&
  (colleges onBusxx 78 == ["Halifax","Goodricke"]) &&
  (colleges onBusxx 44 == ["Constantine","Halifax","Alcuin","Constantine"]) &&
  (colleges onBusxx 96 == ["Goodricke"]) &&
  (colleges onBusxx 19 == ["Langwith"]) &&
  (colleges onBusxx 71 == ["Constantine","Langwith","Halifax","Halifax"]) &&
  (colleges onBusxx 18 == []) &&
  (colleges onBusxx 22 == ["Alcuin"])   

onBusxx :: Students
onBusxx = [("Peter", 78, "Halifax"), ("Mary", 44, "Constantine"), ("Paul", 96, "Goodricke"),
          ("Janet", 71, "Constantine"), ("Jayju", 71, "Langwith"), ("Matt", 44, "Halifax"),
          ("Aaron", 71, "Halifax"), ("Collins", 44, "Alcuin"), ("Shuliang", 78, "Goodricke"), 
          ("Alikanta", 71, "Halifax"), ("Jenny", 22, "Alcuin"), ("Mandy", 19, "Langwith"), ("Johnson", 44, "Constantine")]
