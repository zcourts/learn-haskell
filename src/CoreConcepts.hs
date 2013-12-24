module CoreConcepts where

f :: Int -> String -> IO ()
f age name = print (name ++ " is " ++ show age ++ " years old")

partial :: Int -> String -> IO ()  
partial 12 name = print ("Haha you're a child! " ++ name ++ " you're 12 years old")

total :: Int -> String -> IO ()  
total 12 name = print ("Haha you're a child! " ++ name ++ " you're 12 years old")
total _ name = print (name ++ " maybe you're a child, maybe you're not...but we'll just say you are :)")

total2 :: Int -> String -> IO ()  
total2 12 name = print ("Haha you're a child! " ++ name ++ " you're 12 years old")
total2 age name = print (name ++ " you're " ++ show age ++ " years old")

gaurdedTotal :: Int -> String -> IO ()  
gaurdedTotal age name 
           | age <= 12 = print ("Haha you're " ++ show age ++ " you're a child, " ++ name)
           | age <= 35 = print ("You're " ++ show age ++ ", supposedly this is the best time of your life !")
           | age <= 55 = print ("Ohhh, " ++ name ++ " you're getting on a bit there at" ++ show age ++ " aren't you!")
           | otherwise = print ("To be honest " ++ name ++ " at "++ show age ++ " it's cruel to take a jab at you :P!")
           
mySum :: Int -> Int -> IO ()  
mySum a b 
  | a + b == 2 = print ("Yup " ++ show a ++ "+" ++ show b ++ " = 2" )
  | a + b == 10 = print ("Yup " ++ show a ++ "+" ++ show b ++ " = 10 " )
  | otherwise = print "You're not very good at this math thing are you?"  
          
sumWhere :: Int -> Int -> IO ()  
sumWhere a b 
  | combined == 2 = print ("Yup " ++ show a ++ "+" ++ show b ++ " = 2" )
  | combined == 10 = print ("Yup " ++ show a ++ "+" ++ show b ++ " = 10 " )
  | otherwise = print "You're not very good at this math thing are you?"  
   where combined = a + b
   
sumLet :: Int -> Int -> IO ()  
sumLet a b =
        let total = a + b
        in  print ("The total is " ++ show total)
        
sumLet2 :: Int -> Int -> IO ()  
sumLet2 a b =
        let total = a + b
            times = a * b
        in  print ("The total is " ++ show total ++ " and times each other the value is " ++ show times)
        
caseExpr :: [Int] -> IO ()
caseExpr xs = case xs of [] -> print "Yeah, we don't like empty lists"
                         x:xss -> print x
 
data Person1 = Person1 Int Float Float
                         
--data Person = Person {age::Int, height::Float, foodLevel::Float} deriving(Eq,Show)
data Person = Infant{age::Int, height::Float, foodLevel::Float, diaperCount::Int}
              | Child {}
              | Teenager {}
              | Adult {}
              | Elderly {}
              deriving (Eq,Show)

class (Eq a) => Animal a where  
    grow :: a -> a  
    eat :: a -> Float -> a
    sleep :: a -> Float -> a
    
instance Animal Person where
--p{age = age p + 1}
--grow p@Person{age=age} = p{age=age+1}
--in both cases above we update the label age, keeping all other values the same
-- http://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-490003.15
-- thanks to Hafydd from #haskell irc channel
    --grow :: Person -> Int     
    grow p = p{age = age p + 1}
    
    eat p a = p{height =  newHeight p a}
        where h = height p
              newHeight p amount = h + (1 / (amount * h))
              
    sleep p t = p{height = newHeight (h p t) t}
        where h p t= height p * t / 2
              newHeight height time = height + (1 / (time * height))   
                         