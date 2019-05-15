module MyMonoid where 
  a=[("a",1),("b",2),("c",3)]
  b=[(2,3),(3,4)]

  f::Maybe Int
  f =do
    x<- lookup "a" a
    y<- lookup x b
    putStr "here"
    Just (x+y)
    
  
  fromString::String->Maybe Int
  fromString  x= if length x >2 then Nothing else Just 3
    