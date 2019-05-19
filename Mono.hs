module Mono where 
 
  
  

  data Mono a=A a|Nada deriving(Show)

  instance (Eq a)=>Eq (Mono a) where 
    (A x) == (A y)= x==y
    (A _) == Nada=False
    Nada == _ =False

 
  instance Monoid (Mono a) where 
    mempty = Nada
    mappend Nada x = x
    mappend x Nada= x
    mappend (A x) (A y)=foldMap
