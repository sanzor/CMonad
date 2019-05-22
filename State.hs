module State where 
    import Control.Monad
    newtype State s a = State {run::s->(a,s)} 

    instance Functor (State s) where
        fmap=Control.Monad.liftM
    instance Applicative (State s) where
        pure=return
        (<*>)=Control.Monad.ap
    instance Monad (State s) where
        return a= State $ \k->(a,k) 
        (>>=) m f=State $ \s -> let (a,s')=run m s in
            run (f a) s'
        
            
    f::Int->(Int,IO String)
    f x | x>3 = (x, (++) <$> getLine <*>getLine)
        |otherwise =(x, (\y _->y)  <$>  getLine <*> writeFile "a.txt" (show x) )
    
   
   
  
     

 
   