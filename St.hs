module St where
    import Control.Monad

    
    newtype State s a=State { runState::s->(a,s) }

    f::Int->(String,Int)
    f t=if t>0 then ("greater then0",t)
        else ("smaller then 0",t)
    
    make::State Int String
    make=State f

    instance Monad (State s) where
        return a=State $ \s->(a,s)
        (>>=) m g=State $ \s -> let (a,s')= runState m s in
                                   runState (g a) s'
                                
