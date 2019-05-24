module State where 
    import Control.Monad 
    import System.Directory
   
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
    

    get::State s s
    get=State $ \s -> (s,s)

    put :: s -> State s ()
    put s = State $ \_ -> ((), s)
    

    modify::(s->s)->State s ()
    modify f = get>>= \s -> put (f s) 

    evalState::State s a ->s->a
    evalState act=fst . run act

    execState::State s a ->s->s
    execState act=snd . run act

    -- changeName::String->State Env ()
    -- changeName str= State $ \(Env _ f)-> ((),Env str f )