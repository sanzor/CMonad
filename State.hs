module State where

    newtype State s a=State{run::s->(a,s)}
    
    
    instance Functor (State s) where
        fmap = Control.Monad.liftM
    instance Applicative (State s) where
        pure=return
        (<*>)=Control.Monad.ap

    instance Monad(State s) where
        return a=State $ (\s->(a,s))
        (>>=) m f=State $ (\s ->let (a,s')=run m s in
                                           run (f a) s'
  

    
    get::State s s
    get=State $ \s ->(s,s)

    put::s->State s ()
    put x=State $ \s ->((),s)

    modify::(s->s)->State s ()
    modify f=get>>=\s -> put (f s)
   