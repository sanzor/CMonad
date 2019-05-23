module Reader where 
    import Control.Monad
    newtype Reader e a = R {runReader::e->a}
    instance Functor (Reader e) where
        fmap=Control.Monad.liftM
    instance Applicative (Reader e) where
        pure =return
        (<*>)=Control.Monad.ap

    instance Monad (Reader e) where
        return a=R $ \_-> a
        m>>=f=R $ \r -> runReader (f (runReader m r)) r
    
    ask::Reader e e
    ask=R id