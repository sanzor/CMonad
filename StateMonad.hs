module StateMonad where 
  import Control.Applicative

  newtype S s a=S {runState::s->(a,s)}

  instance Monad (S s) where
    return a=S (\s -> (a,s))
