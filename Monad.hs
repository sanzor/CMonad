module Monad where
    import Control.Monad
    import Tree
    
    
    instance Functor Tree where
        fmap _  Empty=Empty
        fmap f (Node el left right)=Node (f el) (fmap f left) (fmap f right)
        
    instance Applicative Tree where
        pure x=Node x Empty Empty
        (<*>) _ Empty=Empty
        (<*>) Empty _ = Empty
        (<*>) (Node h ff fg) (Node x fy fz)=Node (h x) (Node  (h x) (ff<*>fy) (ff<*>fz)) (Node (h x) (fg<*>fy)(fg<*>fz))

    instance Monad Tree  where
        return k=Node k Empty Empty 
        (>>=) Empty _=Empty
<<<<<<< HEAD
        (>>=) (Leaf x) f=f x
        (>>=) (Node x my mz) f=Node (unwrap $ f x) (my>>=f)(mz>>=f)
=======
        (>>=) (Node x my mz) f=Node id (my>>=f) (mz>>=f)
    
    
>>>>>>> ae5cb879a1e6c92010afa6fbbca08415fd0eda5b
    
   


    unwrap::Tree a->a
    unwrap (Leaf x)=x
    unwrap _ =undefined