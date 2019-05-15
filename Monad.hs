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
        (>>=) (Node x my mz) f=Node id (my>>=f) (mz>>=f)
    
    
    
   


       