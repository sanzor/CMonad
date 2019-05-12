module Monad where
    import Control.Monad
    import Tree
    

    instance Functor Tree where
        fmap _  Empty=Empty
        fmap f (Leaf x)=Leaf (f x)
        fmap f (Node el left right)=Node (f el) (fmap f left) (fmap f right)
        
    instance Applicative Tree where
        pure x=Leaf x
        (<*>) _ Empty=Empty
        (<*>) Empty _ =Empty
        (<*>) (Leaf f) t=fmap f t
        (<*>) (Node h ff fg) lf@(Leaf x)=Node (h x) (ff <*> lf) (fg <*> lf)
        (<*>) (Node h ff fg) (Node x fy fz)=Node (h x) (Node  (h x) (ff<*>fy) (ff<*>fz)) (Node (h x) (fg<*>fy)(fg<*>fz))

    instance Monad Tree where
        return=Leaf
        (>>=) Empty _=Empty
        (>>=) (Leaf x) f=f x
        (>>=) (Node x my mz) f=Node (return x) (f my) (f mz)
    


       