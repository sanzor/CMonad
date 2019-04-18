module Monad where
    import Control.Monad
    import Tree
    

    instance Functor Tree where
        fmap _  Empty=Empty
        fmap f (Leaf x)=Leaf (f x)
        fmap f (Node el left right)=Node (f el) (fmap f left) (fmap f right)
        
    instance Applicative Tree where
        pure x=Leaf x
        <*> (Leaf t) Empty=Empty
        <*> (Node h ff fg) (Leaf x)=Node (h x) (ff <*> pure x) (fg <*> pure x)
        <*> (Node h ff fg) (Node x fy fz)=Node (h x) (Node 
        
    
    