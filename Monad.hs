module Monad where
    import Control.Monad
    import Tree
    

    instance Functor Tree where
        fmap _  Empty=Empty
        fmap f (Leaf x)=Leaf (f x)
        fmap f (Node  el left right)=Node (f el) left right
        
    -- instance Applicative Tree where
    --     pure 
    
    