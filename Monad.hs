module Monad where
    import Control.Monad
    import Tree
    

    instance Functor Tree where
        fmap f (Leaf x)=Leaf (f x)
        fmap f (Node left right)=Node (fmap f left) (fmap f right)