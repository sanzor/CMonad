module Monad where
    import Control.Monad
    import Tree
    

    instance Functor Tree where
        fmap f (Leaf x)=Leaf (f x)
        fmap f (Node left right)=Node (fmap f left) (fmap f right)
    

    groups::[Int]->[[Int]]
    groups ls=go ls [] [] where
       
        go (x:xs) [] bg= go xs [x] bg
        go (x:xs) (y:ys) bg | x==y = go xs (x:y:ys) bg
                            | otherwise = go xs [] ((y:ys):bg)
        
    