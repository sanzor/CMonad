module Monad where
    import Control.Monad
    import Tree
    import Control.Applicative
    
    
    instance Functor Tree where
        fmap _ Empty=Empty
        fmap f (Leaf x)=Leaf (f x)
        fmap f (Node left right)=Node (fmap f left) (fmap f right)
    
    instance Applicative Tree where
        pure =Leaf
        (<*>) _ Empty =Empty
        (<*>) (Node f1 f2) (Node v1 v2)=Node 
                                          (Node (Leaf (f1 v1) Leaf (f1 vs))) (Node (Leaf f2 v1)(Leaf f2 v2))
        (<*>) (Leaf f) (Leaf x)=Leaf $ f x 
        (<*>) (Node l r) (Leaf x)=Leaf fmap (l.r) x
        (<*>) 
        
    -- aplicativeTest::Maybe String->IO (Maybe String)
    -- aplicativeTest mstring=do
    --    (pure writeFile) <*> mstring
    --    return . Just  $ "Done"
    
    --IO (Maybe String)

        

    