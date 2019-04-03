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
        (<*>) (Leaf f) (Leaf x)=Leaf $ f x 
        (<*>) (Node l r) (Leaf x)=Leaf fmap l r
        
    aplicativeTest::Maybe String->IO (Maybe String)
    aplicativeTest mstring=do
       pure writeFile <*> mstring $ Just "asa"
        
            
       return . Just  $ "Done"

        

    