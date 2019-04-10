module Tree where

    data Tree a=Leaf a | Node a (Tree a) (Tree a) |Empty deriving (Eq,Show)