module Tree where

    data Tree a=Leaf a | Node (Tree a) (Tree a) |Empty deriving (Eq,Show)