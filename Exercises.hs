module Exercises where
    groups::[Int]->[[Int]]
    groups ls=go ls [] [] where
        go [] sm bg=sm:bg
        go (x:xs) [] bg= go xs [x] bg
        go (x:xs) (y:ys) bg | x==y = go xs (x:y:ys) bg
                            | otherwise = go xs [x] ((y:ys):bg)

  
        
