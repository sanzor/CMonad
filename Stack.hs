module Stack where
    import State 
    import System.Directory
    data Env=Env{
        envName::String,
        fileNames::[String]
    }
    instance Show Env where 
        show Env{envName=x,fileNames=xs} = "{ envName:"++x++" , files: ["++foldr (\t y-> t++","++y) "" xs ++"] }"
      
    initEnv::IO Env
    initEnv=do
        name<- getLine
        names<- getCurrentDirectory>>=listDirectory
        return Env{envName=name,fileNames=names}
    
    
    
   

    type Stack=[Int]

    pop::State Stack Int
    pop= State $ \(x:xs)->(x,xs)

    push::Int->State Stack ()
    push i =State $ \xs ->((),i:xs)

    
    stackManip::State Stack Int
    stackManip=do
        push 3
        pop
        pop
       
    chName::String->State Env ()
    chName str=State $ \(Env name names) -> ((),Env str names) 

  

