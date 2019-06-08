module Env where
    import State 
    import System.Directory
    import Control.Monad
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
    
    changeName::String->State Env ()
    changeName (y:ys)=State $ \ (Env (x:xs) ls) -> ((),Env (y:xs) ls)

    toStats::State Env String
    toStats= State $ \env -> (show env,env)
    
    useEnv::IO ()
    useEnv=do
        e<-initEnv
        name<-getLine
        let st=do
            changeName name
            toStats
        let str=evalState st e
        putStrLn str

   


        


    

