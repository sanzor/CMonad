module Env where
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
    
    changeName::String->State Env ()
    changeName name=State $ \ Env (x:xs) ls -> ((),Env name ls)

    toStats::State Env String
    toStats= State $ \env ->( (show env)++",file count:"++show . length  $ fileNames env,env)

    useEnv::State Env String
    useEnv=do
        liftM put initEnv
        liftM changeName getLine
        toStats

        


    

