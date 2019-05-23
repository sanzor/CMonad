module Reader where 
    import Control.Monad.Reader
 type Reader r a=ReaderT r Identity
