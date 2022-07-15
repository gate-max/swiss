
module Control.Exception.Swiss (
    tryDef,
    trying,
    trys,
    ) where


import           Control.Exception ( SomeException(SomeException), try )
import           Data.Typeable (typeOf)



-- | > tryDef (simpleHttp "https://www.google.com") (simpleHttp "https://www.nasdaq.com")
--   also work for IO () 
tryDef :: IO a       -- ^ default IO action
          -> IO a    -- ^ main IO action
          -> IO a    -- ^ if succeed, run the main IO; or run the default IO when failed. 
tryDef def x = do
  v <- try x
  case v of
    Right x -> return x
    Left (SomeException e) -> def


-- | > mapM_ (\i -> trying $ runIOaction i) [0..10]
trying :: IO () -> IO ()
trying x = do
  v <- try x
  case v of
    Right x -> return x
    Left (SomeException e) -> print (typeOf e)  


-- | > mapM_ (\i -> trying $ runIOaction i) [0..10]
trys :: IO String -> IO String
trys x = do
  v <- try x
  case v of
    Right x -> return x
    Left (SomeException e) -> return $ show (typeOf e) ++ "\n"  

