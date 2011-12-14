module SafeBase.RIO (
  RIO,
  runRIO,
  printError,
  getCurrentTime,
  rioPutStrLn,
  lift) where
    
import qualified Data.ByteString as B
import System.IO (stderr)
import qualified Data.Time as T
import Control.Monad.Trans.Class (lift)
  
newtype RIO a = UnsafeRIO { runRIO :: IO a }

instance Monad RIO where
  return = UnsafeRIO . return
  m >>= k = UnsafeRIO $ runRIO m >>= runRIO . k
  
instance Functor RIO where
	fmap f x = x >>= (return . f)

printError :: B.ByteString -> RIO ()
printError bs = UnsafeRIO $ B.hPutStr stderr bs

getCurrentTime :: RIO T.UTCTime
getCurrentTime = UnsafeRIO $ T.getCurrentTime

rioPutStrLn :: String -> RIO ()
rioPutStrLn s = UnsafeRIO $ putStrLn s 
  