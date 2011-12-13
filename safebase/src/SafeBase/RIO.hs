module SafeBase.RIO (
  RIO,
  runRIO,
  printError,
  getCurrentTime) where
    
import qualified Data.ByteString as B
import System.IO (stderr)
import qualified Data.Time as T
  
newtype RIO a = UnsafeRIO { runRIO :: IO a }

instance Monad RIO where
  return = UnsafeRIO . return
  m >>= k = UnsafeRIO $ runRIO m >>= runRIO . k

printError :: B.ByteString -> RIO ()
printError bs = UnsafeRIO $ B.hPutStr stderr bs

getCurrentTime :: RIO T.UTCTime
getCurrentTime = UnsafeRIO $ T.getCurrentTime
  