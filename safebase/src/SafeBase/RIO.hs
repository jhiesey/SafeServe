module SafeBase.RIO (
  RIO,
  runRIO,
  printError,
  getCurrentTime,
  rioPutStrLn,
  printPath,
  readFileRIO,
  writeFileRIO,
  lift) where
    
import qualified Data.ByteString as B
import System.IO (stderr)
import System.IO.Strict as ST
import qualified Data.Time as T
import Control.Monad.Trans.Class (lift)
import System.FilePath
import System.Directory
  
newtype RIO a = UnsafeRIO { runRIO :: String -> IO a }

instance Monad RIO where
  return x = UnsafeRIO (\_ -> return x)
  m >>= k = UnsafeRIO $ \path -> (runRIO m path >>= \x -> runRIO (k x) path)
  
instance Functor RIO where
  fmap f x = x >>= (return . f)

printError :: B.ByteString -> RIO ()
printError bs = UnsafeRIO $ \_ -> B.hPutStr stderr bs

getCurrentTime :: RIO T.UTCTime
getCurrentTime = UnsafeRIO $ \_ -> T.getCurrentTime

rioPutStrLn :: String -> RIO ()
rioPutStrLn s = UnsafeRIO $ \_ -> putStrLn s

printPath :: RIO ()
printPath = UnsafeRIO $ \path -> putStrLn path

readFileRIO :: String -> RIO String
readFileRIO name = UnsafeRIO $ \path -> do
  cwd <- getCurrentDirectory
  let absPath = normalise $ cwd ++ "/plugins/" ++ path ++ "/" ++ name
  ST.readFile absPath
  
writeFileRIO :: String -> String -> RIO ()
writeFileRIO name contents = UnsafeRIO $ \path -> do
  cwd <- getCurrentDirectory
  let absPath = normalise $ cwd ++ "/plugins/" ++ path ++ "/" ++ name
  writeFile absPath contents