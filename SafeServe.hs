{-# LANGUAGE OverloadedStrings #-}

-- import System.Plugins
import System.Plugins
import API

import Air.Env hiding ((.))
import qualified Control.Monad.State as St
import qualified Control.Monad.Reader as Rd
import qualified Hack2 as Hk
import qualified Hack2.Contrib.Response as HkR
import Control.Monad
import Data.List
import Data.Maybe
import Control.DeepSeq
import SafeBase.RIO
import Data.Char
 
-- import qualified Data.ByteString as BS
import Network.Miku
import Network.Miku.Engine
import Network.Miku.Utils
import Network.Miku.Type (AppMonad)
import Hack2.Handler.SnapServer
import Hack2.Contrib.Request
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.FilePath ((</>), normalise, takeDirectory)
import System.Directory
import Text.Blaze.Renderer.Utf8

import qualified SafeBase.Framework as Sf

import Data.Enumerator (Enumerator, enumEOF)

import System.Directory
-- import System.IO.Unsafe

-- Templates
import WWW.Templates.Editor
import WWW.Templates.Uploaded


extractCode :: [(B.ByteString, B.ByteString)] -> B.ByteString
extractCode reqBody = fromJust $ Data.List.lookup "theprogram" reqBody

extractSiteName :: [(B.ByteString, B.ByteString)] -> Maybe String
extractSiteName cap =
  let
    str = fromJust $ fmap (B.unpack . snd) $ find (((==) "sitename") . fst) cap
    normalized = filter isAlphaNum ((toUpper $ head str) : tail str)
  in
    if Data.List.length normalized Prelude.> 0 then Just normalized else Nothing

readSource :: String -> IO (Maybe B.ByteString)
readSource name = catch (fmap Just $ B.readFile ("plugins" </> (name ++ ".hs"))) (return $ return Nothing)
    
saveSource :: String -> B.ByteString -> IO ()
saveSource name body = B.writeFile ("plugins" </> (name ++ ".hs")) body
    
editor_page :: String -> B.ByteString -> L.ByteString
editor_page siteName fileBody = renderHtml $ editor siteName $ B.unpack fileBody

uploaded_page :: String -> L.ByteString
uploaded_page siteName = renderHtml $ uploaded siteName

main = do
  run . miku $ do  
    get "/edit/:sitename" $ do
      maybeName <- captures >>= (return . extractSiteName)
      let name = fromJust maybeName
      progBody <- io $ readSource name
      case progBody of
        Nothing -> do
          defaultProgBody <- io $ readSource "default"
          html $ l2s $ editor_page ((toUpper $ head name) : tail name) $ fromJust defaultProgBody
        Just contents -> html $ l2s $ editor_page name contents
	  
    post "/edit/:sitename" $ do
      maybeName <- captures >>= (return . extractSiteName)
      case maybeName of
        Nothing -> text $ "Sorry, that's not a valid site name"
        Just name -> do
          stuff <- Rd.ask
          let decoded = inputs stuff
          reqBody <- io decoded
          let progBody = extractCode reqBody
          io $ do
            saveSource name progBody
            createDirectoryIfMissing False ("plugins" </> name)
            createDirectoryIfMissing False ("plugins" </> name </> "static")

          html $ l2s $ editor_page name progBody
          
    post "/upload/:sitename" $ do
      maybeName <- captures >>= (return . extractSiteName)
      case maybeName of
        Nothing -> text $ "Sorry, that's not a valid site name"
        Just name -> do
          stuff <- Rd.ask
          let decoded = inputs stuff
          reqBody <- io decoded
          io $ putStrLn "Got this far!"
          let fileContents = fromJust $ Data.List.lookup "thefile" reqBody
          let destPath = fromJust $ Data.List.lookup "thepath" reqBody
          io $ do
            cwd <- getCurrentDirectory            
            let absPath = normalise $ cwd ++ "/plugins/" ++ name  ++ "/" ++ (normalise $ B.unpack destPath)
            putStrLn "Got even further"
            putStrLn absPath
            createDirectoryIfMissing True $ takeDirectory absPath
            B.writeFile absPath fileContents
            
          html $ l2s $ uploaded_page name 
      
    get "/run/:sitename" $ do
      maybeName <- captures >>= (return . extractSiteName)
      case maybeName of
        Nothing -> text $ "Sorry, that's not a valid site name"
        Just name -> runProgMonadic name
      
    get "/run/:sitename/*" $ do
      maybeName <- captures >>= (return . extractSiteName)
      case maybeName of
        Nothing -> text $ "Sorry, that's not a valid site name"
        Just name -> runProgMonadic name

    get "/" $ do
      html "You found the homepage!"

    public (Just "www") ["/static"]
    
    public (Just ".") ["/plugins"]

runProgMonadic :: String -> AppMonad
runProgMonadic progName = do
  resApp <- io (loadProg progName)
  case resApp of
    Right fa -> text $ B.pack fa
    Left (mod, v) -> do
      env <- Rd.ask
      safeEnv <- io (toSafeEnv env)
      io (putStrLn "About to run plugin")
      safeRes <- io (runRIO (function v $ safeEnv) progName)
      io (safeRes `deepseq` (unload mod))    
      St.put $ fromSafeResponse safeRes

toStrict = fromMaybe B.empty . listToMaybe . L.toChunks

removeStart :: B.ByteString -> B.ByteString
removeStart fullPath = if Data.List.length slashes Prelude.> 2 then B.drop (slashes Data.List.!! 2) fullPath else B.singleton '/'
  where slashes = B.elemIndices '/' fullPath

toSafeEnv :: Hk.Env -> IO Sf.Env
toSafeEnv hkEnv = do
  byteStr <- input_bytestring hkEnv
  return $ Sf.Env { Sf.requestMethod = Hk.requestMethod hkEnv,
      Sf.scriptName = Hk.scriptName hkEnv,
      Sf.pathInfo = removeStart $ Hk.pathInfo hkEnv,
      Sf.queryString = Hk.queryString hkEnv,
      Sf.serverName = Hk.serverName hkEnv,
      Sf.serverPort = Hk.serverPort hkEnv,
      Sf.httpHeaders = Hk.httpHeaders hkEnv,
      Sf.hackVersion = Hk.hackVersion hkEnv,
      Sf.hackUrlScheme = Hk.hackUrlScheme hkEnv,
      Sf.hackInput = toStrict byteStr,
      Sf.hackErrors = "Can haz error?",
      Sf.hackHeaders = Hk.hackHeaders hkEnv
    }

fromSafeResponse :: Sf.Response -> Hk.Response
fromSafeResponse sfRes = HkR.set_body_bytestring (L.fromChunks [Sf.body sfRes]) $ Hk.Response
  { Hk.status = Sf.status sfRes,
    Hk.headers = Sf.headers sfRes,
    Hk.body = Hk.HackEnumerator enumEOF
  }

loadProg :: String -> IO (Either (Module, Interface) String)
loadProg modName = do
  status <- make ("plugins" </> (modName ++ ".hs")) ["-iapi", "-XSafe"]
  putStrLn $ show status
  case status of
    MakeSuccess _ _ -> f
    MakeFailure e -> return $ Right $ intercalate "\n" e
  
  where
    f = do
      let binPath = "plugins" </> (modName ++ ".o")
      loadStatus <- pdynload_ binPath ["api"] [] ["-XSafe"] "API.Interface" "resource"
      case loadStatus of
        LoadFailure msg -> return $ Right $ intercalate "\n" msg
        LoadSuccess mod v -> return $ Left $ (mod, v)
