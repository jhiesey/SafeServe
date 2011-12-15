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
 
-- import qualified Data.ByteString as BS
import Network.Miku
import Network.Miku.Engine
import Network.Miku.Utils
import Network.Miku.Type (AppMonad)
import Hack2.Handler.SnapServer
import Hack2.Contrib.Request
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.FilePath ((</>))
import Text.Blaze.Renderer.Utf8

import qualified SafeBase.Framework as Sf

import Data.Enumerator (Enumerator, enumEOF)

import System.IO.Unsafe

-- Templates
import WWW.Templates.Editor


extractCode :: [(B.ByteString, B.ByteString)] -> B.ByteString
extractCode reqBody = fromJust $ Data.List.lookup "theprogram" reqBody

extractSiteName :: [(B.ByteString, B.ByteString)] -> String
extractSiteName cap = fromJust $ fmap (B.unpack . snd) $ find (((==) "sitename") . fst) cap

haskellPath :: String -> Maybe String
haskellPath name = if ".." `isInfixOf` name then Nothing else Just ("plugins" </> (name ++ ".hs"))

readSource :: String -> IO (Maybe B.ByteString)
readSource name = 
  case haskellPath name of
    Nothing -> return Nothing
    Just path -> catch (fmap Just $ B.readFile path) (return $ return Nothing)
    
saveSource :: String -> B.ByteString -> IO ()
saveSource name body =
  case haskellPath name of
    Nothing -> return ()
    Just path -> B.writeFile path body
    
editor_page :: String -> B.ByteString -> L.ByteString
editor_page siteName fileBody = renderHtml $ editor siteName $ B.unpack fileBody


main = do
  run . miku $ do  
    get "/edit/:sitename" $ do
      name <- captures >>= (return . extractSiteName)
      progBody <- io $ readSource name
      case progBody of
        Nothing -> text $ "Sorry, that site doesn't exist"
        Just contents -> html $ l2s $ editor_page name contents
	  
    post "/edit/:sitename" $ do
      name <- captures >>= (return . extractSiteName)
      stuff <- Rd.ask
      let decoded = inputs stuff
      reqBody <- io decoded
      let progBody = extractCode reqBody
      io $ saveSource name progBody

      html $ l2s $ editor_page name progBody
      
    get "/run/:sitename" $ do
      name <- captures >>= (return . extractSiteName)
      runProgMonadic name
      
    get "/run/:sitename/*" $ do
      name <- captures >>= (return . extractSiteName)
      runProgMonadic name


    get "/" $ do
      html "You found the homepage!"

    public (Just "www") ["/static"]

runProgMonadic :: String -> AppMonad
runProgMonadic progName = do
  resApp <- io (loadProg progName)
  case resApp of
    Right fa -> text $ B.pack fa
    Left (mod, v) -> do
      env <- Rd.ask
      safeEnv <- io (toSafeEnv env)
      io (putStrLn "About to run plugin")
      safeRes <- io (runRIO $ function v $ safeEnv)
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
loadProg modName =
  case haskellPath modName of
    Nothing -> return $ Right "Invalid site path"
    Just path -> do
      status <- make path ["-iapi", "-XSafe"]
      putStrLn $ show status
      case status of
        MakeSuccess _ _ -> f
        MakeFailure e -> return $ Right $ intercalate "\n" e
  
      where
        f = do
          let binPath = "plugins" </> (modName ++ ".o")
          loadStatus <- pdynload_ binPath ["api"] [] ["-XSafe"] "API.Interface" "resource"
          case loadStatus of
            LoadFailure msg -> return $ Right $ show msg
            LoadSuccess mod v -> return $ Left $ (mod, v)
