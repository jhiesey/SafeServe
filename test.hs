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

import qualified SafeBase.Framework as Sf

import Data.Enumerator (Enumerator, enumEOF)


-- Templates
import WWW.Templates.MainPage

extractCode :: [(B.ByteString, B.ByteString)] -> B.ByteString
extractCode reqBody = fromJust $ Data.List.lookup "theprogram" reqBody

main = do
  run . miku $ do  
    get "/lol" $ do
      progBody <- io $ B.readFile "DynamicTest.hs"
      html $ l2s $ test_page progBody

    get "/" $ do
      html "miku power"
    
    get "/cabal" $ do 
      pt <- io (B.readFile "miku/miku.cabal")
      text pt
  
    get "/magic/*" $ runProgMonadic "DynamicTest"
    
    get "/magic2/:stuff2" $ do
      text . B.pack . show =<< captures
    
    post "/saveprog" $ do
      stuff <- Rd.ask
      let decoded = inputs stuff
      reqBody <- io decoded
      let progBody = extractCode reqBody
      -- io $ putStrLn $ show progBody
      io $ B.writeFile "DynamicTest.hs" progBody
      
      
      html $ l2s $ test_page progBody
    
    public (Just "www") ["/static"]


runProgMonadic :: String -> AppMonad
runProgMonadic progName = do
  resApp <- io (loadProg progName)
  case resApp of
    Right fa -> text $ B.pack fa
    Left (mod, v) -> do
      -- text "SUCCESS!"
      env <- Rd.ask
      safeEnv <- io (toSafeEnv env)
      io (putStrLn "Got here")
      safeRes <- io (runRIO $ function v $ safeEnv)
      io (safeRes `deepseq` (unload mod))    
      St.put $ fromSafeResponse safeRes

toStrict = fromMaybe B.empty . listToMaybe . L.toChunks

toSafeEnv :: Hk.Env -> IO Sf.Env
toSafeEnv hkEnv = do
  byteStr <- input_bytestring hkEnv
  return $ Sf.Env { Sf.requestMethod = Hk.requestMethod hkEnv,
      Sf.scriptName = Hk.scriptName hkEnv,
      Sf.pathInfo = Hk.pathInfo hkEnv,
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
  status <- make (modName++".hs") ["-iapi", "-XSafe"]
  putStrLn $ show status
  case status of
    MakeSuccess _ _ -> f
    MakeFailure e -> return $ Right $ intercalate "\n" e
  
  where
    f = do 
      loadStatus <- pdynload_ (modName++".o") ["api"] [] ["-XSafe"] "API.Interface" "resource"
      case loadStatus of
        LoadFailure msg -> return $ Right $ show msg
        LoadSuccess mod v -> return $ Left $ (mod, v)


