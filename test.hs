{-# LANGUAGE OverloadedStrings #-}

-- import System.Plugins
import System.Plugins
import API

import Air.Env hiding ((.))
import qualified Control.Monad.State as St
import qualified Control.Monad.Reader as Rd
import qualified Hack2 as Hk
import Control.Monad
import Data.List
import Data.Maybe
import Control.DeepSeq
 
-- import qualified Data.ByteString as BS
import Network.Miku
import Network.Miku.Engine
import Network.Miku.Utils
import Hack2.Handler.SnapServer
import Hack2.Contrib.Request
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L


-- Templates
import WWW.Templates.MainPage

extractCode :: [(B.ByteString, B.ByteString)] -> B.ByteString
extractCode reqBody = fromJust $ Data.List.lookup "theprogram" reqBody

main = do
  -- putStrLn "BEFORE DYNLOAD2"
  -- dynload2
  -- putStrLn "AFTER DYNLOAD2"
  run . miku $ do
    -- before $ \e -> do
    --   Prelude.putStrLn "before called"
    --   return e
  
    get "/lol" $ do
      -- putStr "non-root"
      progBody <- io $ B.readFile "DynamicTest.hs"
      html $ l2s $ test_page progBody

    get "/" $ do
      html "miku power"
    
    get "/cabal" $ do 
      pt <- io (B.readFile "miku/miku.cabal")
      text pt
  
    get "/magic" $ do
      pt <- io (runProg "DynamicTest" >>= return . B.pack)
      text pt
    
    post "/saveprog" $ do
      stuff <- Rd.ask
      let decoded = inputs stuff
      reqBody <- io decoded
      let progBody = extractCode reqBody
      -- io $ putStrLn $ show progBody
      io $ B.writeFile "DynamicTest.hs" progBody
      
      
      html $ l2s $ test_page progBody
    
    public (Just "www") ["/static"]
  
-- l2s :: L.ByteString -> B.ByteString
-- l2s x = B.concat $ L.toChunks x  
--   

runProg :: String -> IO String
runProg modName = do
  status <- make (modName++".hs") ["-iapi", "-XSafe"]
  putStrLn $ show status
  case status of
    MakeSuccess _ _ -> f
    MakeFailure e -> return $ concat e
  
  where
    f = do 
      loadStatus <- pdynload_ (modName++".o") ["api"] [] ["-XSafe"] "API.Interface" "resource"
      case loadStatus of
        LoadFailure msg -> return $ show msg
        LoadSuccess mod v -> do
          let resString = function v
          resString `deepseq` unload mod
          return resString
 
 
 


-- src     = "Plugin.hs"
-- -- wrap    = "../Wrapper.hs"
-- apipath = "api"
-- 
-- dynload2 :: IO ()
-- dynload2 = do
--   status <- make src ["-i"++apipath, "-XSafe"]
--   case status of
--     MakeSuccess _ _ -> f
--     MakeFailure e -> mapM_ putStrLn e
-- 
--   where
--     f = do
--       putStrLn "Trying to load"
--       v <- pdynload_ "Plugin.o" ["api"] [] ["-XSafe"] "API.Interface" "resource"
--       putStrLn "Tried loading"
--       case v of
--         LoadSuccess _ a  -> do
--           putStrLn "loaded .. yay!"
--           putStrLn $ function a
--         LoadFailure msg  -> putStrLn "failed"  >> print msg
     