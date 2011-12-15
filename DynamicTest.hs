{-# LANGUAGE OverloadedStrings #-}

-- module DynamicTest where
--   
-- aFunction :: IO String
-- aFunction = do
--   let a = 2 + 3
--   putStrLn "another test"
--   return $ "Hi Feross!" ++ show a
-- 
-- 
-- main :: IO ()
-- main = do
--   x <- aFunction
--   putStrLn x


module DynamicTest where

import API
import SafeBase.Framework as F
import SafeBase.RIO
import qualified SafeBase.ByteString.Char8 as B

import SafeBlaze.Html5
import SafeBlaze.Html5.Attributes
import SafeBlaze.Renderer.String
import qualified SafeBlaze.Html5 as H
import qualified SafeBlaze.Html5.Attributes as A


resource = Interface { function = theApp }

theApp :: Application
-- theApp _ = return $ Response { status = 200, headers = [], body = "BODY" }
theApp =  F.safeserve $ do
    --return ()
    -- before $ \e -> do
    --   Prelude.putStrLn "before called"
    --   return e
    
    F.get "/magic" $ do
      lift $ lift $ rioPutStrLn "Testing"
      F.html "The main page!"

    --F.get "/magic/:cap" $ do
    -- F.text . B.pack . show =<< F.captures

    F.get "/magic/template" $ do
      F.html $ B.pack runTemplate

runTemplate :: String
runTemplate = renderHtml templ

templ ::  Html
templ = docTypeHtml $ do
  H.head $ do
    H.title "A templated page"
  H.body $ do
    H.p "the body"
