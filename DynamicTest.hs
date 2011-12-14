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
import SafeBase.Framework
import SafeBase.RIO

resource = Interface { function = theApp }

theApp :: Application
-- theApp _ = return $ Response { status = 200, headers = [], body = "BODY" }
theApp =  safeserve $ do
    --return ()
    -- before $ \e -> do
    --   Prelude.putStrLn "before called"
    --   return e
    
    get "/magic" $ do
      lift $ lift $ rioPutStrLn "Testing"
      html "YEEHAW!!!"
