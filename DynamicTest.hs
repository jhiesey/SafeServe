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

resource = plugin { function = "good to see ya again again 3" }