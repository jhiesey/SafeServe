module DynamicTest where
  
aFunction :: IO String
aFunction = do
  let a = 2 + 3
  return $ "Hi again! I hope there isn't a serious problem here LA LA LA!!!" ++ show a


main :: IO ()
main = do
  x <- aFunction
  putStrLn x