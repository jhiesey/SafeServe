module Air.Data.Managed where

import Air.Data.Default

class Managed a where
  initialize :: IO (Maybe a)
  initialize = return Nothing
  
  destroy :: a -> IO ()
  destroy = const (return ())
  
  with_managed_object :: (a -> IO ()) -> IO ()
  with_managed_object f = do
    maybe_x <- initialize
    case maybe_x of
      Just x -> f x >> destroy x
      Nothing -> return ()

