-- | print the env and response in the console

module Sack.Contrib.Middleware.Debug (debug) where

import Sack

debug :: (Env -> Response -> IO ()) -> Middleware
debug f app = \env -> do
  r <- app env
  f env r
  return r
