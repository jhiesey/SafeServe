module Sack.Contrib.Middleware.IOConfig (ioconfig) where

import Sack

ioconfig :: (Env -> IO Env) -> Middleware
ioconfig before app = \env -> before env >>= app