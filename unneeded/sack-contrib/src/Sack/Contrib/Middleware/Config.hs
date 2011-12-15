-- | Stolen from rack-contrib: modifies the environment using the block given 
--   during initialization.

module Sack.Contrib.Middleware.Config (config) where

import Sack

config :: (Env -> Env) -> Middleware
config alter app = \env -> app (alter env)