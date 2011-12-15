-- | print the env and response in the console

module Sack.Contrib.Middleware.Inspect (inspect) where

import Sack
import Air.Light
import Prelude hiding ((.), (^))

inspect :: Middleware
inspect app = \env -> env.trace'.app ^ trace'
