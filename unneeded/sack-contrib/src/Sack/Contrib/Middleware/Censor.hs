-- Censor it !

module Sack.Contrib.Middleware.Censor (censor) where

import Sack

censor :: (Response -> IO Response) -> Middleware
censor alter app = \env -> app env >>= alter