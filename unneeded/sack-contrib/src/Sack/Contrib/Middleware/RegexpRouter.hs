-- | matching a list of regexp agains a path_info, if matched, consponding app
--   is used, otherwise, pass the env down to lower middleware

module Sack.Contrib.Middleware.RegexpRouter (regexp_router) where

import Data.Maybe
import Sack
import Sack.Contrib.Utils
import Sack.Contrib.AirBackports
import Data.List (find)
import Air
import Prelude hiding ((.), (^), (>), (-))
import qualified SafeBase.ByteString.Char8 as B

type RoutePath = (String, Application)

regexp_router :: [RoutePath] -> Middleware
regexp_router h app = \env ->
  let path = env.path_info.B.unpack
  in
  case h.find (fst > flip match path > isJust) of
    Nothing -> app env
    Just (_, found_app) -> found_app env