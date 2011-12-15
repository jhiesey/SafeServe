{-# LANGUAGE OverloadedStrings #-}


module Network.SafeServe.Config where

import Sack
import Sack.Contrib.Middleware.Config
import Sack.Contrib.Middleware.ContentLength
import Sack.Contrib.Middleware.ContentType
import Network.SafeServe.Utils
import Prelude hiding ((.), (>), (^), (-))
import SafeBase.ByteString.Char8 (ByteString)


pre_installed_middlewares :: [Middleware]
pre_installed_middlewares = 
  [
    content_length
  , content_type default_content_type
  ]
  where
    default_content_type = "text/plain; charset=UTF-8"


safeserve_captures :: ByteString
safeserve_captures = "safeserve-captures-"