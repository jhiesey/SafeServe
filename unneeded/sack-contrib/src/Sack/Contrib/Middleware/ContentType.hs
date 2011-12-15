-- | Stolen from rack: Sets the Content-Type header on responses which don't 
--   have one.

module Sack.Contrib.Middleware.ContentType (content_type) where

import Sack
import Sack.Contrib.Constants
import Sack.Contrib.Response
import Air.Light
import Prelude hiding ((.), (^), (>), (-))
import SafeBase.ByteString.Char8 (ByteString)


content_type :: ByteString -> Middleware
content_type s app = \env -> do
  response <- app env
  
  return - case response.header _ContentType of
    Nothing -> response.set_header _ContentType s
    Just _ -> response
