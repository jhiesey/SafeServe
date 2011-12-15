{-# LANGUAGE OverloadedStrings #-}

-- | Stolen from rack:
--   The Rack::Static middleware intercepts requests for static files
--   (javascript files, images, stylesheets, etc) based on the url prefixes
--   passed in the options, and serves them using a Rack::File object. This
--   allows a Rack stack to serve both static and dynamic content.

module Sack.Contrib.Middleware.Static (static) where

import Data.Maybe
import Sack
import Sack.Contrib.Middleware.File (file)
import Sack.Contrib.Utils
import Data.List (find, isPrefixOf)
import Air.Light
import Prelude hiding ((.), (^), (>), (+))
import qualified SafeBase.ByteString.Char8 as B
import SafeBase.ByteString.Char8 (ByteString)

static :: Maybe ByteString -> [ByteString] -> Middleware
static root urls app = \env -> do
  let my_urls = if urls.null then ["/favicon.ico"] else urls

  let path = env.path_info .as_string unescape_uri

  let can_serve = my_urls.find ( `B.isPrefixOf` path ) .isJust
  
  if can_serve
    then do
      -- putStrLn "can serve"
      file root app env
    else do
      -- putStrLn "not valid prefix"
      app env
