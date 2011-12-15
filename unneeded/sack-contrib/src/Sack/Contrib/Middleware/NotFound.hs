module Sack.Contrib.Middleware.NotFound (not_found) where

import Sack
import Sack.Contrib.Response
import Sack.Contrib.Constants

import Air.Light
import Prelude hiding ((.), (^), (>), (+))
import Data.Default


not_found :: Middleware
not_found _ = \_ -> return $
  def
    .set_status 404
    .set_content_type _TextHtml
    .set_content_length 0
