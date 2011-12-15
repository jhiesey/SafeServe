{-# LANGUAGE OverloadedStrings #-}

module Sack.Contrib.Middleware.UserMime (user_mime) where

import Data.List (find)
import Sack
import Sack.Contrib.Response
import Sack.Contrib.Utils
import Air.Light
import Prelude hiding ((.), (-))
import qualified SafeBase.ByteString.Char8 as B
import SafeBase.ByteString.Char8 (ByteString)

user_mime :: [(ByteString, ByteString)] -> Middleware
user_mime h app env = do
  r <- app env
  case h.only_fst.find mime >>= flip lookup h of
    Nothing -> return r
    Just v -> return - r.set_content_type v
  where mime x = env.path_info.B.unpack.ends_with ('.' : x.B.unpack)
