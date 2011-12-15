module Sack.Contrib.Middleware.Head (head) where

import Sack
import Sack.Contrib.Response
import Sack.Contrib.Utils

import Air.Env hiding (def, head)
import Prelude ()
import qualified SafeBase.ByteString.Char8 as B
import Data.Default (def)

head :: Middleware
head app = \env -> do
  response <- app env
  if env.request_method.is HEAD 
    then response .set_body def .set_content_length 0 .return
    else response .return
