{-# LANGUAGE OverloadedStrings #-}

import Hack2
import Hack2.Contrib.Response (set_body_bytestring)
import Hack2.Handler.SnapServer
import Data.Default (def)

import Data.ByteString.Lazy.Char8 (pack)
import Hack2.Contrib.Utils (empty_app)
import Hack2.Contrib.Middleware.URLMap


say :: Application
say = \env -> return $ set_body_bytestring (pack $ show env) def

app :: Application
app = url_map [("/hello", say), ("/there", say)] empty_app

main = run app