{-# LANGUAGE OverloadedStrings #-}

import Hack2
import Hack2.Contrib.Response (set_body_bytestring)
import Hack2.Handler.SnapServer
import Data.Default (def)

app :: Application
app = \env -> 
  return $ 
    set_body_bytestring "Hello World 2" $ 
      def { headers = [ ("Content-Type", "text/plain") ] }

main = run app