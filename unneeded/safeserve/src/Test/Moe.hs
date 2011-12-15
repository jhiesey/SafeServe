{-# LANGUAGE OverloadedStrings #-}


import qualified Network.Miku as Miku
import Network.Miku (get, miku)
import Hack2.Handler.SnapServer
import Text.HTML.Moe2

import Prelude hiding ((/), (-), head, (>), div)
import Air.Env ((-))

import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Data.ByteString.Char8 as StrictByteString

hello_page :: LazyByteString.ByteString
hello_page = render_bytestring -
  html - do
    head - do
      meta ! [http_equiv "Content-Type", content "text/html; charset=utf-8"] - (/)
      title - str "my title"

    body - do
      div ! [_class "container"] - do
        str "hello world"

l2s :: LazyByteString.ByteString -> StrictByteString.ByteString
l2s x = StrictByteString.concat - LazyByteString.toChunks x
        
main = do
  putStrLn - "server started..."
  
  run - miku - do
    get "/" - do
      Miku.html - l2s -  hello_page