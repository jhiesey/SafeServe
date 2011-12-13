{-# LANGUAGE OverloadedStrings #-}

module WWW.Templates.MainPage (test_page) where

-- import Data.ByteString.Lazy.Char8 hiding (head)
-- import Prelude hiding ((/), (-), head, (>), (.), div, id)
-- import Air.Light ((-))
-- import Text.HTML.Moe2
-- 
-- test_page :: ByteString
-- test_page = render_bytestring -
--   html - do
--     head - do
--       meta ! [http_equiv "Content-Type", content "text/html; charset=utf-8"] - (/)
--       title - str "my title"
--       link ! [rel "icon", _type "image/png", href "panda_icon.png"] - (/)
-- 
--     body - do
--       div ! [_class "container"] - do
--         str "hello world"
--         div ! [id "header", _style "width: 100%; height: 100px; text-align: center;"] - do
--           str "A header"
--         div ! [id "body"] - do
--           form ! [action "/saveprog", method "post"] - do
--             textarea ! [] - do
--               
--           input ! [_type "submit"] - (/)
          
          
-- import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8

test_page :: B.ByteString -> L.ByteString
test_page fileBody = renderHtml $ editor $ B.unpack fileBody

editor :: String -> Html
editor prog = docTypeHtml $ do
  H.head $ do
    H.title "Natural numbers"
  body $ do
    H.form ! action "/saveprog" ! method "post" $ do
      textarea ! name "theprogram" ! rows "25" ! cols "80" $ toHtml prog
      input ! type_ "submit" ! value "submit"
    H.a ! href "/magic" $ "Magic!"
      
      
      
      