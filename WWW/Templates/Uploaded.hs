{-# LANGUAGE OverloadedStrings #-}

module WWW.Templates.Uploaded (uploaded) where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

uploaded :: String -> Html
uploaded siteName = docTypeHtml $ do
  H.head $ do
    H.title "Upload successful"
  body $ do
    H.p $ "Upload successful!"
    H.a ! href (toValue ("/edit/" ++ siteName)) $ "Back to editor"
      
      
      
      