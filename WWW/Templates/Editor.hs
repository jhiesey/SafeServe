{-# LANGUAGE OverloadedStrings #-}

module WWW.Templates.Editor (editor) where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

editor :: String -> String -> Html
editor siteName prog = docTypeHtml $ do
  H.head $ do
    H.title "Editor"
  body $ do
    H.p $ toHtml ("Editing " ++ siteName)
    H.form ! action (toValue ("/edit/" ++ siteName)) ! method "post" $ do
      textarea ! name "theprogram" ! rows "60" ! cols "150" $ toHtml prog
      H.br
      input ! type_ "submit" ! value "Save!"
    H.a ! href (toValue ("/run/" ++ siteName)) $ "Run!"
      
      
      
      