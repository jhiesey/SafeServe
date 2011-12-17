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
    H.script ! src "/static/codemirror2/lib/codemirror.js" $ ""
    H.link ! rel "stylesheet" ! href "/static/codemirror2/lib/codemirror.css"
    H.script ! src "/static/codemirror2/mode/haskell/haskell.js" $ ""
    H.script ! src "/static/js/jquery-1.7.1.min.js" $ ""
    H.script ! src "/static/jquery-ui/js/jquery-ui-1.8.16.custom.min.js" $ ""
    H.link ! rel "stylesheet" ! href "/static/jquery-ui/css/ui-lightness/jquery-ui-1.8.16.custom.css"
    H.style $ ".CodeMirror {border-style: solid; border-width: 5px; border-color: #888;} .CodeMirror-scroll {height: auto; overflow-y: hidden; overflow-x: auto; width: 100%; }"
  body $ do
    H.p ! A.style "font-family: sans-serif; font-size:20pt;"$ do
      "Editing \""
      H.span ! A.style "color: green;" $ toHtml siteName
      "\""
    H.form ! action (toValue ("/edit/" ++ siteName)) ! method "post" ! A.style "display:inline;" $ do
      textarea ! A.id "programEditor" ! name "theprogram" ! rows "60" ! cols "150" $ toHtml prog
      H.br
      input ! type_ "submit" ! value "Save"
    H.a ! href (toValue ("/run/" ++ siteName)) $ "Run!"
    -- H.form ! action (toValue ("/upload/" ++ siteName)) ! method "post" ! enctype "multipart/form-data" $ do
    --   input ! type_ "file" ! name "thefile" ! size "30"
    --   H.br
    --   input ! type_ "text" ! name "thepath" ! value "/dest/path"
    --   input ! type_ "submit" ! value "Upload"
    H.script $ "$(document).ready(function(){var tArea = document.getElementById('programEditor'); var myCodeMirror = CodeMirror.fromTextArea(tArea); $('input:submit, a').button();});"

      
      
      
      