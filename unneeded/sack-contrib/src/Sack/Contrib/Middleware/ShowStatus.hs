{-# LANGUAGE QuasiQuotes #-}

-- | Stolen from rack:
--   catches all empty responses the app it wraps and replaces them 
--   with a site explaining the error. 
--   Additional details can be put into hack2.showstatus.detail.
--   and will be shown as HTML.  If such details exist, the error page
--   is always rendered, even if the reply was not empty.
--
--   Note: it appears that only when content is empty will this
--         message be shown.

module Sack.Contrib.Middleware.ShowStatus (show_status) where


import Data.Maybe
import Sack
import Sack.Contrib.Constants
import Sack.Contrib.Request hiding (body)
import Sack.Contrib.Response
import Sack.Contrib.Utils
import Air
import Air.Heavy
import Air.TH
import Prelude hiding ((.), (^), (>), head)
import qualified SafeBase.ByteString.Char8 as B

show_status :: Middleware
show_status app = \env -> do
  response <- app env
  
  let content_length = response.header _ContentLength .fromMaybe "0"
  let content_empty = content_length .read .(<= (0 :: Int))
  
  if response.status >= 400 && content_empty
    then 
      let 
          i       = response.status
          message = i.show_status_message .fromMaybe (i.show)
          detail  = env.custom.get "hack2.showstatus.detail" .fromMaybe message
          result  = template message detail env response 
                      .unescape_unicode_xml.u2b.B.pack
          size    = result.bytesize
      in
      return - 
        response
          .set_body result
          .set_content_type _TextHtml
          .set_content_length size
    
    else return response


template :: String -> String -> Env -> Response -> String
template message detail env response = 
  let h = escape_html > escape_unicode_xml in [$here|


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en">
<head>
  <meta http-equiv="content-type" content="text/html; charset=utf-8" />
  <title>#{h message } at #{h (env.path.unescape_uri.b2u) }</title>
  <meta name="robots" content="NONE,NOARCHIVE" />
  <style type="text/css">
    html * { padding:0; margin:0; }
    body * { padding:10px 20px; }
    body * * { padding:0; }
    body { font:small sans-serif; background:#eee; }
    body>div { border-bottom:1px solid #ddd; }
    h1 { font-weight:normal; margin-bottom:.4em; }
    h1 span { font-size:60%; color:#666; font-weight:normal; }
    table { border:none; border-collapse: collapse; width:100%; }
    td, th { vertical-align:top; padding:2px 3px; }
    th { width:12em; text-align:right; color:#666; padding-right:.5em; }
    #info { background:#f6f6f6; }
    #info ol { margin: 0.5em 4em; }
    #info ol li { font-family: monospace; }
    #summary { background: #ffc; }
    #explanation { background:#eee; border-bottom: 0px none; }
  </style>
</head>
<body>
  <div id="summary">
    <h1>#{h message } <span>(#{ response.status })</span></h1>
    <table class="meta">
      <tr>
        <th>Request Method:</th>
        <td>#{ env.request_method }</td>
      </tr>
      <tr>
        <th>Request URL:</th>
      <td>#{h (env.url.unescape_uri.b2u) }</td>
      </tr>
    </table>
  </div>
  <div id="info">
    <p>#{h detail }</p>
  </div>

  <div id="explanation">
    <p>
    You're seeing this error because you use <code>Sack.Contrib.Middleware.ShowStatus</code>.
    </p>
  </div>
</body>
</html>


|]
