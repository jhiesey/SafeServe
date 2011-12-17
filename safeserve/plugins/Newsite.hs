{-# LANGUAGE OverloadedStrings #-}

module Newsite where

import API
import SafeBase.Framework as F
import SafeBase.RIO
import qualified SafeBase.ByteString.Char8 as B

import SafeBlaze.Html5
import SafeBlaze.Html5.Attributes
import SafeBlaze.Renderer.String
import qualified SafeBlaze.Html5 as H
import qualified SafeBlaze.Html5.Attributes as A

resource = Interface { function = theApp }

theApp :: Application
theApp =  F.safeserve $ do

  F.get "/" $ do
    lift $ lift $ rioPutStrLn "Testing"
    F.html $ B.pack runTemplate
	  
  F.get "/anotherPage" $ do
    F.html "<html><head><title>Another Page</title></head><body>And here's a page without a template</body></html>"

  F.get "/readfile" $ do
    contents <- lift $ lift $ do
      printPath
      readFileRIO "riotest.txt"
    F.text $ B.pack contents
    
  F.get "/counthits" $ do
    hitCount <- lift $ lift $ do
      currFile <- readFileRIO "hits.txt"
      let newCount = ((read currFile) :: Int) + 1
      newCount `seq` writeFileRIO "hits.txt" $ show newCount
      return newCount
    F.text $ B.pack $ show hitCount

runTemplate :: String
runTemplate = renderHtml templ

templ ::  Html
templ = docTypeHtml $ do
  H.head $ do
    H.title "Welcome to SafeServe"
  H.body $ do
    H.p "This is a very simple sample app page."
    H.p $ H.a ! A.href "/run/Newsite/anotherPage" $ "Here's a link to another page"
    H.p $ H.a ! A.href "/run/Newsite/readfile" $ "This page demonstrates reading from a file"
    H.p $ H.a ! A.href "/run/Newsite/counthits" $ "And a hit counter"
    H.p $ "Hi Feross"
