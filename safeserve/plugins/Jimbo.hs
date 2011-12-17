{-# LANGUAGE OverloadedStrings #-}

module Jimbo where

import API
import SafeBase.Framework as F
import SafeBase.RIO
import qualified SafeBase.ByteString.Char8 as B

import SafeBlaze.Html5
import SafeBlaze.Html5.Attributes
import SafeBlaze.Renderer.String
import qualified SafeBlaze.Html5 as H
import qualified SafeBlaze.Html5.Attributes as A
import System.IO.Unsafe


resource = Interface { function = theApp }

theApp :: Application
theApp =  F.safeserve $ do

  F.get "/" $ do
    lift $ lift $ rioPutStrLn "Testing"
    F.html $ B.pack runTemplate
	  
  F.get "/anotherPage" $ do
    F.html "<html><head><title>Another Page</title></head><body>And here's a page without a template</body></html>"

runTemplate :: String
runTemplate = renderHtml templ

templ ::  Html
templ = docTypeHtml $ do
  H.head $ do
    H.title "Welcome to SafeServe"
  H.body $ do
    H.p "This is a very simple sample app page."
    H.a ! A.href "/run/Jimbo/anotherPage" $ "Here's a link to another page"
