{-# LANGUAGE OverloadedStrings #-}

module Sack.Contrib.Middleware.XForwardedForToRemoteHost (x_forwarded_for_to_remote_host) where

import Sack
import Air.Env hiding (Default, def)
import Prelude ()
import SafeBase.ByteString.Char8 ()

x_forwarded_for_to_remote_host :: Middleware
x_forwarded_for_to_remote_host app = \env ->
  case env.httpHeaders.lookup "X-Forwarded-For" of
    Nothing -> app env
    Just ip -> 
      let newHackHeaders = ("RemoteHost", ip) : env.hackHeaders
      in
      
      app - env {hackHeaders = newHackHeaders}