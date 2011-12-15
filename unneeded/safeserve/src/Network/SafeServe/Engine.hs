{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Network.SafeServe.Engine where

import Control.Monad.Reader hiding (join)
import Control.Monad.State hiding (join)
import Sack
import Sack.Contrib.Middleware.UserMime
import Sack.Contrib.Middleware.NotFound
import Sack.Contrib.Utils hiding (get, put)
import Air.Env hiding (mod)
import Network.SafeServe.Config
import Network.SafeServe.Type
import Network.SafeServe.Utils
import Prelude ()
import SafeBase.ByteString.Char8 (ByteString)
import qualified SafeBase.ByteString.Char8 as B
import qualified Prelude as P
import Air.Data.Record.SimpleLabel hiding (get)
import Data.Maybe
import qualified Data.Default as Default




safeserve :: SafeServeMonad -> Application
safeserve safeserve_monad = safeserve_middleware safeserve_monad (not_found dummy_app)

    
safeserve_middleware :: SafeServeMonad -> Middleware
safeserve_middleware safeserve_monad = 
  
  let safeserve_state                      = execState safeserve_monad def
      mime_filter                     = user_mime (safeserve_state.mimes)
      safeserve_middleware_stack           = use - safeserve_state.middlewares
      safeserve_router_middleware          = use - safeserve_state.router
      pre_installed_middleware_stack  = use - pre_installed_middlewares
  in
  
  use [pre_installed_middleware_stack, mime_filter, safeserve_middleware_stack, safeserve_router_middleware]
  

safeserve_router :: RequestMethod -> ByteString -> AppMonad -> Middleware
safeserve_router route_method route_string app_monad app = \env ->
  if env.request_method == route_method 
    then
      case env.path_info.parse_params route_string of
        Nothing -> app env
        Just (_, params) -> 
          let safeserve_app = run_app_monad - local (put_namespace safeserve_captures params) app_monad 
          in
          safeserve_app env
    
    else
      app env
  
  
  where
    
    run_app_monad :: AppMonad -> Application
    run_app_monad app_monad = \env -> runReaderT app_monad env .flip execStateT Default.def
    

parse_params :: ByteString -> ByteString -> Maybe (ByteString, [(ByteString, ByteString)])
parse_params "*" x = Just (x, [])
parse_params "" ""  = Just ("", [])
parse_params "" _   = Nothing
parse_params "/" "" = Nothing
parse_params "/" "/"  = Just ("/", [])

parse_params t s = 
  
  let template_tokens = t.B.split '/'
      url_tokens      = s.B.split '/'
  
      _template_last_token_matches_everything         = template_tokens.length P.> 0 && template_tokens.last.is "*"
      _template_tokens_length_equals_url_token_length = template_tokens.length == url_tokens.length
  in
  
  if not - _template_last_token_matches_everything || _template_tokens_length_equals_url_token_length
    then Nothing
    else 
      let rs = zipWith capture template_tokens url_tokens
      in
      if rs.all isJust
        then 
          let token_length = template_tokens.length
              location     = B.pack - "/" / (B.unpack - url_tokens.take token_length .B.intercalate "/")
          in
          Just - (location, rs.catMaybes.catMaybes)
        else Nothing
  
  where
    capture x y 
      | x.B.unpack.starts_with ":" = Just - Just (x.B.tail, y)
      | x.is "*" = Just Nothing
      | x == y = Just Nothing
      | otherwise = Nothing
