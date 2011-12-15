{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Sack.Contrib.Utils where

import Control.Arrow ((<<<))
import SafeBase.ByteString.Char8 (ByteString)
import Data.Default
import Data.List (lookup)
import Data.Time
import Sack
import Sack.Contrib.Constants
import Air.Env hiding (def)
import Data.Default (def)
import Network.URI hiding (path)
import Prelude ()
import System.Locale (defaultTimeLocale)
import qualified SafeBase.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.Map as M
import Sack.Contrib.AirBackports

import qualified SafeBase.ByteString.Char8 as Strict

import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

import Data.Enumerator (run_, enumList, Enumerator, ($$))

fromEnumerator :: Monad m => Enumerator Strict.ByteString m Lazy.ByteString -> m Lazy.ByteString
fromEnumerator m = run_ - m $$ EB.consume

toEnumerator :: Monad m => Lazy.ByteString -> Enumerator Strict.ByteString m a
toEnumerator = enumList 1 < Lazy.toChunks

withEnumerator :: Monad m => (Lazy.ByteString -> Lazy.ByteString) -> Enumerator Strict.ByteString m Lazy.ByteString -> m (Enumerator Strict.ByteString m a)
withEnumerator f enum = do
  bytes <- fromEnumerator enum
  return - toEnumerator - f bytes

empty_app :: Application
empty_app = return def

-- | usage: app.use [content_type, cache]
use :: [Middleware] -> Middleware
use [] = id
use xs = xs.reduce (<<<)

-- use the get / put helper to deal with headers
put :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
put k v xs = (k,v) : xs.reject (fst > is k)

get :: (Eq a) => a -> [(a, b)] -> Maybe b
get = lookup

bytesize :: Lazy.ByteString -> Int
bytesize = Lazy.length > from_i

show_bytestring :: (Show a) => a -> ByteString
show_bytestring = show > B.pack

map_both :: (a -> b) -> [(a,a)] -> [(b,b)]
map_both f = map_fst f > map_snd f

as_string :: (String -> String) -> ByteString -> ByteString
as_string f x = x.B.unpack.f.B.pack

dummy_middleware :: Middleware
dummy_middleware = id

dummy_app :: Application
dummy_app _ = return - def { status = 500 }

escape_html :: String -> String
escape_html = concatMap fixChar
  where
    fixChar '&'   = "&amp;"
    fixChar '<'  = "&lt;"
    fixChar '>'  = "&gt;"
    fixChar '\'' = "&#39;"
    fixChar '"'  = "&quot;"
    fixChar x    = [x]

escape_uri :: String -> String
escape_uri = escapeURIString isAllowedInURI

unescape_uri :: String -> String
unescape_uri = unEscapeString

show_status_message :: Int -> Maybe ByteString
show_status_message x = status_code.M.lookup x


httpdate :: UTCTime -> String
httpdate x = x.format_time "%a, %d %b %Y %X GMT"

request_method    :: Env -> RequestMethod
script_name       :: Env -> ByteString
path_info         :: Env -> ByteString
query_string      :: Env -> ByteString
server_name       :: Env -> ByteString
server_port       :: Env -> Int
hack_version      :: Env -> (Int, Int, Int)
hack_url_scheme   :: Env -> HackUrlScheme
hack_input        :: Env -> HackEnumerator
hack_errors       :: Env -> HackErrors
hack_headers       :: Env -> [(ByteString, ByteString)]

request_method  = requestMethod
script_name     = scriptName
path_info       = pathInfo
query_string    = queryString
server_name     = serverName
server_port     = serverPort
hack_version    = hackVersion
hack_url_scheme = hackUrlScheme
hack_input      = hackInput
hack_errors     = hackErrors
hack_headers    = hackHeaders

