{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Sack.Contrib.Response where

import SafeBase.ByteString.Char8 (ByteString)
import Data.Maybe
import Sack
import Sack.Contrib.Constants
import Sack.Contrib.Utils
import Air.Env hiding (def)
import Prelude ()
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Sack.Contrib.AirBackports
import Data.Default (def)
import Data.Enumerator (run_, enumList, Enumerator, ($$))
import qualified SafeBase.ByteString.Char8 as Strict

body_bytestring :: Response -> IO Lazy.ByteString
body_bytestring r = r.body.unHackEnumerator.fromEnumerator

redirect :: ByteString -> Maybe Int -> Response -> Response
redirect target code = 
    set_status (code.fromMaybe 302)
  > set_header _Location target

finish :: Response -> Response
finish r 
  | r.status.belongs_to [204, 304]
      = r .delete_header _ContentType
          .set_body def
  | otherwise = r


header :: ByteString -> Response -> Maybe ByteString
header s r = r.headers.get s

has_header :: ByteString -> Response -> Bool
has_header s r = r.header s .isJust

set_header :: ByteString -> ByteString -> Response -> Response
set_header k v r = r { headers = r.headers.put k v }

delete_header :: ByteString -> Response -> Response
delete_header k r = r { headers = r.headers.reject (fst > is k) }
  
set_content_type :: ByteString -> Response -> Response
set_content_type s r = r.set_header _ContentType s

set_content_length :: (Integral a) => a -> Response -> Response
set_content_length i r = r.set_header _ContentLength (i.show_bytestring)

set_body :: HackEnumerator -> Response -> Response
set_body s r = r { body = s }

set_body_bytestring :: Lazy.ByteString -> Response -> Response
set_body_bytestring b r = 
  let enum = b.toEnumerator :: (forall a. Enumerator Strict.ByteString IO a)
  in
  set_body (HackEnumerator enum) r

set_status :: Int -> Response -> Response
set_status i r = r { status = i }

set_last_modified :: ByteString -> Response -> Response
set_last_modified s r = r.set_header _LastModified s
