{-# LANGUAGE OverloadedStrings #-}


-- | Stolen from rack: serves files below the +root+ given, according to the 
--   path info of the Rack request.

module Sack.Contrib.Middleware.File (file) where

import Data.Default
import Data.List (isInfixOf)
import Data.Maybe
import Sack
import Sack.Contrib.Constants
import Sack.Contrib.Mime
import Sack.Contrib.Response
import Sack.Contrib.Utils
import Sack.Contrib.AirBackports
import Air.Env hiding (Default, def)
import Prelude ()
import System.Directory
import System.FilePath
import qualified SafeBase.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as Lazy
import SafeBase.ByteString.Char8 (ByteString)
import Data.Enumerator.Binary (enumFile)


file :: Maybe ByteString -> Middleware
file root _ = \env -> do
  let path = env.path_info .as_string unescape_uri
  
  if B.unpack ".." `isInfixOf` B.unpack path
    then forbidden
    else serve root path


serve :: Maybe ByteString -> ByteString -> IO Response
serve root fname = do
  cwd <- getCurrentDirectory
  let my_root = ( root ^ B.unpack ).fromMaybe cwd
  let path = my_root / makeRelative "/" (fname.B.unpack)
  
  -- puts - b2u - path
  
  exist <- doesFileExist - b2u path

  if not exist
      then do
        -- puts "not exist"
        path.B.pack.not_found
        
      else
        do 
          -- puts "exist"
          can_read <- path.b2u.getPermissions ^ readable
          
          -- puts - "can_read is: " + show can_read
          if not can_read
            then path.B.pack.no_permission
            else path.serving
    
    where 
      serving path = do
        -- puts "serving path"
        
        let file_enumerator = enumFile - b2u path
        
        size <- path.b2u.file_size ^ from_i
        -- puts - "size is " + show size
        
        
        mtime_str <- path.b2u.file_mtime ^ httpdate
        -- puts - "mtime_str is " + mtime_str
        
        let default_content_type = "application/octet-stream"
        let safe_lookup = lookup_mime_type > fromMaybe default_content_type
        let content_type = path.takeExtension.B.pack.safe_lookup
        
        return - 
          def 
            .set_body (HackEnumerator file_enumerator)
            .set_content_length size
            .set_content_type content_type
            .set_last_modified (B.pack mtime_str)
            .set_status 200

no_permission :: ByteString -> IO Response
no_permission path = do
  putStrLn "no_permission"
  return - 
    def
      .set_status 404
      .set_content_type _TextPlain
      .set_content_length (msg.Lazy.length)
      .set_body_bytestring msg

    where msg = "No permission: " + s2l path + "\n"

not_found :: ByteString -> IO Response
not_found path = return $
  def
    .set_status 404
    .set_content_type _TextPlain
    .set_content_length (msg.Lazy.length)
    .set_body_bytestring msg
  
  where msg = "File not found: " + s2l path + "\n"

forbidden :: IO Response
forbidden = return - 
  def
    .set_status 403
    .set_content_type _TextPlain
    .set_content_length (msg.Lazy.length)
    .set_body_bytestring msg

  where msg = "Forbidden\n"
  