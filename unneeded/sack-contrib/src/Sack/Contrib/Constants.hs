{-# LANGUAGE OverloadedStrings #-}


module Sack.Contrib.Constants where

import Air.Light
import Prelude hiding ((.))
import Data.Map
import SafeBase.ByteString.Char8 (ByteString)

status_with_no_entity_body :: [Int]
status_with_no_entity_body = [100 .. 199] ++ [204, 304]


-- header constants
_CacheControl                   :: ByteString
_Connection                     :: ByteString
_Date                           :: ByteString
_Pragma                         :: ByteString
_TransferEncoding               :: ByteString
_Upgrade                        :: ByteString
_Via                            :: ByteString
_Accept                         :: ByteString
_AcceptCharset                  :: ByteString
_AcceptEncoding                 :: ByteString
_AcceptLanguage                 :: ByteString
_Authorization                  :: ByteString
_Cookie                         :: ByteString
_Expect                         :: ByteString
_From                           :: ByteString
_Host                           :: ByteString
_IfModifiedSince                :: ByteString
_IfMatch                        :: ByteString
_IfNoneMatch                    :: ByteString
_IfRange                        :: ByteString
_IfUnmodifiedSince              :: ByteString
_MaxForwards                    :: ByteString
_ProxyAuthorization             :: ByteString
_Range                          :: ByteString
_Referer                        :: ByteString
_UserAgent                      :: ByteString
_Age                            :: ByteString
_Location                       :: ByteString
_ProxyAuthenticate              :: ByteString
_Public                         :: ByteString
_RetryAfter                     :: ByteString
_Server                         :: ByteString
_SetCookie                      :: ByteString
_TE                             :: ByteString
_Trailer                        :: ByteString
_Vary                           :: ByteString
_Warning                        :: ByteString
_WWWAuthenticate                :: ByteString
_Allow                          :: ByteString
_ContentBase                    :: ByteString
_ContentEncoding                :: ByteString
_ContentLanguage                :: ByteString
_ContentLength                  :: ByteString
_ContentLocation                :: ByteString
_ContentMD5                     :: ByteString
_ContentRange                   :: ByteString
_ContentType                    :: ByteString
_ETag                           :: ByteString
_Expires                        :: ByteString
_LastModified                   :: ByteString
_ContentTransferEncoding        :: ByteString



_CacheControl                   =  "Cache-Control"        
_Connection                     =  "Connection"           
_Date                           =  "Date"                 
_Pragma                         =  "Pragma"               
_TransferEncoding               =  "Transfer-Encoding"    
_Upgrade                        =  "Upgrade"              
_Via                            =  "Via"                  
_Accept                         =  "Accept"               
_AcceptCharset                  =  "Accept-Charset"       
_AcceptEncoding                 =  "Accept-Encoding"      
_AcceptLanguage                 =  "Accept-Language"      
_Authorization                  =  "Authorization"        
_Cookie                         =  "Cookie"               
_Expect                         =  "Expect"               
_From                           =  "From"                 
_Host                           =  "Host"                 
_IfModifiedSince                =  "If-Modified-Since"    
_IfMatch                        =  "If-Match"             
_IfNoneMatch                    =  "If-None-Match"        
_IfRange                        =  "If-Range"             
_IfUnmodifiedSince              =  "If-Unmodified-Since"  
_MaxForwards                    =  "Max-Forwards"         
_ProxyAuthorization             =  "Proxy-Authorization"  
_Range                          =  "Range"                
_Referer                        =  "Referer"              
_UserAgent                      =  "User-Agent"           
_Age                            =  "Age"                  
_Location                       =  "Location"             
_ProxyAuthenticate              =  "Proxy-Authenticate"   
_Public                         =  "Public"               
_RetryAfter                     =  "Retry-After"          
_Server                         =  "Server"               
_SetCookie                      =  "Set-Cookie"           
_TE                             =  "TE"                   
_Trailer                        =  "Trailer"              
_Vary                           =  "Vary"                 
_Warning                        =  "Warning"              
_WWWAuthenticate                =  "WWW-Authenticate"     
_Allow                          =  "Allow"                
_ContentBase                    =  "Content-Base"         
_ContentEncoding                =  "Content-Encoding"     
_ContentLanguage                =  "Content-Language"     
_ContentLength                  =  "Content-Length"       
_ContentLocation                =  "Content-Location"     
_ContentMD5                     =  "Content-MD5"          
_ContentRange                   =  "Content-Range"        
_ContentType                    =  "Content-Type"         
_ETag                           =  "ETag"                 
_Expires                        =  "Expires"              
_LastModified                   =  "Last-Modified"        
_ContentTransferEncoding        =  "Content-Transfer-Encodeing"


-- mime type
_TextPlain                  :: ByteString
_TextHtml                   :: ByteString
_TextPlainUTF8              :: ByteString
_TextHtmlUTF8               :: ByteString

_TextPlain     = "text/plain"
_TextHtml      = "text/html"
_TextPlainUTF8 = "text/plain; charset=UTF-8"
_TextHtmlUTF8  = "text/html; charset=UTF-8"


-- status code
status_code :: Map Int ByteString
status_code =
  [  x       100          "Continue"
  ,  x       101          "Switching Protocols"
  ,  x       200          "OK"
  ,  x       201          "Created"
  ,  x       202          "Accepted"
  ,  x       203          "Non-Authoritative Information"
  ,  x       204          "No Content"
  ,  x       205          "Reset Content"
  ,  x       206          "Partial Content"
  ,  x       300          "Multiple Choices"
  ,  x       301          "Moved Permanently"
  ,  x       302          "Found"
  ,  x       303          "See Other"
  ,  x       304          "Not Modified"
  ,  x       305          "Use Proxy"
  ,  x       307          "Temporary Redirect"
  ,  x       400          "Bad Request"
  ,  x       401          "Unauthorized"
  ,  x       402          "Payment Required"
  ,  x       403          "Forbidden"
  ,  x       404          "Not Found"
  ,  x       405          "Method Not Allowed"
  ,  x       406          "Not Acceptable"
  ,  x       407          "Proxy Authentication Required"
  ,  x       408          "Request Timeout"
  ,  x       409          "Conflict"
  ,  x       410          "Gone"
  ,  x       411          "Length Required"
  ,  x       412          "Precondition Failed"
  ,  x       413          "Request Entity Too Large"
  ,  x       414          "Request-URI Too Large"
  ,  x       415          "Unsupported Media Type"
  ,  x       416          "Requested Range Not Satisfiable"
  ,  x       417          "Expectation Failed"
  ,  x       500          "Internal Server Error"
  ,  x       501          "Not Implemented"
  ,  x       502          "Bad Gateway"
  ,  x       503          "Service Unavailable"
  ,  x       504          "Gateway Timeout"
  ,  x       505          "HTTP Version Not Supported"
  ] .to_h
  where x a b = (a, b)
