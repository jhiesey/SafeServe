module Network.SafeServe.DSL where

import Control.Monad.Reader
import Control.Monad.State
import Sack
import Sack.Contrib.Middleware.Censor
import Sack.Contrib.Middleware.Config
import Sack.Contrib.Middleware.IOConfig
import Sack.Contrib.Middleware.Static
import Sack.Contrib.Response
import Sack.Contrib.Constants
import Air
import Network.SafeServe.Config
import Network.SafeServe.Engine
import Network.SafeServe.Type
import Network.SafeServe.Utils
import Prelude hiding ((.), (>), (^), (-))
import qualified Control.Monad.State as State
import SafeBase.ByteString.Char8 (ByteString)
-- import qualified Data.ByteString.Lazy.Char8 as Lazy

import Air.Data.Record.SimpleLabel hiding (get)

app :: Application -> AppMonad
app f = ask >>= (rio . f) >>= State.put

middleware :: Middleware -> SafeServeMonad
middleware x = modM __middlewares - insert_last x

get, put, post, delete :: ByteString -> AppMonad -> SafeServeMonad
get    = add_route GET
put    = add_route PUT
post   = add_route POST
delete = add_route DELETE


add_route :: RequestMethod -> ByteString -> AppMonad -> SafeServeMonad
add_route route_method route_string app_monad = do
  modM __router - insert_last - safeserve_router route_method route_string app_monad
      
before :: (Env -> RIO Env) -> SafeServeMonad
before = middleware . ioconfig

after :: (Response -> RIO Response) -> SafeServeMonad
after = middleware . censor

mime :: ByteString -> ByteString -> SafeServeMonad
mime k v = modM __mimes - insert_last (k,v)

public :: Maybe ByteString -> [ByteString] -> SafeServeMonad
public r xs = (static r xs) . middleware

text :: ByteString -> AppMonad
text x = do
  (set_content_type _TextPlain) . update
  (set_body_bytestring x) . update

html :: ByteString -> AppMonad
html x = do
  (set_content_type _TextHtml) . update
  (set_body_bytestring x) . update


captures :: AppMonadT [(ByteString, ByteString)]
captures = ask ^ namespace safeserve_captures
