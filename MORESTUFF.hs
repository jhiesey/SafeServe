{-# LANGUAGE OverloadedStrings #-}

module LaLaLa where

import SafeBase.ByteString (ByteString)
import qualified SafeBase.ByteString.Char8 as B
import qualified SafeBase.ByteString.Char8 as Strict
import SafeBase.RIO
import Data.Default
import Control.Monad.Reader
import qualified Control.Monad.State as ST
import Data.Maybe
import Data.List
import Control.Arrow
import qualified Data.Foldable as DF
import Data.Monoid
import System.FilePath ((</>))


type Application = Env -> RIO Response
type Middleware  = Application -> Application

data RequestMethod =
     OPTIONS
  |  GET
  |  HEAD
  |  POST
  |  PUT
  |  DELETE
  |  TRACE
  |  CONNECT
  deriving (Show, Read, Eq)

data SackUrlScheme = HTTP | HTTPS deriving (Show, Read, Eq)

data Env = Env 
  {  requestMethod  :: RequestMethod
  ,  scriptName     :: ByteString
  ,  pathInfo       :: ByteString
  ,  queryString    :: ByteString
  ,  serverName     :: ByteString
  ,  serverPort     :: Int
  ,  httpHeaders    :: [(ByteString, ByteString)]
  ,  hackVersion    :: (Int, Int, Int)
  ,  hackUrlScheme  :: SackUrlScheme
  ,  hackInput      :: ByteString
  ,  hackErrors     :: ByteString
  ,  hackHeaders    :: [(ByteString, ByteString)]
  }
  deriving (Show)

data Response = Response
  {  status   :: Int
  ,  headers  :: [(ByteString, ByteString)]
  ,  body     :: ByteString
  }
  deriving (Show)

instance Default RequestMethod where
  def = GET

instance Default SackUrlScheme where
  def = HTTP

instance Default Response where
  def = Response 
    {
      status  = 200
    , headers = []
    , body    = def
    }

instance Default Env where
  def = Env 
    {
        requestMethod = def
      , scriptName    = B.empty
      , pathInfo      = B.empty
      , queryString   = B.empty
      , serverName    = B.empty
      , serverPort    = def
      , httpHeaders   = def
      , hackVersion   = currentVersion
      , hackUrlScheme = def
      , hackInput     = def
      , hackErrors    = def
      , hackHeaders   = def
    }
    where
      currentVersion = (2011, 6, 19)    
      
type AppReader    = Env
type AppState     = Response
type AppMonadT    = ReaderT AppReader (ST.StateT AppState RIO)
type AppMonad     = AppMonadT ()


data SafeServeState = SafeServeState
  {
    middlewares     :: [Middleware]
  , router          :: [Middleware]
  , mimes           :: [(ByteString, ByteString)]
  }

type SafeServeMonadT a = ST.State SafeServeState a
type SafeServeMonad    = SafeServeMonadT ()


pre_installed_middlewares :: [Middleware]
pre_installed_middlewares = 
  [
  --   content_length
  -- , content_type default_content_type
  ]
  -- where
  --   default_content_type = "text/plain; charset=UTF-8"
    
insert_last :: a -> [a] -> [a]
insert_last x xs = xs ++ [x]

update :: (ST.MonadState a m, Functor m) => (a -> a) -> m ()
update = ST.modify

instance Default B.ByteString where
  def = B.empty


safeserve :: SafeServeMonad -> Application
safeserve safeserve_monad = safeserve_middleware safeserve_monad (not_found dummy_app)

-- | usage: app.use [content_type, cache]
use :: [Middleware] -> Middleware
use [] = id
use xs = foldl1 (<<<) xs

dummy_middleware :: Middleware
dummy_middleware = id

dummy_app :: Application
dummy_app _ = return $ def { status = 500 }

-- use the get / put helper to deal with headers
put :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
put k v xs = (k,v) : reject ((==) k . fst) xs

get :: (Eq a) => a -> [(a, b)] -> Maybe b
get = lookup


not_found :: Middleware
not_found _ = \_ -> return $
  (set_status 404) $ (set_content_type $ Strict.pack "text/html") $ (set_content_length 0) def

set_status :: Int -> Response -> Response
set_status i r = r { status = i }

set_header :: ByteString -> ByteString -> Response -> Response
set_header k v r = r { headers = put k v (headers r) }

set_body_bytestring :: ByteString -> Response -> Response
set_body_bytestring = set_body

set_content_type :: ByteString -> Response -> Response
set_content_type s r = set_header "Content-Type" s r

set_content_length :: (Integral a) => a -> Response -> Response
set_content_length i r = set_header "Content-Length" (Strict.pack $ show i) r

set_body :: ByteString -> Response -> Response
set_body s r = r { body = s }


-- reduce = foldl1

safeserve_captures :: ByteString
safeserve_captures = "safeserve-captures-"

    
safeserve_middleware :: SafeServeMonad -> Middleware
safeserve_middleware _ = id
-- safeserve_middleware safeserve_monad = 
  -- 
  -- let safeserve_state                      = execState safeserve_monad def
  --     mime_filter                     = user_mime (mimes safeserve_state)
  --     safeserve_middleware_stack           = use $ (middlewares safeserve_state)
  --     safeserve_router_middleware          = use $ (router safeserve_state)
  --     pre_installed_middleware_stack  = use $ pre_installed_middlewares
  -- in
  -- 
  -- use [pre_installed_middleware_stack, mime_filter, safeserve_middleware_stack, safeserve_router_middleware]
  
map_fst :: (a -> b) -> [(a, c)] -> [(b, c)]
map_fst f = map(\(a,b) -> (f a, b))

select, reject :: (a -> Bool) -> [a] -> [a]
select   = filter
reject f = filter (not . f)

belongs_to :: (DF.Foldable t, Eq a) => t a -> a -> Bool
belongs_to = flip DF.elem
  
put_namespace :: ByteString -> [(ByteString, ByteString)] -> Env -> Env
put_namespace x xs env = 
	let adds             = map_fst (mappend x) xs
	    new_headers      = map fst adds
	    new_hack_headers = 
        -- env.hackHeaders.reject (fst > belongs_to new_headers) ++ adds
	      reject (fst >>> belongs_to new_headers) (hackHeaders env) ++ adds 
	in
	env {hackHeaders = new_hack_headers}
  

safeserve_router :: RequestMethod -> ByteString -> AppMonad -> Middleware
safeserve_router route_method route_string app_monad app = \env ->
  if requestMethod env == route_method 
    then
      case parse_params (pathInfo env) route_string of
        Nothing -> app env
        Just (_, params) -> 
          let safeserve_app = run_app_monad $ local (put_namespace safeserve_captures params) app_monad 
          in
          safeserve_app env
    
    else
      app env
  
  
  where
    
    run_app_monad :: AppMonad -> Application
    run_app_monad app_monad = \env -> ST.execStateT (runReaderT app_monad env) def
    

parse_params :: ByteString -> ByteString -> Maybe (ByteString, [(ByteString, ByteString)])
parse_params "*" x = Just (x, [])
parse_params "" ""  = Just ("", [])
parse_params "" _   = Nothing
parse_params "/" "" = Nothing
parse_params "/" "/"  = Just ("/", [])

parse_params t s = 
  
  let template_tokens = B.split '/' t
      url_tokens      = B.split '/' s
  
      _template_last_token_matches_everything         = length template_tokens > 0 && last template_tokens == "*"
      _template_tokens_length_equals_url_token_length = length template_tokens == length url_tokens
  in
  
  if not $ _template_last_token_matches_everything || _template_tokens_length_equals_url_token_length
    then Nothing
    else 
      let rs = zipWith capture template_tokens url_tokens
      in
      if all isJust rs
        then 
          let token_length = length template_tokens
              location     = B.pack $ "/" </> (B.unpack $ B.intercalate "/" (take token_length url_tokens))
          in
          Just $ (location, catMaybes $ catMaybes rs)
        else Nothing
  
  where
    capture x y 
      | isPrefixOf ":" $ B.unpack x = Just $ Just (B.tail x, y)
      | x == "*" = Just Nothing
      | x == y = Just Nothing
      | otherwise = Nothing
	  
main = do
	putStrLn "Hi"