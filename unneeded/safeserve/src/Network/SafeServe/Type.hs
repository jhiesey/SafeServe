-- # LANGUAGE TemplateHaskell #

module Network.SafeServe.Type where

import Control.Monad.Reader
import Control.Monad.State
import Air.Data.Default
import Sack
import Sack.Contrib.Utils
import SafeBase.ByteString.Char8 (ByteString)

-- import Air.TH

type AppReader    = Env
type AppState     = Response
type AppMonadT    = ReaderT AppReader (StateT AppState RIO)
type AppMonad     = AppMonadT ()


data SafeServeState = SafeServeState
  {
    middlewares     :: [Middleware]
  , router          :: [Middleware]
  , mimes           :: [(ByteString, ByteString)]
  }

-- mkDefault ''SafeServeState
-- mkLabel ''SafeServeState

type SafeServeMonadT a = State SafeServeState a
type SafeServeMonad    = SafeServeMonadT ()
