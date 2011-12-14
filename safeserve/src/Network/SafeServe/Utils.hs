{-# LANGUAGE OverloadedStrings #-}

module Network.SafeServe.Utils where

import Control.Monad.State
import Hack2
import Air.Env
import Prelude ()
-- import Data.ByteString.UTF8 (fromString, toString)

import qualified SafeBase.ByteString.Char8 as B
import SafeBase.ByteString.Char8 (ByteString)

namespace :: ByteString -> Env -> [(ByteString, ByteString)]
namespace x env =
  env
    .hackHeaders
    .select (fst > (x `B.isPrefixOf`))
    .map_fst (B.drop (x.B.length))

put_namespace :: ByteString -> [(ByteString, ByteString)] -> Env -> Env
put_namespace x xs env = 
  let adds             = xs.map_fst (x +)
      new_headers      = adds.map fst
      new_hack_headers = 
        env.hackHeaders.reject (fst > belongs_to new_headers) ++ adds
  in
  env {hackHeaders = new_hack_headers}



insert_last :: a -> [a] -> [a]
insert_last x xs = xs ++ [x]

update :: (MonadState a m, Functor m) => (a -> a) -> m ()
update = modify

