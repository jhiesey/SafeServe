{-# LANGUAGE OverloadedStrings #-}

module EnumExamples where

import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.ByteString.Char8 as Strict

import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

import Data.Enumerator

import Air.Env
import Air.Extra
import Prelude ()


fromEnumerator :: Monad m => Enumerator Strict.ByteString m B.ByteString -> m B.ByteString
fromEnumerator m = run_ - m $$ EB.consume

toEnumerator :: Monad m => B.ByteString -> Enumerator Strict.ByteString m a
toEnumerator = enumList 1 < B.toChunks

withEnumerator :: Monad m => (B.ByteString -> B.ByteString) -> Enumerator Strict.ByteString m B.ByteString -> m (Enumerator Strict.ByteString m a)
withEnumerator f enum = do
  bytes <- fromEnumerator enum
  return - toEnumerator - f bytes

main :: IO ()
main = do
  enum <- withEnumerator (B.unpack > upper > B.pack) - toEnumerator (B.pack "abcd")
  bytes <- fromEnumerator enum
  
  B.putStrLn bytes
