module Sack.Contrib.AirBackports where
  
import SafeBase.RIO
import qualified SafeBase.RIO as S

import qualified Codec.Binary.UTF8.String as Codec
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
-- import System.IO
import System.Locale (defaultTimeLocale)
-- import qualified System.IO.Unsafe as Unsafe
import Text.RegexPR
import Data.Maybe

import Air.Env hiding (Default, def)
import Prelude ()

b2u, u2b :: String -> String
b2u = Codec.decodeString
u2b = Codec.encodeString


-- file_size :: String -> IO Integer
-- file_size path = withFile path ReadMode hFileSize
-- 
-- file_mtime :: String -> IO UTCTime
-- file_mtime path = 
--   getModificationTime path ^ seconds ^ from_i ^ posixSecondsToUTCTime
--   where seconds (TOD s _) = s
  
now :: RIO UTCTime
now = S.getCurrentTime

format_time :: String -> UTCTime -> String
format_time = formatTime defaultTimeLocale

-- purify :: IO a -> a
-- purify = Unsafe.unsafePerformIO

simple_time_format :: String
simple_time_format = "%Y-%m-%d %H:%M:%S %Z"

parse_time :: String -> String -> UTCTime
parse_time = readTime defaultTimeLocale


split_raw :: String -> String -> [String]
split_raw re xs
  | xs.match re .isJust = splitRegexPR re xs
  | otherwise           = [xs]

split :: String -> String -> [String]
split re xs = split_raw re xs .reject empty

split' :: String -> [String]
split' s = s.lines.reject empty

sub :: String -> String -> String -> String
sub = subRegexPR

gsub :: String -> String -> String -> String
gsub = gsubRegexPR


type RegexResult = ( String, (String, String) )
type MatchList   = [ (Int, String) ]
match :: String -> String -> Maybe (RegexResult, MatchList)
match = matchRegexPR

strip :: String -> String
strip s = s.sub "^\\s*" "" .reverse .sub "^\\s*" "" .reverse

empty :: String -> Bool
empty s = case s.match("\\S") of
  Just _ -> False
  Nothing -> True
  