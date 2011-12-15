{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

-- the entire logging module is stolen from
-- [innate](http://github.com/manveru/innate/tree/master)

module Sack.Contrib.Middleware.Hub where

import Data.Maybe
import Data.Time
import Air
import Prelude hiding ((.), (^), (>), (+), (-))
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf
import Sack.Contrib.Utils
import Sack.Contrib.AirBackports

#ifdef UNIX
import System.Posix.Process (getProcessID)
#endif

data Severity = 
     Debug
  |  Info
  |  Warn
  |  Error
  |  Fatal
  |  Unknown
  deriving (Show, Eq)

hint :: Severity -> String
hint = show > slice 0 1

type Formatter = Severity -> UTCTime -> Int -> String -> String -> String

type Logger = String -> Severity -> IO ()

hub :: (String -> IO ()) -> Formatter -> String -> Logger
hub stream formatter program = \message severity -> 
  do
    time <- now
    pid <- get_pid
    stream - formatter severity time pid program message

get_pid :: IO Int
get_pid =
#ifdef UNIX
  getProcessID ^ from_i
#else
  return (-1)
#endif

simple_logger :: (String -> IO ()) -> String -> Logger
simple_logger = flip hub simple_formatter

simple_formatter :: Formatter
simple_formatter severity time pid program message =
  printf line_format h t pid l program s
  
  where
    line_format = "%s [%s $%d] %5s | %-25s: %s\n"
    h           = severity.hint
    time_format = "%Y-%m-%d %H:%M:%S"
    t           = time.format_time time_format
    l           = severity.show.upper
    s           = colorize severity message


colorize :: Severity -> String -> String
colorize severity message = color severity (message.text) .show
  where
    level_color = 
      [ (   Debug           , blue    )
      , (   Info            , white   )
      , (   Warn            , yellow  )
      , (   Error           , red     )
      , (   Fatal           , red     )
      , (   Unknown         , green   )
      ]
    
    color severity' = level_color.lookup severity' .fromMaybe green

