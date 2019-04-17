module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.ByteString (ByteString)
import System.Environment  
import Event
import Json
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO

main :: IO ()
main = do
  print "Starting..."
  line <- getLine
  print $ C.unpack $ encode $ processEventsFromText $ T.lines $ T.pack $ line
  return ()

  -- if length args == 0 then
  --   print "Provide text string, exiting."
  -- else
  --   return $ C.unpack $ encode $ processEventsFromText $ T.lines $ T.pack $ head args