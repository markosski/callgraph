{-# LANGUAGE OverloadedStrings #-}

module Event where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Data.Attoparsec.Text
import Data.Either (rights)
import Control.Applicative
import qualified Stack as S
import Stack (Stack)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T

{-
Example of lines to be parsed.

something something cg_start(funcName, "some description")

something cg_end()
-}
data Event = 
  EventStart 
    { eventName :: Text
    , eventMessage :: Text
    } | 
  EventEnd 
    { eventName :: Text
    } 
  deriving (Show)

type FName = Text -- Function name
type FDescr = Text -- Function description
type CallCount = Int -- Number of times function was called

data EventGraph = EventGraph
  { egState :: Stack Event
  , egEdges :: HashMap (FName, FName) CallCount
  , egLabels :: HashMap FName FDescr
  }
  deriving (Show)

--- Event Parsing
matchStartToken = string "cg_start"
matchEndToken = string "cg_end"
parseFuncName = string "(" *> manyTill anyChar (string ")")

eventStartParser :: Parser Event
eventStartParser = do
  message <- manyTill anyChar matchStartToken
  name    <- parseFuncName
  return $ EventStart (T.pack name) (T.strip $ T.pack message)

eventEndParser :: Parser Event
eventEndParser = do
  manyTill anyChar matchEndToken
  name <- parseFuncName
  return $ EventEnd (T.pack name)

--- inner function for parseLines
parseEventLines :: [Text] -> [Event]
parseEventLines lines = 
  rights $ fmap (\x -> parseOnly (try eventStartParser <|> eventEndParser) x) lines

--- EventGraph and operations
emptyEventGraph :: EventGraph
emptyEventGraph =
  EventGraph 
    (S.empty :: Stack Event)
    (HM.empty :: HashMap (FName, FName) CallCount)
    (HM.empty :: HashMap FName FDescr)

insertDescription :: Event -> EventGraph -> EventGraph
insertDescription event graph = 
  let newDescrMap = HM.insert (eventName event) (eventMessage event) (egLabels graph)
  in graph { egLabels = newDescrMap }

incrementCallCount :: (FName, FName) -> EventGraph -> EventGraph
incrementCallCount eventPair graph =
  let pairCount = HM.lookup eventPair (egEdges graph)
  in case pairCount of
    Just count -> graph { egEdges = HM.insert eventPair (count + 1) (egEdges graph) }
    Nothing -> graph { egEdges = HM.insert eventPair 1 (egEdges graph) }
---

--- Process events
processEvent :: Event -> EventGraph -> EventGraph
processEvent nextEvent graph =
  let topEvent = S.peek $ (egState graph)
  in case nextEvent of
    EventStart _ _ -> case topEvent of
      Just event -> EventGraph newState newEdges newLabels
        where nextEventName = eventName nextEvent
              topEventName = eventName event
              newState = S.push nextEvent (egState graph)
              newEdges = egEdges $ incrementCallCount (topEventName, nextEventName) graph
              newLabels = egLabels $ insertDescription nextEvent graph
      Nothing    -> graph { 
                      egState = S.push nextEvent (egState graph)
                      , egLabels = egLabels $ insertDescription nextEvent graph }

    EventEnd endName -> case topEvent of
      Just event -> if (eventName event) == endName
                    then graph { egState = snd $ S.pop $ (egState graph) }
                    else graph
      Nothing    -> graph

processEvents :: [Event] -> EventGraph -> EventGraph
processEvents events graph = processRecu events
  where processRecu []     = graph
        processRecu (x:xs) = processEvents xs (processEvent x graph)

processEventsFromText :: [Text] -> EventGraph
processEventsFromText lines =
  (`processEvents` emptyEventGraph) $ parseEventLines lines
---