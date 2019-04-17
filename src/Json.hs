{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Json where

import Event
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics
import Data.Aeson (ToJSON(..), Value(..), object, (.=))
import Util (calculateEdgeWeight)

data Label = Label 
  { labelKey :: Text
  , labelValue :: Text
  } deriving (Show)

data Edge = Edge
  { fromEdge :: Text
  , toEdge :: Text
  , getWeight :: Float
  } deriving (Show)

instance ToJSON Label where
  toJSON label = object
    [ "id" .= toJSON (labelKey label)
    , "label" .= toJSON (labelValue label)
    ]

instance ToJSON Edge where
  toJSON edge = object
    [ "from" .= toJSON (fromEdge edge)
    , "to" .= toJSON (toEdge edge)
    , "width" .= toJSON (getWeight edge)
    ]

instance ToJSON EventGraph where
  toJSON eg = object
    [ "nodes" .= toJSON [Label (fst x) (snd x) | x <- HM.toList $ egLabels eg]
    , "edges" .= toJSON [
      Edge 
        (fst $ fst x) 
        (snd $ fst x) 
        (calculateEdgeWeight $ fromIntegral $ snd x) | x <- HM.toList $ egEdges eg]
    ]