{-# LANGUAGE DeriveGeneric #-}
module Storage.Types 
    (   Region(..)
    ,   LngLat(..)
    ,   RegionInfo(..)
    ,   Event(..)
    ,   EventInfo(..)
    ,   InsertEvent(..)
    ) where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import Data.List.Split (splitOn)

instance FromRow Region where
  fromRow = Region <$> field <*> field <*> (parsePoints <$> field)
    where parsePoints :: Maybe String -> [LngLat]
          parsePoints (Just s) = map parseLngLat $ splitOn "," s
          parsePoints Nothing = []

          parseLngLat :: String -> LngLat
          parseLngLat s = case splitOn ";" s of
            [lngStr, latStr] -> LngLat (read lngStr) (read latStr)
            _ -> error "Invalid LngLat format"

instance FromRow RegionInfo where
    fromRow = RegionInfo <$> field <*> field <*> field

instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field

instance FromRow EventInfo where
    fromRow = EventInfo <$> field <*> field

instance ToJSON LngLat
instance ToJSON Region
instance ToJSON RegionInfo
instance ToJSON Event
instance ToJSON EventInfo
instance FromJSON InsertEvent

data LngLat = LngLat {
    point_lng :: Float,
    point_lat :: Float
} deriving (Show, Generic)

data Region = Region {
    region_name :: String,
    region_level :: Int,
    region_polygon :: [LngLat]
} deriving (Show, Generic)

data RegionInfo = RegionInfo {
    region_complete_name :: String,
    region_status :: String,
    region_description :: String
} deriving (Show, Generic)

data Event = Event {
    event_id :: Int,
    event_lng :: Float,
    event_lat :: Float
} deriving (Show, Generic)

data EventInfo = EventInfo {
    event_creator :: String,
    event_description :: String
} deriving (Show, Generic)

data InsertEvent = InsertEvent {
    insert_event_lng :: Float,
    insert_event_lat :: Float,
    insert_event_creator :: String,
    insert_event_description :: String
} deriving (Show, Generic)
