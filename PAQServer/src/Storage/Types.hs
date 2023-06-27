{-# LANGUAGE DeriveGeneric #-}
module Storage.Types 
    (   Region(..)
    ,   LngLat(..)
    ,   RegionInfo(..)
    ,   Event(..)
    ) where

import GHC.Generics
import Data.Aeson (ToJSON)
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
    fromRow = Event <$> field <*> field <*> field <*> field <*> field

instance ToJSON LngLat
instance ToJSON Region
instance ToJSON RegionInfo
instance ToJSON Event

data LngLat = LngLat {
    lng :: Float,
    lat :: Float
} deriving (Show, Generic)

data Region = Region {
    name :: String,
    level :: Int,
    polygon :: [LngLat]
} deriving (Show, Generic)

data RegionInfo = RegionInfo {
    complete_name :: String,
    status :: String,
    description :: String
} deriving (Show, Generic)

data Event = Event {
    event_id :: Int,
    event_lng :: Float,
    event_lat :: Float,
    creator :: String,
    event_description :: String
} deriving (Show, Generic)
