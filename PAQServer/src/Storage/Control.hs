module Storage.Control 
    (   initDb
    ,   Connection
    ,   selectRegions
    ,   selectRegion
    ,   selectEvents
    ,   fill
    ,   refill
    ) where

import Database.SQLite.Simple
import qualified Data.Text as T

import Storage.Types
import Storage.Schema
import qualified Storage.Fill.Regions
import qualified Storage.Fill.Coords
import qualified Storage.Fill.Events

-- | Initialize database
initDb :: IO Connection
initDb = do 
            db <- open "data.db"
            form db
            return db

-- | Fill database with predefined data
fill :: Connection -> IO ()
fill db = do
    Storage.Fill.Regions.fill db
    Storage.Fill.Coords.fill db
    Storage.Fill.Events.fill db

-- | Clear and fill database with predefined data
refill :: Connection -> IO ()
refill db = do
    recreate db
    fill db

-- | Select all regions with their coordinates
selectRegions :: Connection -> IO [Region]
selectRegions db = query_ db 
  "SELECT r.region_name, r.level, group_concat(p.lng || ';' || p.lat) \
  \FROM regions r \
  \LEFT JOIN points p ON p.region_name = r.region_name \
  \GROUP BY r.region_name, r.level \
  \ORDER BY p.order_number ASC"

-- | Select region info by name
selectRegion :: Connection -> T.Text -> IO (Maybe RegionInfo)
selectRegion db regionName = do
    regions <- queryNamed db 
        "SELECT r.complete_name, r.status, r.description \
        \FROM regions r \
        \WHERE r.region_name = :x" [":x" := regionName]
    case regions of
        [] -> return Nothing
        (r:_) -> return $ Just r

selectEvents :: Connection -> IO [Event]
selectEvents db = query_ db 
    "SELECT e.id, e.lng, e.lat, e.creator, e.description \
    \FROM events e"
