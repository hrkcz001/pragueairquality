{-# LANGUAGE OverloadedStrings #-}
module Storage.Control 
    (   initDb
    ,   Connection
    ,   selectRegions
    ) where

import Storage.Types
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

initDb :: IO Connection
initDb = do 
            db <- open "data.db"
            form db
            fill db
            return db

form :: Connection -> IO ()
form db = do
            execute_ db "CREATE TABLE IF NOT EXISTS points (order INT, lng REAL, lat REAL, region_name TEXT, PRIMARY KEY (order, region_name), FOREIGN KEY(region_name) REFERENCES regions(name)"
            execute_ db "CREATE TABLE IF NOT EXISTS regions (name TEXT PRIMARY KEY, level INT)"

fill :: Connection -> IO ()
fill db = do
            execute db "INSERT INTO regions (name, geojson) VALUES (?, ?)" ("test" :: T.Text, "test" :: T.Text)

selectRegions :: Connection -> IO [Region]
selectRegions db = do
    query_ db "SELECT id, name, geojson FROM regions" :: IO [(Int, String, String)]
