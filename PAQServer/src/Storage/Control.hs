{-# LANGUAGE OverloadedStrings #-}
module Storage.Control 
    (   initDb
    ,   Connection
    ,   selectRegions
    ,   clear
    ,   fill
    ) where

import Storage.Types
import Storage.Coords
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad (forM_)
import Data.Bits (Bits(clearBit))

initDb :: IO Connection
initDb = do 
            db <- open "data.db"
            form db
            return db

form :: Connection -> IO ()
form db = do
            execute_ db "CREATE TABLE IF NOT EXISTS points (\ 
                        \order_number INTEGER, \ 
                        \lng REAL, \
                        \lat REAL, \
                        \region_name TEXT, \
                        \PRIMARY KEY (order_number, region_name), \
                        \FOREIGN KEY(region_name) REFERENCES regions(name)\
                        \ ON DELETE CASCADE)"
            execute_ db "CREATE TABLE IF NOT EXISTS regions ( \
                        \region_name TEXT PRIMARY KEY, \
                        \level INTEGER NOT NULL CHECK (level >= 0))"

fill :: Connection -> IO ()
fill db = do
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("dejvice" :: T.Text, 2 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("bubenec" :: T.Text, 2 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("podbaba" :: T.Text, 1 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("stromovka" :: T.Text, 1 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("metronom" :: T.Text, 1 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("holejsovice" :: T.Text, 2 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("evrS" :: T.Text, 2 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("hradcany" :: T.Text, 2 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("vitezne" :: T.Text, 3 :: Int)
    execute db "INSERT INTO regions (region_name, level) VALUES (?, ?)" ("center" :: T.Text, 3 :: Int)
    Storage.Coords.coords db

clear :: Connection -> IO ()
clear db = do
            execute_ db "DELETE FROM points"
            execute_ db "DELETE FROM regions"

selectRegions :: Connection -> IO [Region]
selectRegions db = query_ db 
  "SELECT r.region_name, r.level, group_concat(p.lng || ';' || p.lat) \
  \FROM regions r \
  \LEFT JOIN points p ON p.region_name = r.region_name \
  \GROUP BY r.region_name, r.level \
  \ORDER BY p.order_number ASC"
