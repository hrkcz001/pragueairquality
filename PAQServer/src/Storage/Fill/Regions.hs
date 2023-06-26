module Storage.Fill.Regions (fill) where

import Database.SQLite.Simple (Connection, execute)
import qualified Data.Text as T

fill :: Connection -> IO ()
fill db = do
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("dejvice" :: T.Text, 2 :: Int, "Dejvice Complete Name" :: T.Text, "2A" :: T.Text, "Dejvice Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("bubenec" :: T.Text, 2 :: Int, "Bubenec Complete Name" :: T.Text, "2B" :: T.Text, "Bubenec Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("podbaba" :: T.Text, 1 :: Int, "Podbaba Complete Name" :: T.Text, "1A" :: T.Text, "Podbaba Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("stromovka" :: T.Text, 1 :: Int, "Stromovka Complete Name" :: T.Text, "1B" :: T.Text, "Stromovka Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("metronom" :: T.Text, 1 :: Int, "Metronom Complete Name" :: T.Text, "1C" :: T.Text, "Metronom Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("holejsovice" :: T.Text, 2 :: Int, "Holejsovice Complete Name" :: T.Text, "2C" :: T.Text, "Holejsovice Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("evrS" :: T.Text, 2 :: Int, "EvrS Complete Name" :: T.Text, "2A" :: T.Text, "EvrS Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("hradcany" :: T.Text, 2 :: Int, "Hradcany Complete Name" :: T.Text, "2B" :: T.Text, "Hradcany Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("vitezne" :: T.Text, 3 :: Int, "Vitezne Complete Name" :: T.Text, "3A" :: T.Text, "Vitezne Description" :: T.Text)
    execute db "INSERT INTO regions (region_name, level, complete_name, status, description) VALUES (?, ?, ?, ?, ?)" ("center" :: T.Text, 3 :: Int, "Center Complete Name" :: T.Text, "3B" :: T.Text, "Center Description" :: T.Text)
