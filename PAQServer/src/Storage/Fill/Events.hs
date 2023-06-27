module Storage.Fill.Events (fill) where

import Database.SQLite.Simple (Connection, execute)
import qualified Data.Text as T

fill :: Connection -> IO ()
fill db = do
    execute db "INSERT INTO events (lng, lat, creator, description) VALUES (?, ?, ?, ?)" (14.399780403620639 :: Float, 50.09204475707426 :: Float, "John Smith" :: T.Text, "Ecology Event in Prague" :: T.Text)
    execute db "INSERT INTO events (lng, lat, creator, description) VALUES (?, ?, ?, ?)" (14.384674202443051 :: Float, 50.097771196243315 :: Float, "Emma Johnson" :: T.Text, "Green Initiative Workshop" :: T.Text)
    execute db "INSERT INTO events (lng, lat, creator, description) VALUES (?, ?, ?, ?)" (14.402698647018212 :: Float, 50.10360705501657 :: Float, "David Novák" :: T.Text, "Sustainable Living Exhibition" :: T.Text)
    execute db "INSERT INTO events (lng, lat, creator, description) VALUES (?, ?, ?, ?)" (14.426044594287845 :: Float, 50.09832177933046 :: Float, "Eva Nováková" :: T.Text, "Environmental Cleanup Drive" :: T.Text)
    execute db "INSERT INTO events (lng, lat, creator, description) VALUES (?, ?, ?, ?)" (14.448875557421132 :: Float, 50.10757062912049 :: Float, "Martin Dvořák" :: T.Text, "Nature Conservation Conference" :: T.Text)
