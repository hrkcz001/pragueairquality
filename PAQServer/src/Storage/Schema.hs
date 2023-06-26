module Storage.Schema (form, recreate) where

import Database.SQLite.Simple (Connection, execute_)

-- | Create tables if they don't exist
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
                        \complete_name TEXT NOT NULL, \
                        \status TEXT NOT NULL, \
                        \description TEXT NOT NULL, \
                        \level INTEGER NOT NULL CHECK (level >= 0))"

-- | Drop tables and create them again
recreate :: Connection -> IO ()
recreate db = do
                execute_ db "DROP TABLE IF EXISTS points"
                execute_ db "DROP TABLE IF EXISTS regions"
                form db
