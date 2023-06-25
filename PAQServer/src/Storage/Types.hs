{-# LANGUAGE DeriveGeneric #-}
module Storage.Types (
    Region(..)
) where

import GHC.Generics

type Point = (Double, Double)

data Region = Region {
    name :: String,
    level :: Int,
    polygon :: [Point]
} deriving (Show, Generic)
