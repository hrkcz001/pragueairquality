module Main (main) where

import Server

main :: IO ()
main = Server.initHttpServer
