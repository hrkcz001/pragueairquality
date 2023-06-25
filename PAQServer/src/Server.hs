
{-# LANGUAGE OverloadedStrings #-}
module Server (initHttpServer) where

import Web.Scotty
import Storage

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad
import Control.Monad.Trans
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)

import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (fromString)
import Prelude ()
import Prelude.Compat

initHttpServer :: IO ()
initHttpServer = do
    db <- initDb
    startHandling db

startHandling :: Connection -> IO ()
startHandling db = scotty 3000 $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    get "/" $ do
        setHeader "Content-Type" "text/html"
        file "../WebApp/index.html"

    -- Send Elm app
    get "/main" $ do
        setHeader "Content-Type" "application/javascript"
        file "../WebApp/main.js"
    get "/starter" $ do
        setHeader "Content-Type" "application/javascript"
        file "../WebApp/starter.js"
    get "/elm-mapbox" $ do
        setHeader "Content-Type" "application/javascript"
        file "../WebApp/node_modules/elm-mapbox/dist/elm-mapbox.umd.js"

    get "/regions" $ do
        json $ regionsToJson $ Storage.selectRegions db
