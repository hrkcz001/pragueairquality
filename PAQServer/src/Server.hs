{-# LANGUAGE OverloadedStrings #-}
module Server (initHttpServer) where

import Web.Scotty
import Storage.Control
import Storage.Types

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad
import Control.Monad.Trans
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)

import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (Text, pack)
import Data.String (fromString)
import Prelude
import Prelude.Compat


initHttpServer :: IO ()
initHttpServer = do
    db <- initDb
    startHandling db

startHandling :: Connection -> IO ()
startHandling db = scotty 3000 $ do
    -- console logging middleware
    middleware logStdoutDev

    -- top level redirects to map
    get "/" $ do
        redirect "/map"

    -- send index.html and preserve url if page were requested
    get "/:page" $ do
        page <- param "page" :: ActionM Text
        if page `elem` ["map", "test"]
            then do
                    setHeader "Content-Type" "text/html"
                    file "../WebApp/dist/index.html"
            else next
    
    -- send main.js
    get "/static/main.js" $ do
        setHeader "Content-Type" "application/javascript"
        file "../WebApp/dist/main.js"

    -- send regions {name : text, level : int, polygon : list of {lat : float, lon : float}}
    get "/regions" $ do
        reg <- liftIO $ Storage.Control.selectRegions db
        json reg

    -- clear database
    get "/clear" $ do
        _ <- liftIO $ Storage.Control.clear db
        redirect "/"

    -- clear and fill database with predefined data
    get "/refill" $ do
        _ <- liftIO $ Storage.Control.clear db
        _ <- liftIO $ Storage.Control.fill db
        redirect "/"
