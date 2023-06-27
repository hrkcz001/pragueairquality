module Server (initHttpServer) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Control.Monad.Trans
import Data.Text (Text)

import Storage.Control

initHttpServer :: IO ()
initHttpServer = do
    db <- initDb
    startHandling db

startHandling :: Connection -> IO ()
startHandling db = scotty 3000 $ do
    -- console logging middleware
    middleware logStdoutDev

    -- top level redirects to map
    get "" $ do
        redirect "/map"    
    get "/" $ do
        redirect "/map"

    -- send index.html and preserve url if page were requested
    get "/:page" $ do
        page <- param "page" :: ActionM Text
        if page `elem` ["map", "events", "about"]
            then do
                    setHeader "Content-Type" "text/html"
                    file "../WebApp/dist/index.html"
            else next

    -- send file
    get "/static/:file" $ do
        requestedFile <- param "file" :: ActionM Text
        case requestedFile of
            "main.js" -> do
                    setHeader "Content-Type" "application/javascript"
                    file "../WebApp/dist/main.js"
            _ -> next

    -- send regions {name : text, level : int, polygon : list of {lat : float, lon : float}}
    get "/api/regions" $ do
        regions <- liftIO $ Storage.Control.selectRegions db
        json regions

    get "/api/region/:name" $ do
        name <- param "name" :: ActionM Text
        region <- liftIO $ Storage.Control.selectRegion db name
        case region of
            Nothing -> raise "Region not found"
            Just r -> json r

    get "/api/events" $ do
        events <- liftIO $ Storage.Control.selectEvents db
        json events

    get "/api/event/:id" $ do
        eventId <- param "id" :: ActionM Int
        event <- liftIO $ Storage.Control.selectEvent db eventId
        case event of
            Nothing -> raise "Event not found"
            Just e -> json e

    -- clear and fill database with predefined data
    get "/refill" $ do
        _ <- liftIO $ Storage.Control.refill db
        redirect "/"

