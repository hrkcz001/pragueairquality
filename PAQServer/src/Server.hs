module Server (initHttpServer) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Data.Text (Text)

import Storage.Control
import Storage.Types (InsertEvent(..))

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
            "about.txt" -> do
                    setHeader "Content-Type" "text/plain"
                    file "./static/about.txt"
            "mapbox-gl.css" -> do
                    setHeader "Content-Type" "text/css"
                    file "./static/mapbox-gl.css"
            "mapbox-gl.js" -> do
                    setHeader "Content-Type" "application/javascript"
                    file "./static/mapbox-gl.js"
            _ -> next

    -- send regions { name : text, level : int, polygon : list of { lat : float, lon : float } }
    get "/api/regions" $ do
        regions <- liftAndCatchIO $ Storage.Control.selectRegions db
        json regions

    -- send region by name { complete_name : text, status : text, description : text }
    get "/api/regions/:name" $ do
        name <- param "name" :: ActionM Text
        region <- liftAndCatchIO $ Storage.Control.selectRegion db name
        case region of
            Nothing -> raise "Region not found"
            Just r -> json r

    -- select all events { id : int, lng : float, lat : float }
    get "/api/events" $ do
        events <- liftAndCatchIO $ Storage.Control.selectEvents db
        json events
    
    -- select event by id { creator : text, description : text }
    get "/api/events/:id" $ do
        eventId <- param "id" :: ActionM Int
        event <- liftAndCatchIO $ Storage.Control.selectEvent db eventId
        case event of
            Nothing -> raise "Event not found"
            Just e -> json e
    
    -- insert event { lng : float, lat : float, creator : text, description : text }
    post "/api/events" $ do
        event <- jsonData :: ActionM InsertEvent
        liftAndCatchIO $ Storage.Control.insertEvent db event
        events <- liftAndCatchIO $ Storage.Control.selectEvents db
        json events

    -- clear and fill database with predefined data
    -- for debug purposes only
    get "/refill" $ do
        _ <- liftAndCatchIO $ Storage.Control.refill db
        redirect "/"
