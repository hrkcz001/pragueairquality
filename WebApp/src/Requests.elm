module Requests exposing (getAbout, getEventInfo, getEvents, getRegionInfo, getRegions, postEvent)

import Http
import Json.Decode
import Json.Encode
import LngLat exposing (LngLat)


getRegions : (Result Http.Error (List { name : String, level : Int, polygon : List LngLat }) -> msg) -> Cmd msg
getRegions msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = "api/regions"
        , body = Http.emptyBody
        , expect = Http.expectJson msg (Json.Decode.list regionDecoder)
        , timeout = Just 10000
        , tracker = Nothing
        }



--| id is String because I use it as name for features in mapbox


getEvents : (Result Http.Error (List { id : String, lng : Float, lat : Float }) -> msg) -> Cmd msg
getEvents msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = "api/events"
        , body = Http.emptyBody
        , expect = Http.expectJson msg (Json.Decode.list eventDecoder)
        , timeout = Just 10000
        , tracker = Nothing
        }


eventDecoder : Json.Decode.Decoder { id : String, lng : Float, lat : Float }
eventDecoder =
    Json.Decode.map3 (\id lng lat -> { id = String.fromInt id, lng = lng, lat = lat })
        (Json.Decode.field "event_id" Json.Decode.int)
        (Json.Decode.field "event_lng" Json.Decode.float)
        (Json.Decode.field "event_lat" Json.Decode.float)


regionDecoder : Json.Decode.Decoder { name : String, level : Int, polygon : List LngLat }
regionDecoder =
    Json.Decode.map3 (\name level polygon -> { name = name, level = level, polygon = polygon })
        (Json.Decode.field "region_name" Json.Decode.string)
        (Json.Decode.field "region_level" Json.Decode.int)
        (Json.Decode.field "region_polygon" (Json.Decode.list lngLatDecoder))


lngLatDecoder : Json.Decode.Decoder LngLat
lngLatDecoder =
    Json.Decode.map2 LngLat
        (Json.Decode.field "point_lng" Json.Decode.float)
        (Json.Decode.field "point_lat" Json.Decode.float)


getRegionInfo : String -> (Result Http.Error { name : String, status : String, description : String } -> msg) -> Cmd msg
getRegionInfo region msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = "api/regions/" ++ region
        , body = Http.emptyBody
        , expect = Http.expectJson msg regionInfoDecoder
        , timeout = Just 10000
        , tracker = Nothing
        }


regionInfoDecoder : Json.Decode.Decoder { name : String, status : String, description : String }
regionInfoDecoder =
    Json.Decode.map3 (\name status description -> { name = name, status = status, description = description })
        (Json.Decode.field "region_complete_name" Json.Decode.string)
        (Json.Decode.field "region_status" Json.Decode.string)
        (Json.Decode.field "region_description" Json.Decode.string)


getEventInfo : String -> (Result Http.Error { creator : String, description : String } -> msg) -> Cmd msg
getEventInfo event msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = "api/events/" ++ event
        , body = Http.emptyBody
        , expect = Http.expectJson msg eventInfoDecoder
        , timeout = Just 10000
        , tracker = Nothing
        }


eventInfoDecoder : Json.Decode.Decoder { creator : String, description : String }
eventInfoDecoder =
    Json.Decode.map2 (\creator description -> { creator = creator, description = description })
        (Json.Decode.field "event_creator" Json.Decode.string)
        (Json.Decode.field "event_description" Json.Decode.string)


getAbout : (Result Http.Error String -> msg) -> Cmd msg
getAbout msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = "static/about.txt"
        , body = Http.emptyBody
        , expect = Http.expectString msg
        , timeout = Just 10000
        , tracker = Nothing
        }


postEvent : LngLat -> String -> String -> (Result Http.Error (List { id : String, lng : Float, lat : Float }) -> msg) -> Cmd msg
postEvent lngLat creator description msg =
    Http.request
        { method = "POST"
        , headers = []
        , url = "api/events"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "insert_event_lng", Json.Encode.float lngLat.lng )
                    , ( "insert_event_lat", Json.Encode.float lngLat.lat )
                    , ( "insert_event_creator", Json.Encode.string creator )
                    , ( "insert_event_description", Json.Encode.string description )
                    ]
                )
        , expect = Http.expectJson msg (Json.Decode.list eventDecoder)
        , timeout = Just 10000
        , tracker = Nothing
        }
