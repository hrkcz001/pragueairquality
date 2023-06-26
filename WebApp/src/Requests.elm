module Requests exposing (getRegions, getRegionInfo)

import Http
import Json.Decode
import LngLat exposing (LngLat)

getRegions : (Result Http.Error (List { name : String, level : Int, polygon : List LngLat }) -> msg) -> Cmd msg
getRegions msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = "regions"
        , body = Http.emptyBody
        , expect = Http.expectJson msg (Json.Decode.list regionDecoder)
        , timeout = Just 10000
        , tracker = Nothing
        }

regionDecoder : Json.Decode.Decoder ({ name : String, level : Int, polygon : List LngLat })
regionDecoder =
    Json.Decode.map3 (\name level polygon -> { name = name, level = level, polygon = polygon })
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "level" Json.Decode.int)
        (Json.Decode.field "polygon" (Json.Decode.list lngLatDecoder))

lngLatDecoder : Json.Decode.Decoder LngLat
lngLatDecoder =
    Json.Decode.map2 LngLat
        (Json.Decode.field "lng" Json.Decode.float)
        (Json.Decode.field "lat" Json.Decode.float)

getRegionInfo : String -> (Result Http.Error { name : String, status : String, description : String } -> msg) -> Cmd msg
getRegionInfo region msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = "regions?name=" ++ region
        , body = Http.emptyBody
        , expect = Http.expectJson msg regionInfoDecoder
        , timeout = Just 10000
        , tracker = Nothing
        }

regionInfoDecoder : Json.Decode.Decoder { name : String, status : String, description : String }
regionInfoDecoder =
    Json.Decode.map3 (\name status description -> { name = name, status = status, description = description })
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "status" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
