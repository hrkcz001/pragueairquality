module Region exposing (..)

import Browser
import Http
import Html exposing (div, text, Html)
import Html.Attributes exposing (style)
import Html.Events 
import Json.Decode
import Json.Encode
import Styles.Attributes
import LngLat exposing (LngLat)
import Mapbox.Cmd.Option as Opt
import Mapbox.Element exposing (..)
import Mapbox.Expression as E exposing (false, float, int, str, true)
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Mapbox.Style as Style exposing (Style(..))
import Mapbox.Source as Source
import Mapbox.Expression exposing (Color)

import Styles.Streets exposing (styleLayers)
import Requests

type alias Region =
    { name : String
    , level : Int
    , polygon : List LngLat
    }

type alias RegionInfo =
    { name : String
    , status : String
    , description : String
    }

sourcesFromRegions : List Region -> List Source.Source
sourcesFromRegions regions =
    let polygonClose region = case region.polygon of
            [] -> []
            (head :: xs) -> (head :: xs) ++ [ head ]
        coords region =
            region
                |> polygonClose
                |> List.map (\{ lng, lat } -> "[" ++ (String.fromFloat lng) ++ ", " ++ (String.fromFloat lat) ++ "]")
                |> String.join ", "
        regionToSource region = Json.Decode.decodeString Json.Decode.value ("""
            {
              "type": "FeatureCollection",
              "features": [
                {
                  "type": "Feature",
                  "properties": {
                    "name": \"""" ++ region.name ++ """\"
                  },
                  "geometry": {
                    "type": "Polygon",
                    "coordinates": [
                      [ """ ++ (coords region) ++ """
                      ]
                    ]
                  }
                }
              ]
            }
            """) |> Result.withDefault (Json.Encode.object [])
    in
    regions
    |> List.map (\region -> Source.geoJSONFromValue region.name [Source.generateIds] (regionToSource region))

colorFromLevel : Int -> E.Expression msg Color
colorFromLevel level = case level of
    1 -> E.rgba 0 255 0 1
    2 -> E.rgba 255 255 0 1
    _ -> E.rgba 255 0 0 1

viewRegionInfo : msg -> Maybe RegionInfo -> Html msg
viewRegionInfo mapMsg regionInfo = 
    case regionInfo of
                    Just info -> div Styles.Attributes.regionInfo
                                 [ Html.h2 [] [ text info.name ]
                                 , Html.h3 [] [ text info.status ]
                                 , Html.p [] [ text info.description ]
                                 , Html.button (Styles.Attributes.closeButton ++
                                     [ Html.Events.onClick mapMsg ]) [text "X"]
                                 ]
                    Nothing ->  Html.div [] []

layersFromRegions : List Region -> List Layer.Layer
layersFromRegions regions = 
    List.map (\region -> 
        Layer.fill region.name
        region.name
        [   Layer.fillColor (colorFromLevel region.level)
        ,   Layer.fillOutlineColor (E.rgba 0 0 0 255)
        ,   Layer.fillOpacity 
            (E.ifElse (E.toBool (E.featureState (str "hover"))) 
                (float 0.6) 
                (float 0.3)
            )
        ]
    ) regions

listenLayersFromRegions : List Region -> List String
listenLayersFromRegions regions = 
    List.map (\region -> region.name) regions
