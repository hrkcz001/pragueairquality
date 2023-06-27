module Region exposing (..)

import Html exposing (Html)
import Html.Events
import Json.Decode
import Json.Encode
import LngLat exposing (LngLat)
import Mapbox.Expression as E
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Styles.Attributes



--| A region is a polygon with a name and a level (Color)


type alias Region =
    { name : String
    , level : Int
    , polygon : List LngLat
    }



--| Region info is the information that is displayed when a region is clicked


type alias RegionInfo =
    { name : String
    , status : String
    , description : String
    }



--| Create a list of sources collections from a list of regions
--| I use separate features collections for each regions
--| because I want to be able to change the color of each region
--| Could be done another way, but I don't know how
--| (or at least could be minimized to separate featues collections for each level)


sourcesFromRegions : List Region -> List Source.Source
sourcesFromRegions regions =
    let
        polygonClose region =
            case region.polygon of
                [] ->
                    []

                head :: xs ->
                    (head :: xs) ++ [ head ]

        coords region =
            region
                |> polygonClose
                |> List.map (\{ lng, lat } -> "[" ++ String.fromFloat lng ++ ", " ++ String.fromFloat lat ++ "]")
                |> String.join ", "

        regionToSource region =
            Json.Decode.decodeString Json.Decode.value ("""
            {
              "type": "FeatureCollection",
              "features": [
                {
                  "type": "Feature",
                  "properties": {
                    "name": "region.""" ++ region.name ++ """"
                  },
                  "geometry": {
                    "type": "Polygon",
                    "coordinates": [
                      [ """ ++ coords region ++ """
                      ]
                    ]
                  }
                }
              ]
            }
            """) |> Result.withDefault (Json.Encode.object [])
    in
    regions
        |> List.map (\region -> Source.geoJSONFromValue region.name [ Source.generateIds ] (regionToSource region))


colorFromLevel : Int -> E.Expression msg E.Color
colorFromLevel level =
    case level of
        1 ->
            E.rgba 0 255 0 1

        2 ->
            E.rgba 255 255 0 1

        _ ->
            E.rgba 255 0 0 1



--| Create a list of layers from a list of regions


layersFromRegions : List Region -> List Layer.Layer
layersFromRegions regions =
    List.map
        (\region ->
            Layer.fill ("region." ++ region.name)
                region.name
                [ Layer.fillColor (colorFromLevel region.level)
                , Layer.fillOutlineColor (E.rgba 0 0 0 255)
                , Layer.fillOpacity
                    (E.ifElse (E.toBool (E.featureState (E.str "hover")))
                        (E.float 0.6)
                        (E.float 0.3)
                    )
                ]
        )
        regions



--| Create a list of names of layers from a list of region
--| Used to listen to events on regions


listenLayersFromRegions : List Region -> List String
listenLayersFromRegions regions =
    List.map (\region -> "region." ++ region.name) regions



--| View a region info


viewRegionInfo : msg -> Maybe RegionInfo -> Html msg
viewRegionInfo mapMsg regionInfo =
    case regionInfo of
        Just info ->
            Html.div Styles.Attributes.regionInfo
                [ Html.h2 [] [ Html.text info.name ]
                , Html.h3 [] [ Html.text info.status ]
                , Html.p [] [ Html.text info.description ]
                , Html.button
                    (Styles.Attributes.closeButton
                        ++ [ Html.Events.onClick mapMsg ]
                    )
                    [ Html.text "X" ]
                ]

        Nothing ->
            Html.div [] []
