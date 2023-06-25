module Map exposing (main)

import Styles.Streets exposing (styleLayers)

import Browser
import Html exposing (div, text, Html)
import Html.Attributes exposing (style)
import Json.Decode
import Json.Encode
import LngLat exposing (LngLat)
import Mapbox.Cmd.Option as Opt
import Mapbox.Element exposing (..)
import Mapbox.Expression as E exposing (false, float, int, str, true)
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Mapbox.Style as Style exposing (Style(..))
import Mapbox.Source as Source
import Mapbox.Expression exposing (Color)

type alias Model =
    { position : LngLat
    , regions : List Region
    , features : List Json.Encode.Value
    }

type alias Region =
    { name : String
    , level : Int
    , polygon : List LngLat
    }

type Msg = Hover EventData
         | Click EventData

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { regions = [
            Region "bermud" 3 [ LngLat -64.73 32.31
            , LngLat -80.19 25.76
            , LngLat -66.09 18.43
            , LngLat -64.73 32.31
            ]
        ], position = LngLat 0 0, features = [] }, Cmd.none )

featureName : Json.Decode.Decoder String
featureName =
    Json.Decode.at [ "properties", "name" ] Json.Decode.string

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hover { lngLat, renderedFeatures } ->
            ( { model | position = lngLat, features = renderedFeatures }, Cmd.none )

        Click { lngLat, renderedFeatures } ->
            let _ = renderedFeatures
                    |> List.head
                    |> Maybe.withDefault Json.Encode.null
                    |> Json.Decode.decodeValue featureName
                    |> Result.withDefault ""
                    |> Debug.log "clicked"
            in
            ( model, Cmd.none )

regionsToSources : List Region -> List Source.Source
regionsToSources regions =
    let coords region =
            region.polygon
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

hoveredFeatures : List Json.Encode.Value -> MapboxAttr msg
hoveredFeatures =
    List.map (\feat -> ( feat, [ ( "hover", Json.Encode.bool True ) ] ))
        >> featureState

colorFromLevel : Int -> E.Expression msg Color
colorFromLevel level = case level of
    1 -> E.rgba 255 0 0 1
    2 -> E.rgba 255 255 0 1
    _ -> E.rgba 0 255 0 1

view : Model -> Html Msg
view model = div [ Html.Attributes.style "height" "100vh" ]
            [map
                [ onMouseMove Hover
                , onClick Click
                , id "paq-map"
                , eventFeaturesLayers ( List.map (\region -> region.name) model.regions )
                , hoveredFeatures model.features
                ]
                (Style
                    { transition = Style.defaultTransition
                    , light = Style.defaultLight
                    , sources = Source.vectorFromUrl "composite" "mapbox://mapbox.mapbox-terrain-v2,mapbox.mapbox-streets-v7"
                                :: regionsToSources model.regions
                    , misc =
                        [ Style.defaultCenter <| LngLat 20.39789404164037 43.22523201923144
                        , Style.defaultZoomLevel 1.5967483759772743
                        , Style.sprite "mapbox://sprites/mapbox/streets-v9"
                        , Style.glyphs "mapbox://fonts/mapbox/{fontstack}/{range}.pbf"
                        ]
                        , layers = styleLayers
                                    ++
                                    (List.map (\region -> 
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
                                    ) model.regions)
                    }
                )
            ]
