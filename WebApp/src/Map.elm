module Map exposing (Model, Msg(..), init, update, view)

import Styles.Streets exposing (styleLayers)

import Browser
import Http
import Html exposing (div, text, Html)
import Html.Attributes exposing (style)
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

type alias Model =
    { regions : List Region
    , features : List Json.Encode.Value
    }

type alias Region =
    { name : String
    , level : Int
    , polygon : List LngLat
    }

type Msg = Hover EventData
         | Click EventData
         | GotRegions (Result Http.Error (List Region))

getRegions : Cmd Msg
getRegions =
    Http.request
        { method = "GET"
        , headers = []
        , url = "regions"
        , body = Http.emptyBody
        , expect = Http.expectJson (GotRegions) (Json.Decode.list regionDecoder)
        , timeout = Just 10000
        , tracker = Nothing
        }

regionDecoder : Json.Decode.Decoder Region
regionDecoder =
    Json.Decode.map3 Region
        (Json.Decode.at [ "name" ] Json.Decode.string)
        (Json.Decode.at [ "level" ] Json.Decode.int)
        (Json.Decode.at [ "polygon" ] (Json.Decode.list lngLatDecoder))

lngLatDecoder : Json.Decode.Decoder LngLat
lngLatDecoder =
    Json.Decode.map2 LngLat
        (Json.Decode.field "lng" Json.Decode.float)
        (Json.Decode.field "lat" Json.Decode.float)

init : ( Model, Cmd Msg )
init =
    ({ regions = [], features = [] }, getRegions )

featureName : Json.Decode.Decoder String
featureName =
    Json.Decode.at [ "properties", "name" ] Json.Decode.string

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hover { lngLat, renderedFeatures } ->
            ( { model | features = renderedFeatures }, Cmd.none )

        Click { lngLat, renderedFeatures } ->
            let {-_ = renderedFeatures
                    |> List.head
                    |> Maybe.withDefault Json.Encode.null
                    |> Json.Decode.decodeValue featureName
                    |> Result.withDefault ""
                    |> Debug.log "clicked"-}
                _ = Debug.log "pos" ("lng: " ++ (String.fromFloat lngLat.lng) ++ ", lat: " ++ (String.fromFloat lngLat.lat))
            in
            ( model, Cmd.none )
        GotRegions (Ok regions) ->
            ( { model | regions = regions }, Cmd.none )
        GotRegions (Err _) ->
            ( model, Cmd.none )

regionsToSources : List Region -> List Source.Source
regionsToSources regions =
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

hoveredFeatures : List Json.Encode.Value -> MapboxAttr msg
hoveredFeatures =
    List.map (\feat -> ( feat, [ ( "hover", Json.Encode.bool True ) ] ))
        >> featureState

colorFromLevel : Int -> E.Expression msg Color
colorFromLevel level = case level of
    1 -> E.rgba 0 255 0 1
    2 -> E.rgba 255 255 0 1
    _ -> E.rgba 255 0 0 1

view : Model -> Html Msg
view model = div Styles.Attributes.map
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
                        [ Style.defaultCenter <| LngLat 14.414220904827602 50.09176980049028
                        , Style.defaultZoomLevel 13
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
