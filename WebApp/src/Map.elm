module Map exposing (Model, Msg(..), Mode(..), init, update, view)

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
import Region exposing (..)
import Event exposing (..)

type alias Model =
    { features : List Json.Encode.Value
    , regions : List Region
    , selectedRegion : Maybe RegionInfo
    , events : List Event
    , selectedEvent : Maybe EventInfo
    }

type Mode = Regions
          | Clear
          | Events

type Msg = Hover EventData
         | Click EventData
         | GotRegions (Result Http.Error (List Region))
         | GotRegion (Result Http.Error RegionInfo)
         | CloseRegionInfo
         | GotEvents (Result Http.Error (List Event))
         | GotEvent (Result Http.Error EventInfo)
         | CloseEventInfo

init : ( Msg -> msg ) -> ( Model, Cmd msg )
init wrapMsg =
    ({ features = [], regions = [], selectedRegion = Nothing, events = [], selectedEvent = Nothing }, Cmd.batch [ Requests.getRegions (wrapMsg << GotRegions)
                                                                         , Requests.getEvents (wrapMsg << GotEvents)
                                                                         ])

featureName : Json.Decode.Decoder String
featureName =
    Json.Decode.at [ "properties", "name" ] Json.Decode.string

update : ( Msg -> msg ) -> Msg -> Model -> ( Model, Cmd msg )
update wrapMsg msg model =
    case msg of
        Hover { lngLat, renderedFeatures } ->
            ( { model | features = renderedFeatures }, Cmd.none )

        Click { lngLat, renderedFeatures } ->
            let _ = Debug.log "pos" ("lng: " ++ (String.fromFloat lngLat.lng) ++ ", lat: " ++ (String.fromFloat lngLat.lat))
                feature = renderedFeatures
                    |> List.head
                    |> Maybe.withDefault Json.Encode.null
                    |> Json.Decode.decodeValue featureName
                    |> Result.withDefault ""
                    |> String.split "."

                ( featType, featName ) = case feature of
                    [a, b] -> (a, b)
                    _ -> ("", "")
                cmd = case featType of
                        "region" -> case featName of
                                "" -> Cmd.none
                                _ -> Requests.getRegionInfo featName (wrapMsg << GotRegion)
                        "event" -> case featName of
                                "" -> Cmd.none
                                _ -> Requests.getEventInfo featName (wrapMsg << GotEvent)
                        _ -> Cmd.none
            in
            ( model, cmd )
                
        GotRegions (Ok regions) ->
            ( { model | regions = regions }, Cmd.none )
        GotRegions (Err _) ->
            ( model, Cmd.none )
        GotRegion (Ok regionInfo) ->
            ( { model | selectedRegion = Just regionInfo }, Cmd.none )
        GotRegion (Err _) ->
            ( { model | selectedRegion = Nothing }, Cmd.none )
        CloseRegionInfo ->
            ( { model | selectedRegion = Nothing }, Cmd.none )
        GotEvents (Ok events) ->
            ( { model | events = events }, Cmd.none )
        GotEvents (Err _) ->
            ( model, Cmd.none )
        GotEvent (Ok eventInfo) ->
            ( { model | selectedEvent = Just eventInfo }, Cmd.none )
        GotEvent (Err _) ->
            ( { model | selectedEvent = Nothing }, Cmd.none )
        CloseEventInfo ->
            ( { model | selectedEvent = Nothing }, Cmd.none )

hoveredFeatures : List Json.Encode.Value -> MapboxAttr msg
hoveredFeatures =
    List.map (\feat -> ( feat, [ ( "hover", Json.Encode.bool True ) ] ))
        >> featureState

view : Mode -> Model -> Html Msg
view mode model =
    let content = case mode of
            Regions -> viewRegionInfo CloseRegionInfo model.selectedRegion
            Events -> viewEventInfo CloseEventInfo model.selectedEvent
            _ -> div [] []
    in
    div []
        [ viewMap mode model
        , content
        ]

viewMap : Mode -> Model -> Html Msg
viewMap mode model = 
    let modeLayers = case mode of
            Regions -> layersFromRegions model.regions
            Events -> layersFromEvents model.events
            Clear -> []
        modeListenLayers = case mode of
            Regions -> listenLayersFromRegions model.regions
            Events -> listenLayersFromEvents model.events
            Clear -> []
        modeSources = case mode of
            Regions -> sourcesFromRegions model.regions
            Events -> sourcesFromEvents model.events
            Clear -> []
    in
    div Styles.Attributes.map
            [map
                [ maxZoom 18
                , minZoom 10
                , maxBounds (   LngLat 14.098789849977067 49.932573881803535
                            ,   LngLat 14.750530939532837 50.2500770495798)
                , onMouseMove Hover
                , onClick Click
                , id "paq-map"
                , eventFeaturesLayers modeListenLayers
                , hoveredFeatures model.features
                ]
                (Style
                    { transition = Style.defaultTransition
                    , light = Style.defaultLight
                    , sources = Source.vectorFromUrl "composite" "mapbox://mapbox.mapbox-terrain-v2,mapbox.mapbox-streets-v7"
                                :: modeSources
                    , misc =
                        [ Style.defaultCenter <| LngLat 14.417941209392495 50.10093189854709
                        , Style.defaultZoomLevel 13
                        , Style.sprite "mapbox://sprites/mapbox/streets-v9"
                        , Style.glyphs "mapbox://fonts/mapbox/{fontstack}/{range}.pbf"
                        ]
                        , layers = styleLayers
                                    ++ modeLayers
                    }
                )
            ]
