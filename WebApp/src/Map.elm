module Map exposing (Model, Msg(..), Mode(..), init, update, view)

import Http
import Html exposing (div, text, Html)
import Html.Attributes
import Html.Events 
import Json.Decode
import Json.Encode
import Styles.Attributes
import LngLat exposing (LngLat)
import Mapbox.Element exposing (..)
import Mapbox.Source as Source
import Mapbox.Style as Style exposing (Style(..))
import Mapbox.Source as Source

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
    , insertMode : Bool
    , inserting : Maybe LngLat
    , insertedDetails : Maybe String
    , about : String
    }

type Mode = Regions
          | Events
          | About

type Msg = Hover EventData
         | Click EventData
         | GotRegions (Result Http.Error (List Region))
         | GotRegion (Result Http.Error RegionInfo)
         | CloseRegionInfo
         | GotEvents (Result Http.Error (List Event))
         | GotEvent (Result Http.Error EventInfo)
         | CloseEventInfo
         | InsertMode
         | CancelInsertMode
         | Inserting LngLat
         | InsertedDetails String
         | SubmitInsert
         | GotAbout (Result Http.Error String)

init : ( Msg -> msg ) -> ( Model, Cmd msg )
init wrapMsg =
    ({ features = [], regions = [], selectedRegion = Nothing, events = [], selectedEvent = Nothing, insertMode = False, 
    inserting = Nothing, insertedDetails = Nothing, about = "" }, Cmd.batch [ Requests.getRegions (wrapMsg << GotRegions)
                                                                         , Requests.getEvents (wrapMsg << GotEvents)
                                                                         , Requests.getAbout (wrapMsg << GotAbout)
                                                                         ])

featureName : Json.Decode.Decoder String
featureName =
    Json.Decode.at [ "properties", "name" ] Json.Decode.string

update : ( Msg -> msg ) -> Msg -> Model -> ( Model, Cmd msg )
update wrapMsg msg model =
    case msg of
        Hover { renderedFeatures } ->
            ( { model | features = renderedFeatures }, Cmd.none )

        Click { lngLat, renderedFeatures } ->
            let feature = renderedFeatures
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
            if model.insertMode
                then case model.inserting of
                        Just _ -> ( model, Cmd.none )
                        Nothing -> ( { model | inserting = Just lngLat }, Cmd.none )
                else ( model, cmd )
                
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
        InsertMode ->
            ( { model | insertMode = True }, Cmd.none )
        CancelInsertMode ->
            ( { model | insertMode = False, inserting = Nothing, insertedDetails = Nothing }, Cmd.none )
        Inserting lngLat ->
            ( { model | inserting = Just lngLat }, Cmd.none )
        InsertedDetails details ->
            ( { model | insertedDetails = Just details }, Cmd.none )
        SubmitInsert ->
            case ( model.inserting , model.insertedDetails ) of
                ( Just point, Just details ) -> ( { model | insertMode = False, inserting = Nothing, insertedDetails = Nothing }, Requests.postEvent point "Anonymous" details (wrapMsg << GotEvents))        
                _ -> ( model, Cmd.none )
        GotAbout (Ok about) ->
            ( { model | about = about }, Cmd.none )
        GotAbout (Err _) ->
            ( model, Cmd.none )

hoveredFeatures : List Json.Encode.Value -> MapboxAttr msg
hoveredFeatures =
    List.map (\feat -> ( feat, [ ( "hover", Json.Encode.bool True ) ] ))
        >> featureState

view : Mode -> Model -> Html Msg
view mode model =
    let content = case mode of
            Regions -> viewRegionInfo CloseRegionInfo model.selectedRegion
            Events -> viewEventInfo CloseEventInfo model.selectedEvent
            About -> Html.div Styles.Attributes.about
                         [ Html.p [] [ Html.text model.about ]
                         ]

        insertButton = if model.insertMode
                        then Html.button (Styles.Attributes.insertButton
                                ++ [Html.Events.onClick CancelInsertMode])
                                [ text "Cancel" ]
                        else Html.button (Styles.Attributes.insertButton
                                ++ [Html.Events.onClick InsertMode])
                                [ text "Add Event" ]

        floating = if model.insertMode
            then case model.inserting of
                Just _ -> [div Styles.Attributes.eventInfo
                    [ Html.h2 [] [ text "Anonymous" ] 
                    , Html.textarea (Styles.Attributes.inputForm
                                     ++ [Html.Attributes.placeholder "Details"
                                        , Html.Events.onInput InsertedDetails
                                        ])
                                     []
                    , Html.button (Styles.Attributes.closeButton
                                      ++ [Html.Events.onClick CancelInsertMode])
                                      [ text "X" ]
                    , Html.button (Styles.Attributes.insertingSubmit
                                      ++ [ Html.Attributes.disabled <|
                                            model.insertedDetails == Nothing ||
                                            model.insertedDetails == Just ""
                                         , Html.Events.onClick SubmitInsert])
                                      [ text "Submit" ]
                    ]]
                Nothing -> [ insertButton
                           , content
                           ]
            else    [ insertButton
                    , content
                    ]
    in
    div []
        (viewMap mode model
        :: floating)

viewMap : Mode -> Model -> Html Msg
viewMap mode model = 
    let modeLayers = case mode of
            Regions -> layersFromRegions model.regions
            Events -> layersFromEvents model.events
            _ -> []
        modeListenLayers = case mode of
            Regions -> listenLayersFromRegions model.regions
            Events -> listenLayersFromEvents model.events
            _ -> []
        modeSources = case mode of
            Regions -> sourcesFromRegions model.regions
            Events -> sourcesFromEvents model.events
            _ -> []
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
