module Map exposing (Mode(..), Model, Msg(..), init, update, view)

import Event exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import LngLat exposing (LngLat)
import Mapbox.Element exposing (..)
import Mapbox.Source as Source
import Mapbox.Style as Style exposing (Style(..))
import Region exposing (..)
import Requests
import Styles.Attributes
import Styles.Streets exposing (styleLayers)

import InsertEvent


{-| The model of the map

  - hoveredFeatures: the features currently hovered by the mouse

  - regions: { name, geometry } to be displayed on the map

  - selectedRegion: the region currently selected

  - events: { id, geometry } to be displayed on the map

  - selectedEvent: the event currently selected

  - insertion messages to insert a new event

  - about: the about text to be displayed

-}
type alias Model =
    { hoveredFeatures : List Json.Encode.Value
    , mode : Mode
    , regions : List Region
    , selectedRegion : Maybe RegionInfo
    , events : List Event
    , selectedEvent : Maybe EventInfo
    , insertModel : Maybe InsertEvent.Model
    , about : String
    }



--| Mode that map is currently working in
--| Mode is needed to change layers and corrsponding sources on the map without reloading it


type Mode
    = Loading
    | Regions
    | Events
    | About


type Msg
    = Hover EventData
    | Click EventData
    | SetMode Mode
    | GotRegions (Result Http.Error (List Region))
    | GotRegion (Result Http.Error RegionInfo)
    | RegionInfoClosed
    | GotEvents (Result Http.Error (List Event))
    | GotEvent (Result Http.Error EventInfo)
    | EventInfoClosed
    | InsertMode
    | InsertEventMsg InsertEvent.Msg
    | GotAbout (Result Http.Error String)



--| Initialize the map
--| Get regions, events and about text from the server


init : Model
init =
    { hoveredFeatures = []
    , mode = Loading
    , regions = []
    , selectedRegion = Nothing
    , events = []
    , selectedEvent = Nothing
    , insertModel = Nothing
    , about = "Loading..."
    }
    



--| get a name of a mapbox feature


featureName : Json.Decode.Decoder String
featureName =
    Json.Decode.at [ "properties", "name" ] Json.Decode.string


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update wrapMsg msg model =
    case msg of
        Hover { renderedFeatures } ->
            ( { model | hoveredFeatures = renderedFeatures }, Cmd.none )

        --| If the map is in insert mode, clicking on the map will start inserting a new event
        --| If the map is not in insert mode, clicking on the map will get the feature name and
        --| request the corresponding info from the server

        Click { lngLat, renderedFeatures } ->
            let
                feature =
                    renderedFeatures
                        |> List.head
                        |> Maybe.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue featureName
                        |> Result.withDefault ""
                        |> String.split "."

                ( _, featName ) =
                    case feature of
                        [ a, b ] ->
                            ( a, b )

                        _ ->
                            ( "", "" )

                ( newModel,  cmd ) =
                    case model.mode of
                        Regions ->
                            case featName of
                                "" ->
                                    ( { model | selectedRegion = Nothing }, Cmd.none )

                                _ ->
                                    ( model, Requests.getRegionInfo featName (wrapMsg << GotRegion))

                        Events ->
                            case featName of
                                "" ->
                                    case model.insertModel of
                                        Just insertModel ->
                                            ( { model | insertModel = (
                                                Just <|
                                                InsertEvent.update
                                                    (InsertEvent.PointChoosed (Just lngLat))
                                                    insertModel
                                            )}, Cmd.none )

                                        Nothing ->
                                            ( { model | selectedEvent = Nothing }, Cmd.none )

                                _ ->
                                    ( { model | insertModel = Nothing }, Requests.getEventInfo featName (wrapMsg << GotEvent))

                        _ ->
                            ( model, Cmd.none)
            in
            ( newModel, cmd )

        SetMode mode ->
            let
                cmd = case mode of
                        Loading -> 
                            Cmd.none

                        Regions ->
                            Requests.getRegions (wrapMsg << GotRegions)

                        Events ->
                            Requests.getEvents (wrapMsg << GotEvents)

                        About ->
                            Requests.getAbout (wrapMsg << GotAbout)
            in
                ( { model | mode = mode
                          , hoveredFeatures = [] 
                          , insertModel = Nothing
                }, cmd )

        GotRegions (Ok regions) ->
            ( { model | regions = regions }, Cmd.none )

        GotRegions (Err _) ->
            ( model, Cmd.none )

        GotRegion (Ok regionInfo) ->
            ( { model | selectedRegion = Just regionInfo }, Cmd.none )

        GotRegion (Err _) ->
            ( { model | selectedRegion = Nothing }, Cmd.none )

        RegionInfoClosed ->
            ( { model | selectedRegion = Nothing }, Cmd.none )

        GotEvents (Ok events) ->
            ( { model | events = events }, Cmd.none )

        GotEvents (Err _) ->
            ( model, Cmd.none )

        GotEvent (Ok eventInfo) ->
            ( { model | selectedEvent = Just eventInfo }, Cmd.none )

        GotEvent (Err _) ->
            ( { model | selectedEvent = Nothing }, Cmd.none )

        EventInfoClosed ->
            ( { model | selectedEvent = Nothing }, Cmd.none )

        InsertMode ->
            ( { model | selectedEvent = Nothing, insertModel = Just InsertEvent.init }, Cmd.none )

        InsertEventMsg insertMsg ->
            case insertMsg of
                InsertEvent.InsertCancelled ->
                    ( { model | insertModel = Nothing }, Cmd.none )

                InsertEvent.InsertSubmitted ->
                    case model.insertModel of
                        Just insertModel ->
                            case ( insertModel.insertedPoint, insertModel.insertedName, insertModel.insertedDetails ) of
                                ( Just point, Just name, Just details ) ->
                                    ( { model | insertModel = Nothing }, Requests.postEvent 
                                                                            point
                                                                            name 
                                                                            details 
                                                                            (wrapMsg << GotEvents)
                                    )
                                _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    case model.insertModel of
                        Just insertModel ->
                            let
                                newInsertModel =
                                    InsertEvent.update insertMsg insertModel
                            in
                            ( { model | insertModel = Just newInsertModel }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

        GotAbout (Ok about) ->
            ( { model | about = about }, Cmd.none )

        GotAbout (Err _) ->
            ( model, Cmd.none )



--| change state of hovered features on the map


hoveredFeatures : List Json.Encode.Value -> MapboxAttr msg
hoveredFeatures =
    List.map (\feat -> ( feat, [ ( "hover", Json.Encode.bool True ) ] ))
        >> featureState


view : (Msg -> msg) -> Model -> Html msg
view wrapMsg model =
    let
        --| floating content can be either region info, event info or about text
        --| depending on the current mode
        content =
            case model.mode of
                Loading ->
                    div [] []
                Regions ->
                    viewRegionInfo (wrapMsg RegionInfoClosed) model.selectedRegion

                Events ->
                    viewEventInfo (wrapMsg EventInfoClosed) model.selectedEvent

                About ->
                    Html.div Styles.Attributes.about
                        [ Html.p [] [ Html.text model.about ]
                        ]

        --| button to start inserting a new event
        insertButton =
            if model.mode == Events && model.insertModel == Nothing then
                Html.button
                    (Styles.Attributes.insertButton
                        ++ [ Html.Events.onClick (wrapMsg InsertMode) ]
                    )
                    [ text "Add Event" ]
            else
                Html.div [] []

        insertForm =
            case model.insertModel of
                Just insertModel ->
                    InsertEvent.view (wrapMsg << InsertEventMsg) insertModel

                Nothing ->
                    Html.div [] []

    in
    div []
        [viewMap wrapMsg model
        , content
        , insertButton
        , insertForm
        ]



--| View the map based on the current mode
--| Sources are loaded from geojson code chunks


viewMap : (Msg -> msg) -> Model -> Html msg
viewMap wrapMsg model =
    let
        --| layers of the map are based on the current mode
        modeLayers =
            case model.mode of
                Regions ->
                    layersFromRegions model.regions

                Events ->
                    layersFromEvents model.events

                _ ->
                    []

        --| layers of the map that listen to events
        modeListenLayers =
            case model.mode of
                Regions ->
                    listenLayersFromRegions model.regions

                Events ->
                    listenLayersFromEvents model.events

                _ ->
                    []

        --| sources of the map are based on the current mode
        modeSources =
            case model.mode of
                Regions ->
                    sourcesFromRegions model.regions

                Events ->
                    sourcesFromEvents model.events

                _ ->
                    []
    in
    div Styles.Attributes.map
        [ map
            [ maxZoom 18
            , minZoom 10
            , maxBounds
                --| limit the map to Prague and its surroundings
                ( LngLat 14.098789849977067 49.932573881803535
                , LngLat 14.750530939532837 50.2500770495798
                )
            , onMouseMove (wrapMsg << Hover)
            , onClick (wrapMsg << Click)
            , id "paq-map"
            , eventFeaturesLayers modeListenLayers
            , hoveredFeatures model.hoveredFeatures --| highlight hovered features
            ]
            (Style
                { transition = Style.defaultTransition
                , light = Style.defaultLight
                , sources =
                    Source.vectorFromUrl "composite" "mapbox://mapbox.mapbox-terrain-v2,mapbox.mapbox-streets-v7"
                        :: modeSources
                , misc =
                    [ Style.defaultCenter <| LngLat 14.417941209392495 50.10093189854709
                    , Style.defaultZoomLevel 13
                    , Style.sprite "mapbox://sprites/mapbox/streets-v9"
                    , Style.glyphs "mapbox://fonts/mapbox/{fontstack}/{range}.pbf"
                    ]
                , layers =
                    --| layers that are always present
                    styleLayers
                        --| layers that depend on the current mode
                        ++ modeLayers
                }
            )
        ]
