module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation
import Url
import Url.Parser as UrlParser
import Html

import Html.Events exposing (onClick)
import Json.Decode as Json

import Map

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }

type alias Model =
    { key : Browser.Navigation.Key
    , route : Route
    , mapModel : Map.Model
    }

type Msg
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | MapMsg Map.Msg
    | GoTo String

type Route
    = Regions
    | Clear
    | Test
    | NotFound

type alias Flags =
    Maybe String

routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Regions (UrlParser.s "map")
        , UrlParser.map Test (UrlParser.s "test")
        ]

toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (UrlParser.parse routeParser url)

init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init token url key =
    let
        route =
            toRoute url
        ( mapModel, mapCmd ) =
            Map.init MapMsg
    in
    ( { key = key
      , route = route
      , mapModel = mapModel
      }
    , Cmd.batch
        [ mapCmd
        ]
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.replaceUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        ChangedUrl url ->
            ( { model | route = toRoute url }
            , Cmd.none
            )
        MapMsg mapMsg ->
            let
                ( newMapModel, mapCmd ) =
                    Map.update MapMsg mapMsg model.mapModel
            in
            ( { model | mapModel = newMapModel }
            , mapCmd
            )
        GoTo url ->
            ( model
            , Browser.Navigation.replaceUrl model.key url
            )

view : Model -> Document Msg
view model =
    let
        content =
            case model.route of
                Regions ->
                    Html.map MapMsg
                    <| Map.view Map.Regions model.mapModel

                Clear ->
                    Html.map MapMsg
                    <| Map.view Map.Clear model.mapModel

                Test ->
                    Html.div  [ onClick <| GoTo "/map"
                              ] [ Html.text "Map" ]

                NotFound ->
                    Html.text "Not found"
    in
    { title = "Prague Air Quality"
    , body = [ content ]
    }
