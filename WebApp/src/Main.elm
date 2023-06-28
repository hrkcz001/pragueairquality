module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation
import Html
import Html.Events exposing (onClick)
import Map exposing (Mode(..))
import Styles.Attributes
import Url
import Url.Parser as UrlParser
import Mapbox.Element exposing (css)



-- MAIN


main : Program () Model Msg
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
    , mapModel : Maybe Map.Model
    }


type Msg
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | MapMsg Map.Msg
    | GoTo String --| change url without reloading



--| Routes for the application


type Route
    = Map
    | NotFound



--| Url parser


routeParser : UrlParser.Parser ( ( Route, Maybe Mode ) -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map ( Map, Just Regions ) (UrlParser.s "map")
        , UrlParser.map ( Map, Just Events ) (UrlParser.s "events")
        , UrlParser.map ( Map, Just About ) (UrlParser.s "about")
        ]



--| Convert a url to a route


toRoute : Url.Url -> ( Route, Maybe Mode )
toRoute url =
    Maybe.withDefault ( NotFound, Nothing ) (UrlParser.parse routeParser url)


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( route, mode ) =
            toRoute url
        ( mapModel, mapCmd ) =
            case mode of
                Just initMode ->
                    Map.update MapMsg (Map.SetMode initMode) Map.init
                    |> Tuple.mapFirst Just
                _ ->
                    ( Nothing, Cmd.none )
    in
    ( { key = key
      , route = route
      , mapModel = mapModel
      }
    , mapCmd
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

        --| Update the route when the url changes
        ChangedUrl url ->
            let
                ( route, mode ) =
                    toRoute url
                ( newMapModel, mapCmd ) =
                    case mode of
                        Nothing ->
                            ( Nothing, Cmd.none )

                        Just newMode ->
                            Map.update MapMsg (Map.SetMode newMode) (Maybe.withDefault Map.init model.mapModel)
                            |> Tuple.mapFirst Just
            in
            ( { model | route = route, mapModel = newMapModel }
            , mapCmd
            )

        MapMsg mapMsg ->
            let
                ( newMapModel, mapCmd ) =
                    case model.mapModel of
                        Nothing ->
                            ( Nothing, Cmd.none )

                        Just mapModel ->
                            Map.update MapMsg mapMsg mapModel
                            |> Tuple.mapFirst Just
            in
            ( { model | mapModel = newMapModel }
            , mapCmd
            )

        GoTo url ->
            ( model
            , Browser.Navigation.replaceUrl model.key url
            )



--| Switch map mode based on the route


view : Model -> Document Msg
view model =
    let
        --| Styles for the header entries
        defaultStyles =
            { map = Styles.Attributes.entry
            , events = Styles.Attributes.entry
            , about = Styles.Attributes.entry
            }

        mapMode = 
            Maybe.map .mode <| model.mapModel

        --| Active entry
        entryStyles =
            case mapMode of
                Just Regions ->
                    { defaultStyles | map = Styles.Attributes.entry ++ Styles.Attributes.active }

                Just Events ->
                    { defaultStyles | events = Styles.Attributes.entry ++ Styles.Attributes.active }

                Just About ->
                    { defaultStyles | about = Styles.Attributes.entry ++ Styles.Attributes.active }

                _ ->
                    defaultStyles

        --| Header with links to the different map modes
        header =
            Html.div []
                [ Html.div Styles.Attributes.headerBackground []
                , Html.div
                    (Styles.Attributes.titleName
                        ++ [ onClick <| GoTo "/map" ]
                    )
                    [ Html.text "Prague Air Quality" ]
                , Html.div Styles.Attributes.header
                    [ Html.div
                        (entryStyles.map
                            ++ [ onClick <| GoTo "/map" ]
                        )
                        [ Html.text "Map" ]
                    , Html.div
                        (entryStyles.events
                            ++ [ onClick <| GoTo "/events" ]
                        )
                        [ Html.text "Events" ]
                    , Html.div
                        (entryStyles.about
                            ++ [ onClick <| GoTo "/about" ]
                        )
                        [ Html.text "About" ]
                    ]
                ]

        --| Map
        content =
            case model.route of
                Map ->
                    case model.mapModel of
                        Nothing ->
                            Html.div []
                                [ Html.h1 [] [ Html.text "Loading..." ]
                                ]

                        Just mapModel ->
                            Map.view MapMsg mapModel

                NotFound ->
                    Html.div []
                        [ Html.h1 [] [ Html.text "Not Found" ]
                        ]
    in
    { title = "Prague Air Quality"
    , body = [ css, header, content ]
    }
