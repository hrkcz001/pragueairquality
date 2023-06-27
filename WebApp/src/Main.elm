module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation
import Html
import Html.Events exposing (onClick)
import Map
import Styles.Attributes
import Url
import Url.Parser as UrlParser



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
    , mapModel : Map.Model
    }


type Msg
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | MapMsg Map.Msg
    | GoTo String --| change url without reloading



--| Routes for the application


type Route
    = Regions
    | Events
    | About
    | NotFound



--| Url parser


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Regions (UrlParser.s "map")
        , UrlParser.map Events (UrlParser.s "events")
        , UrlParser.map About (UrlParser.s "about")
        ]



--| Convert a url to a route


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (UrlParser.parse routeParser url)


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
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

        --| Update the route when the url changes
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

        --| Active entry
        entryStyles =
            case model.route of
                Regions ->
                    { defaultStyles | map = Styles.Attributes.entry ++ Styles.Attributes.active }

                Events ->
                    { defaultStyles | events = Styles.Attributes.entry ++ Styles.Attributes.active }

                About ->
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
                Regions ->
                    Html.map MapMsg <|
                        Map.view Map.Regions model.mapModel

                Events ->
                    Html.map MapMsg <|
                        Map.view Map.Events model.mapModel

                About ->
                    Html.map MapMsg <|
                        Map.view Map.About model.mapModel

                NotFound ->
                    Html.div []
                        [ Html.h1 [] [ Html.text "Not Found" ]
                        ]
    in
    { title = "Prague Air Quality"
    , body = [ header, content ]
    }
