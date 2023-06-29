module InsertEvent exposing (Model, Msg(..), init, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import LngLat exposing (LngLat)

import Styles.Attributes

type alias Model =
    { insertedPoint : Maybe LngLat
    , insertedName : Maybe String
    , insertedDetails : Maybe String
    }

type Msg
    = PointChoosed (Maybe LngLat)
    | NameInserted String
    | DetailsInserted String
    | InsertCancelled
    | InsertSubmitted

init : Model
init =
    { insertedPoint = Nothing
    , insertedName = Nothing
    , insertedDetails = Nothing
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        PointChoosed point ->
            { model | insertedPoint = point }

        NameInserted name ->
            { model | insertedName = Just name }

        DetailsInserted details ->
            { model | insertedDetails = Just details }

        _ ->
            model

view : (Msg -> msg ) -> Model -> Html msg
view wrapMsg model =
    case model.insertedPoint of
        Nothing ->
            Html.div []
                [ Html.text "Choose a point on the map first"
                , Html.button
                    (Styles.Attributes.insertButton
                        ++ [ Html.Events.onClick (wrapMsg InsertCancelled) ]
                    )
                    [ Html.text "Cancel" ]
                ]
        Just _ ->
            Html.div Styles.Attributes.eventInfo
                [ Html.input
                    (Styles.Attributes.inputName
                        ++ [ Html.Attributes.placeholder "Name"
                           , Html.Events.onInput (wrapMsg << NameInserted)
                           ]
                    )
                    []
                , Html.textarea
                    (Styles.Attributes.inputDetails
                        ++ [ Html.Attributes.placeholder "Details"
                           , Html.Events.onInput (wrapMsg << DetailsInserted)
                           ]
                    )
                    []
                , Html.button
                    (Styles.Attributes.closeButton
                        ++ [ Html.Events.onClick (wrapMsg InsertCancelled) ]
                    )
                    [ Html.text "X" ]

                --| disable submit button if name or details are empty
                , Html.button
                    (Styles.Attributes.insertingSubmit
                        ++ [ Html.Attributes.disabled <|
                                model.insertedName
                                    == Nothing
                                    || model.insertedName
                                    == Just ""
                                    || model.insertedDetails
                                    == Nothing
                                    || model.insertedDetails
                                    == Just ""
                           , Html.Events.onClick (wrapMsg InsertSubmitted)
                           ]
                    )
                    [ Html.text "Submit" ]
                ]
