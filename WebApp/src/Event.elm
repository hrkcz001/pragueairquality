module Event exposing (Event, EventInfo, layersFromEvents, listenLayersFromEvents, sourcesFromEvents, viewEventInfo)

import Html exposing (Html)
import Html.Events
import Json.Decode
import Json.Encode
import Mapbox.Expression as E
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Styles.Attributes



--| An event on the map
--| id is String because it is used as name for features


type alias Event =
    { id : String, lng : Float, lat : Float }



--| Information about an event


type alias EventInfo =
    { creator : String, description : String }



--| Create a list of sources from a list of events
--| Generates from GeoJSON
--| The sources are used to create layers


sourcesFromEvents : List Event -> List Source.Source
sourcesFromEvents events =
    let
        coords event =
            "[" ++ String.fromFloat event.lng ++ ", " ++ String.fromFloat event.lat ++ "]"

        eventToFeature event =
            """
            {
              "type": "Feature",
              "properties": {
                "name": "event.""" ++ event.id ++ """"
              },
              "geometry": {
                "type": "Point",
                "coordinates": """ ++ coords event ++ """
              }
            }
            """
    in
    List.map eventToFeature events
        |> String.join ","
        |> (\features -> Json.Decode.decodeString Json.Decode.value ("""
        {
          "type": "FeatureCollection",
          "features": [
              """ ++ features ++ """
           ]
        }
        """) |> Result.withDefault (Json.Encode.object []))
        |> Source.geoJSONFromValue "events" [ Source.generateIds ]
        |> List.singleton



--| Create a list of layers from a list of events
--| creates a circle layer for each event
--| hover effect by changing opacity


layersFromEvents : List Event -> List Layer.Layer
layersFromEvents events =
    List.map
        (\event ->
            Layer.circle ("event." ++ event.id)
                "events"
                [ Layer.circleRadius (E.float 10)
                , Layer.circleColor (E.rgba 0 150 255 255)
                , Layer.circleStrokeColor (E.rgba 0 0 0 255)
                , Layer.circleStrokeWidth (E.float 1)
                , Layer.circleOpacity
                    (E.ifElse (E.toBool (E.featureState (E.str "hover")))
                        (E.float 0.7)
                        (E.float 0.2)
                    )
                ]
        )
        events



--| Create a list of layer names from a list of events


listenLayersFromEvents : List Event -> List String
listenLayersFromEvents events =
    List.map (\event -> "event." ++ event.id) events



--| View for the event info


viewEventInfo : msg -> Maybe EventInfo -> Html msg
viewEventInfo mapMsg eventInfo =
    case eventInfo of
        Nothing ->
            Html.div [] []

        Just info ->
            Html.div Styles.Attributes.eventInfo
                [ Html.h2 [] [ Html.text info.creator ]
                , Html.p [] [ Html.text info.description ]
                , Html.button
                    (Styles.Attributes.closeButton
                        ++ [ Html.Events.onClick mapMsg ]
                    )
                    [ Html.text "X" ]
                ]
