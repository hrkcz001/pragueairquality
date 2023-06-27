module Event exposing (Event, EventInfo, sourcesFromEvents, layersFromEvents, listenLayersFromEvents, viewEventInfo)

import Json.Decode
import Json.Encode
import Mapbox.Source as Source
import Mapbox.Layer as Layer
import Mapbox.Expression as E
import Html exposing (Html)
import Html.Events

import Styles.Attributes

type alias Event = { id : String, lng : Float, lat : Float }
type alias EventInfo = { creator : String, description : String }

sourcesFromEvents : List Event -> List Source.Source
sourcesFromEvents events =
    let coords event = "[" ++ (String.fromFloat event.lng) ++ ", " ++ (String.fromFloat event.lat) ++ "]"
        eventToFeature event = """
            {
              "type": "Feature",
              "properties": {
                "name": \"event.""" ++ event.id ++ """\"
              },
              "geometry": {
                "type": "Point",
                "coordinates": """ ++ (coords event) ++ """
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
    |> Source.geoJSONFromValue "events" [Source.generateIds]
    |> List.singleton

layersFromEvents : List Event -> List Layer.Layer
layersFromEvents events = 
    List.map (\event -> 
        Layer.circle ("event." ++ event.id)
        "events"
        [   Layer.circleRadius (E.float 10)
        ,   Layer.circleColor (E.rgba 0 150 255 255)
        ,   Layer.circleStrokeColor (E.rgba 0 0 0 255)
        ,   Layer.circleStrokeWidth (E.float 1)
        ,   Layer.circleOpacity 
            (E.ifElse (E.toBool (E.featureState (E.str "hover"))) 
                (E.float 0.7) 
                (E.float 0.2)
            )
        ]
    ) events

listenLayersFromEvents : List Event -> List String
listenLayersFromEvents events = 
    List.map (\event -> "event." ++ event.id) events

viewEventInfo : msg -> Maybe EventInfo -> Html msg
viewEventInfo mapMsg eventInfo = 
    case eventInfo of
        Nothing -> Html.div [] []
        Just info ->
            Html.div Styles.Attributes.eventInfo
                [ Html.h2 [] [ Html.text info.creator ]
                , Html.p [] [ Html.text info.description ]
                , Html.button (Styles.Attributes.closeButton
                                ++ [ Html.Events.onClick mapMsg ])    
                        [ Html.text "X" ]
                ]
