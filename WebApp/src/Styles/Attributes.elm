module Styles.Attributes exposing
    ( about
    , active
    , closeButton
    , entry
    , eventInfo
    , header
    , headerBackground
    , inputDetails
    , inputName
    , insertButton
    , choosePointHint
    , insertingSubmit
    , map
    , regionInfo
    , titleName
    )

import Html exposing (Attribute)
import Html.Attributes as Attributes


map : List (Attribute msg)
map =
    [ Attributes.style "position" "absolute"
    , Attributes.style "z-index" "0"
    , Attributes.style "width" "100%"
    , Attributes.style "height" "93%"
    , Attributes.style "top" "7%"
    , Attributes.style "left" "0%"
    , Attributes.style "overflow" "hidden"
    ]


headerBackground : List (Attribute msg)
headerBackground =
    [ Attributes.style "position" "absolute"
    , Attributes.style "z-index" "0"
    , Attributes.style "width" "100%"
    , Attributes.style "height" "7%"
    , Attributes.style "top" "0%"
    , Attributes.style "left" "0%"
    , Attributes.style "background-color" "ghostwhite"
    , Attributes.style "box-sizing" "border-box"
    , Attributes.style "border-bottom" "1px solid #000"
    ]


titleName : List (Attribute msg)
titleName =
    [ Attributes.style "position" "absolute"
    , Attributes.style "z-index" "1"
    , Attributes.style "top" "0%"
    , Attributes.style "left" "0%"
    , Attributes.style "cursor" "pointer"
    , Attributes.style "color" "darkcyan"
    , Attributes.style "padding" "0.5rem"
    , Attributes.style "font-size" "2.125rem"
    ]


header : List (Attribute msg)
header =
    [ Attributes.style "position" "absolute"
    , Attributes.style "text-align" "center"
    , Attributes.style "display" "table"
    , Attributes.style "z-index" "1"
    , Attributes.style "width" "50%"
    , Attributes.style "height" "7%"
    , Attributes.style "top" "0%"
    , Attributes.style "left" "25%"
    ]


entry : List (Attribute msg)
entry =
    [ Attributes.style "display" "table-cell"
    , Attributes.style "cursor" "pointer"
    , Attributes.style "vertical-align" "middle"
    ]


active : List (Attribute msg)
active =
    [ Attributes.style "text-decoration" "underline"
    , Attributes.style "color" "darkgreen"
    ]


closeButton : List (Attribute msg)
closeButton =
    [ Attributes.style "position" "absolute"
    , Attributes.style "top" "0"
    , Attributes.style "right" "0"
    , Attributes.style "cursor" "pointer"
    , Attributes.style "font-weight" "bold"
    , Attributes.style "color" "#000"
    , Attributes.style "text-decoration" "none"
    ]


insertButton : List (Attribute msg)
insertButton =
    [ Attributes.style "position" "absolute"
    , Attributes.style "top" "9%"
    , Attributes.style "right" "2%"
    , Attributes.style "font-size" "1.2rem"
    , Attributes.style "cursor" "pointer"
    , Attributes.style "color" "#000"
    , Attributes.style "text-decoration" "none"
    ]

choosePointHint : List (Attribute msg)
choosePointHint =
    [ Attributes.style "position" "absolute"
    , Attributes.style "text-align" "center"
    , Attributes.style "top" "10%"
    , Attributes.style "left" "0%"
    , Attributes.style "width" "100%"
    , Attributes.style "font-size" "1.5rem"
    , Attributes.style "font-weight" "bold"
    , Attributes.style "color" "#000"
    , Attributes.style "text-decoration" "none"
    ]


regionInfo : List (Attribute msg)
regionInfo =
    [ Attributes.style "position" "absolute"
    , Attributes.style "z-index" "2"
    , Attributes.style "width" "20%"
    , Attributes.style "height" "93%"
    , Attributes.style "top" "7%"
    , Attributes.style "left" "0%"
    , Attributes.style "overflow" "auto"
    , Attributes.style "background-color" "FloralWhite"
    , Attributes.style "padding" "1rem"
    , Attributes.style "box-sizing" "border-box"
    , Attributes.style "border-right" "1px solid #000"
    ]


eventInfo : List (Attribute msg)
eventInfo =
    [ Attributes.style "position" "absolute"
    , Attributes.style "z-index" "2"
    , Attributes.style "width" "60%"
    , Attributes.style "height" "60%"
    , Attributes.style "top" "20%"
    , Attributes.style "left" "20%"
    , Attributes.style "overflow" "auto"
    , Attributes.style "background-color" "FloralWhite"
    , Attributes.style "padding" "1rem"
    , Attributes.style "box-sizing" "border-box"
    , Attributes.style "border" "1px solid #000"
    ]


inputDetails : List (Attribute msg)
inputDetails =
    [ Attributes.style "position" "absolute"
    , Attributes.style "width" "80%"
    , Attributes.style "height" "60%"
    , Attributes.style "top" "25%"
    , Attributes.style "left" "10%"
    , Attributes.style "background-color" "#fff"
    , Attributes.style "border" "1px solid #000"
    ]


inputName : List (Attribute msg)
inputName =
    [ Attributes.style "position" "absolute"
    , Attributes.style "width" "30%"
    , Attributes.style "height" "5%"
    , Attributes.style "top" "5%"
    , Attributes.style "left" "10%"
    , Attributes.style "background-color" "#fff"
    , Attributes.style "border" "1px solid #000"
    ]


insertingSubmit : List (Attribute msg)
insertingSubmit =
    [ Attributes.style "position" "absolute"
    , Attributes.style "width" "10%"
    , Attributes.style "height" "5%"
    , Attributes.style "top" "90%"
    , Attributes.style "left" "45%"
    , Attributes.style "cursor" "pointer"
    , Attributes.style "color" "#000"
    , Attributes.style "text-decoration" "none"
    ]


about : List (Attribute msg)
about =
    [ Attributes.style "position" "absolute"
    , Attributes.style "z-index" "2"
    , Attributes.style "width" "70%"
    , Attributes.style "height" "60%"
    , Attributes.style "top" "20%"
    , Attributes.style "left" "15%"
    , Attributes.style "overflow" "auto"
    , Attributes.style "white-space" "pre-wrap"
    , Attributes.style "background-color" "FloralWhite"
    , Attributes.style "padding" "1rem"
    , Attributes.style "box-sizing" "border-box"
    , Attributes.style "border" "1px solid #000"
    ]
