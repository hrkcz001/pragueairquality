module Styles.Attributes exposing
    ( map
    , regionInfo
    , eventInfo
    , closeButton
    , titleName
    , headerBackground
    , header
    , entry
    , active
    , addDebt
    , content
    , dropdown
    , input
    , label
    , person
    , personDetail
    , personLink
    , personList
    , textarea
    )

import Html exposing (Attribute)
import Html.Attributes as Attributes

content : List (Attribute msg)
content =
    [ Attributes.style "display" "flex" ]

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

personList : List (Attribute msg)
personList =
    [ Attributes.style "flex-basis" "15%"
    , Attributes.style "margin" "1rem"
    , Attributes.style "padding-right" "1rem"
    , Attributes.style "border-right" "1px solid #000"
    ]


person : List (Attribute msg)
person =
    [ Attributes.style "padding" ".25rem 0" ]


personLink : List (Attribute msg)
personLink =
    [ Attributes.style "color" "blue"
    , Attributes.style "text-decoration" "underline"
    , Attributes.style "cursor" "pointer"
    ]


personDetail : List (Attribute msg)
personDetail =
    [ Attributes.style "flex-basis" "65%"
    , Attributes.style "position" "relative"
    ]


addDebt : List (Attribute msg)
addDebt =
    [ Attributes.style "flex-basis" "20%"
    , Attributes.style "margin" "1rem"
    , Attributes.style "padding-left" "1rem"
    , Attributes.style "border-left" "1px solid #000"
    ]


label : List (Attribute msg)
label =
    [ Attributes.style "display" "block"
    , Attributes.style "font-weight" "bold"
    , Attributes.style "margin-bottom" ".5rem"
    ]


dropdown : List (Attribute msg)
dropdown =
    [ Attributes.style "margin-bottom" "1rem"
    ]


input : List (Attribute msg)
input =
    [ Attributes.style "width" "100%"
    , Attributes.style "padding" "0.25rem"
    , Attributes.style "margin-bottom" "1rem"
    ]


textarea : List (Attribute msg)
textarea =
    [ Attributes.style "width" "100%"
    , Attributes.style "padding" "0.25rem"
    , Attributes.style "margin-bottom" "1rem"
    , Attributes.style "resize" "vertical"
    , Attributes.style "min-height" "5rem"
    ]
