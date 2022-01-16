module Icons exposing
    ( minusCircle
    , mousePointer
    , pause
    , play
    , plusCircle
    , xSquare
    )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


minusCircle : Html msg
minusCircle =
    svgFeatherIcon "minus-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


mousePointer : Html msg
mousePointer =
    svgFeatherIcon "mouse-pointer"
        [ Svg.path [ d "M3 3l7.07 16.97 2.51-7.39 7.39-2.51L3 3z" ] []
        , Svg.path [ d "M13 13l6 6" ] []
        ]


pause : Html msg
pause =
    svgFeatherIcon "pause"
        [ Svg.rect [ Svg.Attributes.x "6", y "4", width "4", height "16" ] []
        , Svg.rect [ Svg.Attributes.x "14", y "4", width "4", height "16" ] []
        ]


play : Html msg
play =
    svgFeatherIcon "play"
        [ Svg.polygon [ points "5 3 19 12 5 21 5 3" ] []
        ]


plusCircle : Html msg
plusCircle =
    svgFeatherIcon "plus-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "16" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


xSquare : Html msg
xSquare =
    svgFeatherIcon "x-square"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "9", y2 "15" ] []
        ]
