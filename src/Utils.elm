module Utils exposing (centeredText, colors, styles, toElmUiColor)

import Color
import Element as E
import Element.Background as Background
import Element.Border as Border
import Html.Attributes


toElmUiColor : Color.Color -> E.Color
toElmUiColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha


colors =
    { white =
        E.rgb 0.97 0.97 0.97
    , lightGrey =
        E.rgb 0.87 0.87 0.87
    , darkGrey =
        toElmUiColor Color.darkGrey
    , black =
        toElmUiColor Color.black
    }


styles =
    { button =
        [ Background.color colors.lightGrey
        , E.mouseOver
            [ Background.color colors.white ]
        , E.paddingXY 10 5
        , E.width <| E.px 150
        , Border.widthXY 2 1
        , Border.color colors.darkGrey
        ]
    , tab =
        [ Background.color colors.lightGrey
        , E.mouseOver
            [ Background.color colors.white ]
        , Border.widthEach
            { top = 1
            , left = 1
            , bottom = 0
            , right = 1
            }
        , Border.color colors.darkGrey
        , Border.roundEach
            { topLeft = 10
            , topRight = 10
            , bottomLeft = 0
            , bottomRight = 0
            }
        ]
    , padTop10 =
        E.htmlAttribute <| Html.Attributes.style "margin-top" "10px"
    , padTop20 =
        E.htmlAttribute <| Html.Attributes.style "margin-top" "20px"
    , padBottom20 =
        E.htmlAttribute <| Html.Attributes.style "margin-bottom" "20px"
    , padBottom10 =
        E.htmlAttribute <| Html.Attributes.style "margin-bottom" "10px"
    }


centeredText : String -> E.Element msg
centeredText text =
    E.el [ E.centerX ] <|
        E.text text
