module Utils exposing (toElmUiColor, styles, centeredText)

import Color
import Element as E
import Element.Background as Background
import Element.Border as Border

toElmUiColor : Color.Color -> E.Color
toElmUiColor color =
    let
        {red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha


styles :
  { button : List (E.Attribute msg)
  , tab : List (E.Attribute msg)
  }
styles =
  { button =
    [ Background.color <| toElmUiColor Color.lightGrey
    , E.mouseOver
        [ Background.color <| toElmUiColor Color.grey ]
    , E.paddingXY 10 5
    , E.width <| E.px 150
    , Border.widthXY 2 1
    , Border.color <| toElmUiColor Color.darkGrey
    ]
  , tab =
    [ Background.color <| toElmUiColor Color.lightGrey
    , E.mouseOver
        [ Background.color <| toElmUiColor Color.grey ]
    , Border.widthEach
      { top = 1
      , left = 1
      , bottom = 0
      , right = 1
      }
    , Border.color <| toElmUiColor Color.darkGrey
    , Border.roundEach
      { topLeft = 10
      , topRight = 10
      , bottomLeft = 0
      , bottomRight = 0
      }
    ]
  }


centeredText : String -> E.Element msg
centeredText text =
  E.el [ E.centerX ] <|
    E.text text
