module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Color exposing (Color)
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (px, Paint(..))
import Math.Vector2 as Vector2
import Draggable
import Draggable.Events
import Json.Decode as Json
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events
import Html.Events.Extra.Mouse as Mouse


type alias Model =
  { fields : List Field
  , activeSourceId : Maybe Id
  , nextId : Id
  , drag : Draggable.State Id
  , contextMenu : ContextMenu
  , popUp: PopUp
  }


type ContextMenu
  = FieldContextMenu
  | GeneralContextMenu Position
  | NoContextMenu


type PopUp
  = HelpPopUp
  | NoPopUp


type alias Field =
  { source: Charge
  , density: Int
  , steps: Int
  , delta: Float
  , lines: List Line
  }

type alias Id =
  Int

type alias Line =
  List Point

type alias Point =
  (Float, Float)

type alias Position =
  Point

type Sign
  = Positive
  | Negative

type alias Charge =
  { id : Id
  , sign: Sign
  , magnitude: Float
  , x: Float
  , y: Float
  , r: Float
  }

default :
  { r : Float
  , density : Int
  , steps : Int
  , delta : Float
  , magnitude : Float
  }
default =
  { r = 10.0
  , density = 30
  , steps = 900
  , delta = 1
  , magnitude = 1.0
  }


style :
  { button : List (E.Attribute Msg)
  }
style =
  { button =
    [ Background.color <| toElmUiColor Color.lightGrey
    , E.mouseOver
        [ Background.color <| toElmUiColor Color.grey ]
    , E.paddingXY 10 5
    , E.width <| E.px 150
    , Border.widthXY 2 1
    , Border.color <| toElmUiColor Color.darkGrey
    ]
  }


initialModel : () -> (Model, Cmd Msg)
initialModel _ =
  let
    fields =
      [{ source = { id = 0, sign = Negative, magnitude = 3.0, x = 500.0, y = 350.0, r = 10.0 }
      , density = 30
      , steps = 900
      , delta = 1
      , lines = []
      }
      , { source = { id = 1, sign = Positive, magnitude = 1.0, x = 600.0, y = 350.0, r = 10.0 }
      , density = 30
      , steps = 900
      , delta = 1
      , lines = []
      }
      , { source = { id = 2, sign = Positive, magnitude = 10.0, x = 500.0, y = 450.0, r = 10.0 }
      , density = 30
      , steps = 900
      , delta = 1
      , lines = []
      }
      , { source = { id = 3, sign = Negative, magnitude = 20.0, x = 600.0, y = 450.0, r = 10.0 }
      , density = 30
      , steps = 900
      , delta = 1
      , lines = []
      }
      ]
  in
  ({ fields =
    calculateFields fields
  , activeSourceId = if List.length fields > 0 then Just 0 else Nothing
  , nextId = List.length fields
  , drag = Draggable.init
  , contextMenu = NoContextMenu
  , popUp = NoPopUp
  }
  , Cmd.none
  )


calculateFields : List Field -> List Field
calculateFields fields =
  List.map
    (\field ->
      let
        deltaAngle =
          2 * pi / toFloat field.density
        lines =
          List.map
            (\index ->
              let
                angle =
                  deltaAngle * index
                x =
                  field.source.x + field.source.r * cos angle
                y =
                  field.source.y + field.source.r * sin angle
              in
              calculateFieldLine (List.map .source fields) field.steps field.delta field.source.sign (x, y)
            )
            (List.map toFloat <| List.range 0 (field.density - 1))
      in
      { field |
        lines = lines
      }
    )
    fields


calculateFieldLine : List Charge -> Int -> Float -> Sign -> Point -> Line
calculateFieldLine charges steps delta sourceSign start =
  List.foldl
    (\_ line ->
      let
        (x, y) =
          case line of
            prev :: _ ->
              prev
            _ ->
              (0, 0) -- impossible
        netField =
          List.foldl
          (\charge sum ->
            let
              d =
                distance (x, y) (charge.x, charge.y) / 100
              magnitude =
                charge.magnitude / (d ^ 2)
              sign =
                case charge.sign of
                  Positive ->
                    1
                  Negative ->
                    -1
              field =
                Vector2.scale (sign * magnitude) <|
                  Vector2.normalize <|
                    Vector2.vec2 (x - charge.x) (y - charge.y)
            in
            Vector2.add sum field
          )
          (Vector2.vec2 0 0)
          charges
        next =
          let
            vec =
              Vector2.add
                (Vector2.vec2 x y)
                ((case sourceSign of
                  Positive ->
                    identity
                  Negative ->
                    Vector2.negate
                )<|
                  Vector2.scale delta <|
                    Vector2.normalize netField
                )
          in
          (Vector2.getX vec, Vector2.getY vec)
      in
      next :: line
  )
  [ start ]
  (List.range 0 (steps - 1))


distance : Point -> Point -> Float
distance (x1, y1) (x2, y2) =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


type Msg
  = OnDragBy Draggable.Delta
  | DragMsg (Draggable.Msg Id)
  | StartDragging Id
  | ActivateSource Id
  | ToggleSourceSign
  | ScaleSourceMagnitude Int
  | ShowFieldContextMenu
  | ShowGeneralContextMenu Mouse.Event
  | DeleteActiveField
  | ClickedBackground
  | DuplicateActiveField
  | AddPositiveCharge Position
  | AddNegativeCharge Position
  | ShowPopUp PopUp


dragConfig : Draggable.Config Id Msg
dragConfig =
  Draggable.customConfig
    [ Draggable.Events.onDragBy OnDragBy
    , Draggable.Events.onDragStart StartDragging
    , Draggable.Events.onClick ActivateSource
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnDragBy delta ->
      let
        newFields =
          updateActive (dragSource delta) model.activeSourceId model.fields
      in
      ( { model |
        fields =
          calculateFields newFields
      }
      , Cmd.none
      )

    StartDragging id ->
      (setActiveSourceId id model, Cmd.none)

    ActivateSource id ->
      (setActiveSourceId id model, Cmd.none)

    ToggleSourceSign ->
      let
        newFields =
          updateActive
            (\field ->
              let
                source =
                  field.source
              in
              { field |
                source =
                  { source |
                    sign =
                      negateSign field.source.sign
                  }
              }
            )
            model.activeSourceId
            model.fields
      in
      ( { model |
        fields =
          calculateFields newFields
      }
      , Cmd.none
      )

    ScaleSourceMagnitude delta ->
      let
        newFields =
          updateActive
            (\field ->
              let
                source =
                  field.source
              in
              { field |
                source =
                  { source |
                    magnitude =
                      min 20 <| max 0.5 <| source.magnitude + toFloat delta * -0.01
                  }
              }
            )
            model.activeSourceId
            model.fields
      in
      ( { model |
        fields =
          calculateFields newFields
      }
      , Cmd.none
      )

    DragMsg dragMsg ->
      Draggable.update dragConfig dragMsg model

    ShowFieldContextMenu ->
      ({ model
        | contextMenu =
          case model.activeSourceId of
            Nothing ->
              model.contextMenu
            Just _ ->
              FieldContextMenu
      }
      , Cmd.none
      )

    ShowGeneralContextMenu { offsetPos } ->
      ({ model |
        contextMenu =
          GeneralContextMenu offsetPos
      }
      , Cmd.none
      )

    DeleteActiveField ->
      let
        newFields =
          case model.activeSourceId of
            Nothing ->
              model.fields
            Just id ->
              List.filter
                (\field ->
                  field.source.id /= id
                )
                model.fields
      in
      ( { model |
        fields =
          calculateFields newFields
        , contextMenu =
          NoContextMenu
        , activeSourceId =
          Nothing
      }
      , Cmd.none
      )

    ClickedBackground ->
      (resetState model, Cmd.none)

    DuplicateActiveField ->
      (duplicateActiveField model, Cmd.none)

    AddPositiveCharge position ->
      (addCharge Positive position model, Cmd.none)

    AddNegativeCharge position ->
      (addCharge Negative position model, Cmd.none)

    ShowPopUp popUp ->
      (showPopUp popUp model, Cmd.none)


showPopUp : PopUp -> Model -> Model
showPopUp popUp model =
  { model |
    popUp =
      popUp
  }


setActiveSourceId : Id -> Model -> Model
setActiveSourceId id model =
  { model |
    activeSourceId = Just id
  }


addCharge : Sign -> Position -> Model -> Model
addCharge sign (x, y) model =
  let
    newCharge : Charge
    newCharge =
      { sign = sign
      , magnitude = default.magnitude
      , x = x
      , y = y
      , r = default.r
      , id = model.nextId
      }
    newField : Field
    newField =
      { source = newCharge
      , density = default.density
      , steps = default.steps
      , delta = default.delta
      , lines = []
      }
    newFields : List Field
    newFields =
      newField :: model.fields
  in
  { model
    | fields =
      calculateFields newFields
    , nextId =
      model.nextId + 1
  }


duplicateActiveField : Model -> Model
duplicateActiveField model =
  let
    duplicatedFields =
      List.indexedMap
        (\index field ->
          let
            source =
              field.source
          in
          { field |
            source =
              { source
                | x =
                  source.x + source.r * 2 + 15
                , id =
                  model.nextId + index
              }
          }
        )
        (getActiveFields model)
    newFields =
      model.fields ++ duplicatedFields
  in
  { model
    | fields =
      calculateFields newFields
    , nextId =
      model.nextId + List.length duplicatedFields
  }


resetState : Model -> Model
resetState model =
  { model
    | contextMenu =
      NoContextMenu
    , popUp =
      NoPopUp
  }


updateActive : (Field -> Field) -> Maybe Id -> List Field -> List Field
updateActive func activeId fields =
  case activeId of
    Nothing ->
      fields
    Just id ->
      List.map
        (\field ->
          if field.source.id == id then
            func field
          else
            field
        )
        fields


dragSource : Position -> Field -> Field
dragSource (dx, dy) field =
  let
    source =
      field.source
  in
  { field |
    source =
      { source
        | x = field.source.x + dx
        , y = field.source.y + dy
      }
  }


view : Model -> Html Msg
view model =
  E.layout
    [ E.width E.fill
    , E.height E.fill
    , Element.Events.onClick ClickedBackground
    , E.htmlAttribute <| Mouse.onContextMenu ShowGeneralContextMenu
    , Font.size 16
    , Font.family
      [ Font.monospace
      ]
    ] <|
    E.el
      [ E.inFront <| viewContextMenu model
      , E.inFront <| viewPopUp model
      , E.below <| viewPopUpSelector
      , E.centerX
      , E.centerY
      ]
      ( E.html <| Svg.svg
        [ Attributes.width (px 1200)
        , Attributes.height (px 780)
        , Attributes.viewBox 0 0 1200 780
        ] <|
        List.map viewFieldLines model.fields
        ++ List.map (viewFieldSource model.activeSourceId) model.fields
      )


viewPopUpSelector : E.Element Msg
viewPopUpSelector =
  E.column
    [ E.centerX
    ]
    [ viewButtonNoProp <| ShowPopUp HelpPopUp
    ]


viewButtonNoProp : Msg -> E.Element Msg
viewButtonNoProp msg =
  Input.button (style.button ++ [
    E.htmlAttribute <| onClickNoProp msg
  ]) <|
    { onPress =
      Nothing
    , label =
      E.el [ E.centerX ]
        (E.text "Help")
    }


viewPopUp : Model -> E.Element Msg
viewPopUp model =
  case model.popUp of
    HelpPopUp ->
      viewHelpPopUp
    NoPopUp ->
      E.none


viewHelpPopUp : E.Element Msg
viewHelpPopUp =
  E.column
    [ E.centerX
    , E.centerY
    , E.padding 20
    , E.spacing 6
    , Background.color <| toElmUiColor Color.lightGrey
    , Border.width 2
    , Border.color <| toElmUiColor Color.black
    ]
    [ E.el
      [ Font.size 18
      , E.paddingEach
        { left = 0
        , right = 0
        , top = 0
        , bottom = 10
        }
      ] <|
      E.text "Help"
    , textHeader "When you mouse over a charge and ..."
    , E.text "  Single click: select charge"
    , E.text "  Double click: negate charge"
    , E.text "  Right click: * delete charge"
    , E.text "               * duplicate charge"
    , textHeader "When you mouse over background and ..."
    , E.text "  Right Click: * add + charge"
    , E.text "               * add - charge"
    ]


textHeader : String -> E.Element Msg
textHeader text =
  E.el
    [ E.paddingXY 0 6
    ] <|
    E.text text


viewContextMenu : Model -> E.Element Msg
viewContextMenu model =
  case model.contextMenu of
    FieldContextMenu ->
      viewFieldContextMenu style.button model
    GeneralContextMenu position ->
      viewGeneralContextMenu style.button position model
    NoContextMenu ->
      E.none


getActiveFields : Model -> List Field
getActiveFields model =
  case model.activeSourceId of
    Just id ->
      List.filter
        (\field ->
          field.source.id == id
        )
        model.fields
    Nothing ->
      []


viewFieldContextMenu : List (E.Attribute Msg) -> Model -> E.Element Msg
viewFieldContextMenu menuItemStyles model =
  let
    (x, y) =
      case List.head <| getActiveFields model of
        Just field ->
          (field.source.x, field.source.y)
        Nothing ->
          (0, 0) -- impossible
  in
  E.column
    [ E.moveRight x
    , E.moveDown y
    ]
    [ Input.button
      menuItemStyles
      { onPress = Just DeleteActiveField
      , label = E.text "delete"
      }
    , Input.button
      menuItemStyles
      { onPress = Just DuplicateActiveField
      , label = E.text "duplicate"
      }
    ]


viewGeneralContextMenu : List (E.Attribute Msg) -> Position -> Model -> E.Element Msg
viewGeneralContextMenu menuItemStyles (x, y) model =
  E.column
    [ E.moveRight x
    , E.moveDown y
    ]
    [ Input.button
      menuItemStyles
      { onPress = Just <| AddPositiveCharge (x, y)
      , label = E.text "add + charge"
      }
    , Input.button
      menuItemStyles
      { onPress = Just <| AddNegativeCharge (x, y)
      , label = E.text "add - charge"
      }
    ]


viewFieldSource : Maybe Id -> Field -> Svg Msg
viewFieldSource activeSourceId field =
  let
    fill =
      signToColor field.source.sign
    gradientId =
      "gradient" ++ String.fromInt field.source.id
  in
  Svg.g []
  [ Svg.defs []
    [ Svg.radialGradient
      [ Attributes.id <| gradientId ]
      [ Svg.stop
        [ Attributes.offset "1%"
        , Attributes.stopColor <| Color.toCssString <| setAlpha 1 fill
        ] []
      , Svg.stop
        [ Attributes.offset "100%"
        , Attributes.stopColor <| Color.toCssString <| setAlpha 0.2 fill
        ] []
      ]
    ]
  , Svg.circle
    [ Attributes.cx (px field.source.x)
    , Attributes.cy (px field.source.y)
    , Attributes.r (px <| lerp 0 20 10 40 (min 20 field.source.r * field.source.magnitude / 10))
    , Attributes.fill <| Reference gradientId
    , Html.Attributes.style "pointer-events" "none"
    ]
    []
  , Svg.circle
    ([ Attributes.cx (px field.source.x)
    , Attributes.cy (px field.source.y)
    , Attributes.r (px field.source.r)
    , Attributes.fill <| Paint fill
    , Draggable.mouseTrigger field.source.id DragMsg
    , onWheel ScaleSourceMagnitude
    , Html.Events.onDoubleClick ToggleSourceSign
    , onRightClick ShowFieldContextMenu
    ] ++ Draggable.touchTriggers field.source.id DragMsg
    ++ case activeSourceId of
      Just id ->
        if field.source.id == id then
          [ Attributes.stroke <| Paint Color.lightGreen
          , Attributes.strokeWidth <| px 2.5
          ]
        else
          []
      Nothing ->
        []
    )
    []
  ]


viewFieldLines : Field -> Svg Msg
viewFieldLines field =
  Svg.g [] <|
    List.map
      (\line -> Svg.polyline
        [ Attributes.fill PaintNone, Attributes.stroke <| Paint Color.black, Attributes.points line ]
        []
      )
      field.lines


onWheel : (Int -> msg) -> Html.Attribute msg
onWheel message =
  Html.Events.on "wheel" (Json.map message (Json.at ["deltaY"] Json.int ))


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
  Html.Events.custom "contextmenu"
    (Json.succeed
      { message = msg
      , stopPropagation = True
      , preventDefault = True
      }
    )


onClickNoProp : msg -> Html.Attribute Msg
onClickNoProp msg =
  Html.Events.custom "click"
    (Json.succeed
    { message = ShowPopUp HelpPopUp
    , stopPropagation = True
    , preventDefault = False
    }
  )


signToColor : Sign -> Color
signToColor sign =
  case sign of
    Positive ->
      Color.orange
    Negative ->
      Color.blue


setAlpha : Float -> Color -> Color
setAlpha alpha color =
  let
    rgba =
      Color.toRgba color
  in
  Color.fromRgba <|
    { rgba |
      alpha = alpha
    }


negateSign : Sign -> Sign
negateSign sign =
  case sign of
    Positive ->
      Negative
    Negative ->
      Positive


lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp min1 max1 min2 max2 num =
  let
    ratio =
      abs <| (num - min1) / (max1 - min1)
  in
  min2 + ratio * (max2 - min2)


toElmUiColor : Color.Color -> E.Color
toElmUiColor color =
    let
        {red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha


subscriptions : Model -> Sub Msg
subscriptions { drag } =
  Draggable.subscriptions DragMsg drag


main : Program () Model Msg
main =
  Browser.element
    { init = initialModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    }