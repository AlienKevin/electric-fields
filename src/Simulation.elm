module Simulation exposing (Model, Msg, Settings, init, view, update, subscriptions, encodeModel, decodeModel, calculateFields, defaultName, defaultSettings)

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
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Json.Encode as Encode
import Element as E
import Element.Input as Input
import Element.Font as Font
import Element.Events
import Html.Events.Extra.Mouse as Mouse
import Process
import Task
import Utils exposing (styles)


type alias Model =
  { name : String
  , fields : List Field
  , activeSourceId : Maybe Id
  , nextId : Id
  , drag : Draggable.State Id
  , contextMenu : ContextMenu
  , settings : Settings
  , isWheeling : Bool
  , isWheelingTimeOutCleared : Bool
  , width : Float
  , height : Float
  }


type alias Settings =
  { r : Float
  , density : Int
  , steps : Int
  , delta : Float
  , magnitude : Float
  }


type ContextMenu
  = FieldContextMenu
  | GeneralContextMenu Position
  | NoContextMenu


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


defaultSettings : Settings
defaultSettings =
  { r = 10.0
  , density = 30
  , steps = 900
  , delta = 1
  , magnitude = 1.0
  }

defaultName : String
defaultName =
  "Untitled Model"


init : Model
init =
  let
    defaultFields =
      [{ source = { id = 0, sign = Negative, magnitude = 3.0, x = 465.0, y = 270.0, r = 10.0 }
      , density = 30
      , steps = 900
      , delta = 1
      , lines = []
      }
      , { source = { id = 1, sign = Positive, magnitude = 1.0, x = 618.0, y = 515.0, r = 10.0 }
      , density = 30
      , steps = 900
      , delta = 1
      , lines = []
      }
      , { source = { id = 2, sign = Positive, magnitude = 10.0, x = 553.0, y = 338.0, r = 10.0 }
      , density = 30
      , steps = 900
      , delta = 1
      , lines = []
      }
      , { source = { id = 3, sign = Negative, magnitude = 20.0, x = 597.0, y = 182.0, r = 10.0 }
      , density = 30
      , steps = 900
      , delta = 1
      , lines = []
      }
      ]

    defaultWidth = 1200

    defaultHeight = 750

    defaultModel =
      { name = defaultName
      , fields =
        calculateFields defaultWidth defaultHeight defaultFields
      , activeSourceId = if List.length defaultFields > 0 then Just 0 else Nothing
      , nextId = List.length defaultFields
      , drag = Draggable.init
      , contextMenu = NoContextMenu
      , settings = defaultSettings
      , isWheeling = False
      , isWheelingTimeOutCleared = False
      , width = defaultWidth
      , height = defaultHeight
      }
  in
  defaultModel


calculateFields : Float -> Float -> List Field -> List Field
calculateFields width height fields =
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
              calculateFieldLine
                { charges = List.map .source fields
                , steps = field.steps
                , delta = field.delta
                , sourceSign = field.source.sign
                , start = (x, y)
                , xBound = width
                , yBound = height
                }
            )
            (List.map toFloat <| List.range 0 (field.density - 1))
      in
      { field |
        lines = lines
      }
    )
    fields


calculateFieldLine :
  { charges : List Charge
  , steps : Int
  , delta : Float
  , sourceSign : Sign
  , start : Point
  , xBound : Float
  , yBound : Float
  } -> Line
calculateFieldLine { charges, steps, delta, sourceSign, start, xBound, yBound } =
  foldlWhile
    (\_ line ->
      let
        (x, y) =
          case line of
            prev :: _ ->
              prev
            _ ->
              (0, 0) -- impossible
        outOfBounds =
          x > xBound || x < 0 || y > yBound || y < 0
        netField =
          if outOfBounds then
            Vector2.vec2 0 0
          else
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
          if outOfBounds then
            (x, y)
          else
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
      (next :: line, outOfBounds)
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
  | EndDragging
  | ActivateSource Id
  | ToggleSourceSign
  | ScaleSourceMagnitude Int
  | ShowFieldContextMenu
  | ShowGeneralContextMenu Mouse.Event
  | DeleteActiveField
  | ClickedBackground
  | DuplicateActiveField
  | DeselectActiveField
  | AddPositiveCharge Position
  | AddNegativeCharge Position
  | StopWheelingTimeOut


dragConfig : Draggable.Config Id Msg
dragConfig =
  Draggable.customConfig
    [ Draggable.Events.onDragBy OnDragBy
    , Draggable.Events.onDragStart StartDragging
    , Draggable.Events.onDragEnd EndDragging
    , Draggable.Events.onClick ActivateSource
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnDragBy offsetPos ->
      (onDragBy offsetPos model, Cmd.none)

    StartDragging id ->
      (startDragging id model, Cmd.none)

    EndDragging ->
      (endDragging model, Cmd.none)

    ActivateSource id ->
      (setActiveSourceId id model, Cmd.none)

    ToggleSourceSign ->
      (toggleSourceSign model, Cmd.none)

    ScaleSourceMagnitude delta ->
      scaleSourceMagnitude delta model

    StopWheelingTimeOut ->
      (stopWheelingTimeOut model, Cmd.none)

    DragMsg dragMsg ->
      Draggable.update dragConfig dragMsg model

    ShowFieldContextMenu ->
      (showFieldContextMenu model, Cmd.none)

    ShowGeneralContextMenu { offsetPos } ->
      (showGeneralContextMenu offsetPos model, Cmd.none)

    DeleteActiveField ->
      (deleteActiveField model, Cmd.none)

    ClickedBackground ->
      (resetState model, Cmd.none)

    DuplicateActiveField ->
      (duplicateActiveField model, Cmd.none)

    DeselectActiveField ->
      (deselectActiveField model, Cmd.none)

    AddPositiveCharge position ->
      (addCharge Positive position model, Cmd.none)

    AddNegativeCharge position ->
      (addCharge Negative position model, Cmd.none)


onDragBy : Position -> Model -> Model
onDragBy offsetPos model =
  let
    newFields =
      updateActive (dragSource offsetPos) model.activeSourceId model.fields
  in
  { model |
    fields =
      calculateFields model.width model.height newFields
  }


startDragging : Id -> Model -> Model
startDragging id model =
  setActiveSourceId id <| optimizeModel model


optimizeModel : Model -> Model
optimizeModel model =
  { model
    | fields =
      List.map
        (\field ->
          if field.delta <= 7 then
            { field
              | delta =
                field.delta * 3
              , steps =
                round <| toFloat field.steps / 3
            }
          else
            field
        )
        model.fields
  }


endDragging : Model -> Model
endDragging model =
  deoptimizeModel model


deoptimizeModel : Model -> Model
deoptimizeModel model =
  { model
    | fields =
      calculateFields model.width model.height <|
      List.map
        (\field ->
          if field.delta <= 7 then
            { field
              | delta =
                field.delta / 3
              , steps =
                field.steps * 3
            }
          else
            field
        )
        model.fields
  }


encodeModel : Model -> Encode.Value
encodeModel { name, fields, activeSourceId, nextId, settings, width, height } =
  let
    encodeSign : Sign -> Encode.Value
    encodeSign sign =
      case sign of
        Positive ->
          Encode.string "Positive"
        Negative ->
          Encode.string "Negative"

    encodeCharge : Charge -> Encode.Value
    encodeCharge { id, sign, magnitude, x, y, r } =
      Encode.object
        [ ("id", Encode.int id)
        , ("sign", encodeSign sign)
        , ("magnitude", Encode.float magnitude)
        , ("x", Encode.float x)
        , ("y", Encode.float y)
        , ("r", Encode.float r)
        ]
    
    encodeField : Field -> Encode.Value
    encodeField { source, density, steps, delta } =
      Encode.object
      [ ("source", encodeCharge source)
      , ("density", Encode.int density)
      , ("steps", Encode.int steps)
      , ("delta", Encode.float delta)
      ]

    encodeSettings : Settings -> Encode.Value
    encodeSettings { magnitude, r, density, steps, delta} =
      Encode.object
      [ ("magnitude", Encode.float magnitude)
      , ("r", Encode.float r)
      , ("density", Encode.int density)
      , ("steps", Encode.int steps)
      , ("delta", Encode.float delta)
      ]
    encodeMaybeId : Maybe Id -> Encode.Value
    encodeMaybeId maybeId =
      case maybeId of
        Just id ->
          Encode.int id
        Nothing ->
          Encode.null
  in
  Encode.object
    [ ("name", Encode.string name)
    , ("fields", Encode.list encodeField fields)
    , ("activeSourceId", encodeMaybeId activeSourceId)
    , ("nextId", Encode.int nextId)
    , ("settings", encodeSettings settings)
    , ("width", Encode.float width)
    , ("height", Encode.float height)
    ]


decodeModel : Decoder Model
decodeModel =
  let
    decodeSign =
      Decode.string
        |> Decode.andThen
        (\sign ->
          case sign of
            "Positive" ->
              Decode.succeed Positive
            "Negative" ->
              Decode.succeed Negative
            _ ->
              Decode.fail ("I can't recognize \"" ++ sign ++ "\". It should be either \"Postive\" or \"Negative\"")
        )

    decodeCharge =
      Field.require "id" Decode.int <| \id ->
      Field.require "sign" decodeSign <| \sign ->
      Field.require "magnitude" Decode.float <| \magnitude ->
      Field.require "x" Decode.float <| \x ->
      Field.require "y" Decode.float <| \y ->
      Field.require "r" Decode.float <| \r ->

      Decode.succeed
        { id = id
        , sign = sign
        , magnitude = magnitude
        , x = x
        , y = y
        , r = r
        }

    decodeField =
      Field.require "source" decodeCharge <| \source ->
      Field.require "density" Decode.int <| \density ->
      Field.require "steps" Decode.int <| \steps ->
      Field.require "delta" Decode.float <| \delta ->

      Decode.succeed
        { source = source
        , density = density
        , steps = steps
        , delta = delta
        , lines = []
        }

    decodeSettings =
      Field.require "r" Decode.float <| \r ->
      Field.require "magnitude" Decode.float <| \magnitude ->
      Field.require "density" Decode.int <| \density ->
      Field.require "steps" Decode.int <| \steps ->
      Field.require "delta" Decode.float <| \delta ->

      Decode.succeed
        { r = r
        , magnitude = magnitude
        , density = density
        , steps = steps
        , delta = delta
        }
    
  in
  Field.require "name" Decode.string <| \name ->
  Field.require "fields" (Decode.list decodeField) <| \fields ->
  Field.attempt "activeSourceId" Decode.int <| \activeSourceId ->
  Field.require "nextId" Decode.int <| \nextId ->
  Field.require "settings" decodeSettings <| \settings ->
  Field.require "width" Decode.float <| \width ->
  Field.require "height" Decode.float <| \height ->

  Decode.succeed
    { name = name
    , fields = calculateFields width height fields
    , activeSourceId = activeSourceId
    , nextId = nextId
    , settings = settings
    , width = width
    , height = height
    , drag = Draggable.init
    , contextMenu = NoContextMenu
    , isWheeling = False
    , isWheelingTimeOutCleared = False
    }


setActiveSourceId : Id -> Model -> Model
setActiveSourceId id model =
  { model |
    activeSourceId = Just id
  }


toggleSourceSign : Model -> Model
toggleSourceSign model =
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
  { model
    | fields =
      calculateFields model.width model.height newFields
  }


scaleSourceMagnitude : Int -> Model -> (Model, Cmd Msg)
scaleSourceMagnitude delta model =
  let
    newModel =
      if model.isWheeling then
        model
      else
        optimizeModel model
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
        newModel.activeSourceId
        newModel.fields
  in
  ({ model
    | fields =
      calculateFields model.width model.height newFields
    , isWheeling =
      True
    , isWheelingTimeOutCleared =
      True
  }
  , setTimeOut 200 StopWheelingTimeOut
  )


setTimeOut : Float -> msg -> Cmd msg
setTimeOut time msg =
  Process.sleep time
  |> Task.perform (\_ -> msg)


stopWheelingTimeOut : Model -> Model
stopWheelingTimeOut model =
  if model.isWheelingTimeOutCleared then
    { model
      | isWheelingTimeOutCleared = False
    }
  else
    deoptimizeModel { model
      | isWheeling = False
      , isWheelingTimeOutCleared = False
    }


showFieldContextMenu : Model -> Model
showFieldContextMenu model =
  { model
    | contextMenu =
      case model.activeSourceId of
        Nothing ->
          model.contextMenu
        Just _ ->
          FieldContextMenu
  }


showGeneralContextMenu : Position -> Model -> Model
showGeneralContextMenu offsetPos model =
  { model |
    contextMenu =
      GeneralContextMenu offsetPos
  }


deleteActiveField : Model -> Model
deleteActiveField model =
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
  { model |
    fields =
      calculateFields model.width model.height newFields
    , contextMenu =
      NoContextMenu
    , activeSourceId =
      Nothing
  }


addCharge : Sign -> Position -> Model -> Model
addCharge sign (x, y) model =
  let
    newCharge : Charge
    newCharge =
      { sign = sign
      , magnitude = model.settings.magnitude
      , x = x
      , y = y
      , r = model.settings.r
      , id = model.nextId
      }
    newField : Field
    newField =
      { source = newCharge
      , density = model.settings.density
      , steps = model.settings.steps
      , delta = model.settings.delta
      , lines = []
      }
    newFields : List Field
    newFields =
      newField :: model.fields
  in
  { model
    | fields =
      calculateFields model.width model.height newFields
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
      calculateFields model.width model.height newFields
    , nextId =
      model.nextId + List.length duplicatedFields
  }


deselectActiveField : Model -> Model
deselectActiveField model =
  { model
    | activeSourceId =
      Nothing
  }


resetState : Model -> Model
resetState model =
  { model
    | contextMenu =
      NoContextMenu
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
    , Font.size 16
    , Font.family
      [ Font.monospace
      ]
    ] <|
    E.el
      [ E.inFront <| viewContextMenu model
      , E.centerX
      , E.centerY
      , E.paddingXY 0 5
      ]
      ( E.html <| Svg.svg
        [ Attributes.width (px model.width)
        , Attributes.height (px model.height)
        , Attributes.viewBox 0 0 model.width model.height
        , Attributes.id "modelSvg"
        , Mouse.onContextMenu ShowGeneralContextMenu
        ] <|
        List.map viewFieldLines model.fields
        ++ List.map (viewFieldSource model.activeSourceId) model.fields
      )


viewContextMenu : Model -> E.Element Msg
viewContextMenu model =
  case model.contextMenu of
    FieldContextMenu ->
      viewFieldContextMenu styles.button model
    GeneralContextMenu position ->
      viewGeneralContextMenu styles.button position
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
viewFieldContextMenu menuItemstyless model =
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
      menuItemstyless
      { onPress = Just DeleteActiveField
      , label = E.text "Delete"
      }
    , Input.button
      menuItemstyless
      { onPress = Just DuplicateActiveField
      , label = E.text "Duplicate"
      }
    , Input.button
      menuItemstyless
      { onPress = Just DeselectActiveField
      , label = E.text "Deselect"
      }
    ]


viewGeneralContextMenu : List (E.Attribute Msg) -> Position -> E.Element Msg
viewGeneralContextMenu menuItemstyless (x, y) =
  E.column
    [ E.moveRight x
    , E.moveDown y
    ]
    [ Input.button
      menuItemstyless
      { onPress = Just <| AddPositiveCharge (x, y)
      , label = E.text "Add + charge"
      }
    , Input.button
      menuItemstyless
      { onPress = Just <| AddNegativeCharge (x, y)
      , label = E.text "Add - charge"
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
          [ Attributes.id "activeSource"
          , Attributes.stroke <| Paint Color.lightGreen
          , Attributes.strokeWidth <| px 2.5
          ]
        else
          []
      Nothing ->
        []
    )
    [ Svg.animate
      [ Attributes.attributeName "stroke-opacity"
      , Attributes.animationValues [ 1, 0.3, 1 ]
      , Attributes.dur <| TypedSvg.Types.Duration "3s"
      , Attributes.repeatCount TypedSvg.Types.RepeatIndefinite
      ] []
    ]
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
  Html.Events.on "wheel" (Decode.map message (Decode.at ["deltaY"] Decode.int ))


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
  Html.Events.custom "contextmenu"
    (Decode.succeed
      { message = msg
      , stopPropagation = True
      , preventDefault = True
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


foldlWhile : (a -> b -> (b, Bool)) -> b -> List a -> b
foldlWhile accumulate initial list =
  let
    foldlHelper accumulated aList =
      case aList of
        head :: tail ->
          let
            (nextAccumulated, break) = accumulate head accumulated
          in
          if break then
            nextAccumulated
          else
            foldlHelper nextAccumulated tail
        [] ->
          accumulated
  in
  foldlHelper initial list


subscriptions : Model -> Sub Msg
subscriptions { drag } =
  Draggable.subscriptions DragMsg drag
