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

type alias Model =
  { fields : List Field
  , activeSourceId : Id
  , drag : Draggable.State Id
  }

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

initialModel : () -> (Model, Cmd Msg)
initialModel _ =
  let
    fields =
      [{ source = { id = 0, sign = Negative, magnitude = 3.0, x = 300.0, y = 350.0, r = 10.0 }
      , density = 30
      , steps = 450
      , delta = 2
      , lines = []
      }
      , { source = { id = 1, sign = Positive, magnitude = 1.0, x = 400.0, y = 350.0, r = 10.0 }
      , density = 30
      , steps = 450
      , delta = 2
      , lines = []
      }
      , { source = { id = 2, sign = Positive, magnitude = 10.0, x = 300.0, y = 450.0, r = 10.0 }
      , density = 30
      , steps = 450
      , delta = 2
      , lines = []
      }
      , { source = { id = 3, sign = Negative, magnitude = 20.0, x = 400.0, y = 450.0, r = 10.0 }
      , density = 30
      , steps = 450
      , delta = 2
      , lines = []
      }
      ]
  in
  ({ fields =
    calculateFields fields
  , activeSourceId = 0
  , drag = Draggable.init
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
      ( { model |
        activeSourceId = id
      }
      , Cmd.none
      )

    ActivateSource id ->
      ( { model
        | activeSourceId = id
      }
      , Cmd.none
      )

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


updateActive : (Field -> Field) -> Id -> List Field -> List Field
updateActive func id fields =
  List.map
    (\field ->
      if field.source.id == id then
        func field
      else
        field
    )
    fields


dragSource : (Float, Float) -> Field -> Field
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
  Svg.svg
    [ Attributes.width (px 1000)
    , Attributes.height (px 780)
    , Attributes.viewBox 0 0 1000 780
    , Html.Attributes.style "display" "block"
    , Html.Attributes.style "margin" "auto"
    ] <|
  List.map viewFieldLines model.fields
  ++ List.map (viewFieldSource model.activeSourceId) model.fields


viewFieldSource : Id -> Field -> Svg Msg
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
    ] ++ Draggable.touchTriggers field.source.id DragMsg
    ++ if field.source.id == activeSourceId then
        [ Attributes.stroke <| Paint Color.lightGreen
        , Attributes.strokeWidth <| px 2.5
        ]
      else
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