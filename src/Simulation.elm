module Simulation exposing (Model, Msg, Position, Settings, Sign(..), State(..), addCharge, calculateFields, decodeModel, defaultName, defaultSettings, encodeModel, init, subscriptions, update, view)

import Browser.Events
import Color exposing (Color)
import Dict exposing (Dict)
import Dict.Extra
import Draggable
import Draggable.Events
import Element as E
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Json.Encode as Encode
import List.Extra
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Maybe.Extra
import Process
import Round
import Set exposing (Set)
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (Cursor(..), Paint(..), Transform(..), px)
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
    , state : State
    , width : Float
    , height : Float
    }


type State
    = Resting
    | Running


type alias Settings =
    { r : Float
    , density : Int
    , steps : Int
    , delta : Float
    , magnitude : Float
    , showSourceValue : Bool
    , colors : SettingColors
    }


type alias SettingColors =
    { positiveCharge : Color
    , negativeCharge : Color
    , positiveLine : Color
    , negativeLine : Color
    , background : Color
    }


type ContextMenu
    = FieldContextMenu
    | GeneralContextMenu Position
    | NoContextMenu


type alias Field =
    { source : Charge
    , density : Int
    , steps : Int
    , delta : Float
    , lines : List Line
    }


type alias Id =
    Int


type alias Line =
    ( Id, List Point, Maybe Id )


type alias Point =
    ( Float, Float )


type alias Position =
    Point


type Sign
    = Positive
    | Negative


type alias Charge =
    { id : Id
    , sign : Sign
    , magnitude : Float
    , position : Vec2
    , velocity : Vec2
    , r : Float
    }


defaultSettings : Settings
defaultSettings =
    { r = 15.0
    , density = 20
    , steps = 3000
    , delta = 2
    , magnitude = 1.0
    , showSourceValue = True
    , colors =
        { positiveCharge = Color.orange
        , negativeCharge = Color.blue
        , positiveLine = Color.black
        , negativeLine = Color.black
        , background = Color.white
        }
    }


defaultName : String
defaultName =
    "Untitled Model"


type Msg
    = OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg Id)
    | StartDragging Id
    | EndDragging
    | ActivateSource Id
    | ToggleSourceSign
    | ScaleSourceMagnitude Int
    | UpdateSourceCharge Sign
    | ShowFieldContextMenu
    | ShowGeneralContextMenu Mouse.Event
    | DeleteActiveField
    | ClickedBackground
    | DuplicateActiveField
    | DeselectActiveField
    | AddPositiveCharge Position
    | AddNegativeCharge Position
    | StopWheelingTimeOut
    | Step Float


init : Float -> Float -> Model
init width height =
    let
        defaultFields =
            [ { source = { id = 0, sign = Negative, magnitude = 3.0, position = vec2 465.0 270.0, velocity = vec2 0 0, r = defaultSettings.r }
              , density = defaultSettings.density
              , steps = defaultSettings.steps
              , delta = defaultSettings.delta
              , lines = []
              }
            , { source = { id = 1, sign = Positive, magnitude = 1.0, position = vec2 618.0 515.0, velocity = vec2 0 0, r = defaultSettings.r }
              , density = defaultSettings.density
              , steps = defaultSettings.steps
              , delta = defaultSettings.delta
              , lines = []
              }
            , { source = { id = 2, sign = Positive, magnitude = 10.0, position = vec2 553.0 338.0, velocity = vec2 0 0, r = defaultSettings.r }
              , density = defaultSettings.density
              , steps = defaultSettings.steps
              , delta = defaultSettings.delta
              , lines = []
              }
            , { source = { id = 3, sign = Negative, magnitude = 20.0, position = vec2 597.0 182.0, velocity = vec2 0 0, r = defaultSettings.r }
              , density = defaultSettings.density
              , steps = defaultSettings.steps
              , delta = defaultSettings.delta
              , lines = []
              }
            ]

        defaultModel =
            { name = defaultName
            , fields =
                calculateFields width height defaultFields
            , activeSourceId =
                if List.length defaultFields > 0 then
                    Just 0

                else
                    Nothing
            , nextId = List.length defaultFields
            , drag = Draggable.init
            , contextMenu = NoContextMenu
            , settings = defaultSettings
            , isWheeling = False
            , isWheelingTimeOutCleared = False
            , width = width
            , height = height
            , state = Resting
            }
    in
    defaultModel


countFieldLinesEndingWithChargeId : Id -> Field -> Int
countFieldLinesEndingWithChargeId id field =
    List.sum <|
        List.map
            (\( _, _, endChargeId ) ->
                if endChargeId == Just id then
                    1

                else
                    0
            )
            field.lines


leanifyFields : List Field -> List Field
leanifyFields fields =
    let
        sourceToDestination =
            findDuplicateFieldLines fields

        _ =
            Debug.log "sourceToDestination" sourceToDestination
    in
    List.map
        (\field ->
            case Dict.get field.source.id sourceToDestination of
                Just destinationIds ->
                    let
                        _ =
                            Debug.log "field" field.source.id

                        _ =
                            Debug.log "destinationIds" (Set.toList destinationIds)

                        _ =
                            Debug.log "field.lines" (List.map (\( _, _, end ) -> end) field.lines)
                    in
                    { field
                        | lines =
                            List.filter
                                (\( _, _, endChargeId ) ->
                                    case endChargeId of
                                        Just id ->
                                            not (Set.member id destinationIds)

                                        Nothing ->
                                            True
                                )
                                field.lines
                    }

                Nothing ->
                    field
        )
        fields


findDuplicateFieldLines : List Field -> Dict Id (Set Id)
findDuplicateFieldLines fields =
    Dict.Extra.fromListDedupe Set.union <|
        List.map
            (\( fieldA, fieldB ) ->
                let
                    chargeA =
                        fieldA.source.id

                    chargeB =
                        fieldB.source.id

                    numOfLinesFromAtoB =
                        countFieldLinesEndingWithChargeId chargeB fieldA

                    numOfLinesFromBtoA =
                        countFieldLinesEndingWithChargeId chargeA fieldB
                in
                if numOfLinesFromAtoB > numOfLinesFromBtoA then
                    ( chargeB, Set.singleton chargeA )

                else
                    ( chargeA, Set.singleton chargeB )
            )
            (List.Extra.uniquePairs fields)


calculateFields : Float -> Float -> List Field -> List Field
calculateFields width height fields =
    leanifyFields <|
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

                                    dx =
                                        field.source.r * cos angle

                                    dy =
                                        field.source.r * sin angle

                                    start =
                                        Vector2.add (vec2 dx dy) field.source.position
                                in
                                calculateFieldLine
                                    { charges = List.map .source fields
                                    , steps = field.steps
                                    , delta = field.delta
                                    , sourceSign = field.source.sign
                                    , startChargeId = field.source.id
                                    , start = ( Vector2.getX start, Vector2.getY start )
                                    , xBound = width * 1.5
                                    , yBound = height * 1.5
                                    }
                            )
                            (List.map toFloat <| List.range 0 (field.density - 1))
                in
                { field
                    | lines = lines
                }
            )
            fields


calculateFieldLine :
    { charges : List Charge
    , steps : Int
    , delta : Float
    , sourceSign : Sign
    , startChargeId : Id
    , start : Point
    , xBound : Float
    , yBound : Float
    }
    -> Line
calculateFieldLine { charges, steps, delta, sourceSign, startChargeId, start, xBound, yBound } =
    foldlWhile
        (\_ line ->
            let
                ( _, points, _ ) =
                    line

                ( x, y ) =
                    case points of
                        prev :: _ ->
                            prev

                        _ ->
                            ( 0, 0 )

                -- impossible
                previousPosition =
                    vec2 x y

                outOfBounds =
                    x > xBound || x < 0 || y > yBound || y < 0

                reachedAChargeWithId =
                    List.Extra.findMap
                        (\charge ->
                            if
                                charge.id
                                    /= startChargeId
                                    && Vector2.distance charge.position previousPosition
                                    <= 5
                            then
                                Just charge.id

                            else
                                Nothing
                        )
                        charges

                stopCalculation =
                    outOfBounds || Maybe.Extra.isJust reachedAChargeWithId

                netField =
                    if stopCalculation then
                        Vector2.vec2 0 0

                    else
                        List.foldl
                            (\charge sum ->
                                let
                                    d =
                                        Vector2.distance previousPosition charge.position / 100

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
                                                Vector2.sub previousPosition charge.position
                                in
                                Vector2.add sum field
                            )
                            (Vector2.vec2 0 0)
                            charges

                next =
                    if stopCalculation then
                        ( x, y )

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
                                     )
                                     <|
                                        Vector2.scale delta <|
                                            Vector2.normalize netField
                                    )
                        in
                        ( Vector2.getX vec, Vector2.getY vec )
            in
            ( ( startChargeId, next :: points, reachedAChargeWithId ), stopCalculation )
        )
        ( startChargeId, [ start ], Nothing )
        (List.range 0 (steps - 1))


distance : Point -> Point -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


dragConfig : Draggable.Config Id Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy OnDragBy
        , Draggable.Events.onDragStart StartDragging
        , Draggable.Events.onDragEnd EndDragging
        , Draggable.Events.onClick ActivateSource
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnDragBy offsetPos ->
            ( onDragBy offsetPos model, Cmd.none )

        StartDragging id ->
            ( startDragging id model, Cmd.none )

        EndDragging ->
            ( endDragging model, Cmd.none )

        ActivateSource id ->
            ( setActiveSourceId id model, Cmd.none )

        ToggleSourceSign ->
            ( toggleSourceSign model, Cmd.none )

        ScaleSourceMagnitude delta ->
            scaleSourceMagnitude delta model

        UpdateSourceCharge sign ->
            updateSourceCharge sign model

        StopWheelingTimeOut ->
            ( stopWheelingTimeOut model, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        ShowFieldContextMenu ->
            ( showFieldContextMenu model, Cmd.none )

        ShowGeneralContextMenu { offsetPos } ->
            ( showGeneralContextMenu offsetPos model, Cmd.none )

        DeleteActiveField ->
            ( deleteActiveField model, Cmd.none )

        ClickedBackground ->
            ( resetState model, Cmd.none )

        DuplicateActiveField ->
            ( duplicateActiveField model, Cmd.none )

        DeselectActiveField ->
            ( deselectActiveField model, Cmd.none )

        AddPositiveCharge position ->
            ( addCharge Positive position model, Cmd.none )

        AddNegativeCharge position ->
            ( addCharge Negative position model, Cmd.none )

        Step delta ->
            ( step delta model, Cmd.none )


onDragBy : Position -> Model -> Model
onDragBy offsetPos model =
    let
        newFields =
            updateActive (dragSource offsetPos) model.activeSourceId model.fields
    in
    { model
        | fields =
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

        encodeVector2 : Vec2 -> Encode.Value
        encodeVector2 vec =
            Encode.object
                [ ( "x", Encode.float <| Vector2.getX vec )
                , ( "y", Encode.float <| Vector2.getY vec )
                ]

        encodeCharge : Charge -> Encode.Value
        encodeCharge { id, sign, magnitude, position, r } =
            Encode.object
                [ ( "id", Encode.int id )
                , ( "sign", encodeSign sign )
                , ( "magnitude", Encode.float magnitude )
                , ( "position", encodeVector2 position )
                , ( "r", Encode.float r )
                ]

        encodeField : Field -> Encode.Value
        encodeField { source, density, steps, delta } =
            Encode.object
                [ ( "source", encodeCharge source )
                , ( "density", Encode.int density )
                , ( "steps", Encode.int steps )
                , ( "delta", Encode.float delta )
                ]

        encodeColor : Color -> Encode.Value
        encodeColor color =
            let
                { red, green, blue, alpha } =
                    Color.toRgba color
            in
            Encode.object
                [ ( "red", Encode.float red )
                , ( "green", Encode.float green )
                , ( "blue", Encode.float blue )
                , ( "alpha", Encode.float alpha )
                ]

        encodeSettingColors : SettingColors -> Encode.Value
        encodeSettingColors { positiveCharge, negativeCharge, positiveLine, negativeLine, background } =
            Encode.object
                [ ( "positiveCharge", encodeColor positiveCharge )
                , ( "negativeCharge", encodeColor negativeCharge )
                , ( "positiveLine", encodeColor positiveLine )
                , ( "negativeLine", encodeColor negativeLine )
                , ( "background", encodeColor background )
                ]

        encodeSettings : Settings -> Encode.Value
        encodeSettings { magnitude, r, density, steps, delta, showSourceValue, colors } =
            Encode.object
                [ ( "magnitude", Encode.float magnitude )
                , ( "r", Encode.float r )
                , ( "density", Encode.int density )
                , ( "steps", Encode.int steps )
                , ( "delta", Encode.float delta )
                , ( "showSourceValue", Encode.bool showSourceValue )
                , ( "colors", encodeSettingColors colors )
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
        [ ( "name", Encode.string name )
        , ( "fields", Encode.list encodeField fields )
        , ( "activeSourceId", encodeMaybeId activeSourceId )
        , ( "nextId", Encode.int nextId )
        , ( "settings", encodeSettings settings )
        , ( "width", Encode.float width )
        , ( "height", Encode.float height )
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

        decodeVector2 =
            Field.require "x" Decode.float <|
                \x ->
                    Field.require "y" Decode.float <|
                        \y ->
                            Decode.succeed <|
                                vec2 x y

        decodeCharge =
            Field.require "id" Decode.int <|
                \id ->
                    Field.require "sign" decodeSign <|
                        \sign ->
                            Field.require "magnitude" Decode.float <|
                                \magnitude ->
                                    Field.require "position" decodeVector2 <|
                                        \position ->
                                            Field.require "r" Decode.float <|
                                                \r ->
                                                    Decode.succeed
                                                        { id = id
                                                        , sign = sign
                                                        , magnitude = magnitude
                                                        , position = position
                                                        , velocity = vec2 0 0
                                                        , r = r
                                                        }

        decodeField =
            Field.require "source" decodeCharge <|
                \source ->
                    Field.require "density" Decode.int <|
                        \density ->
                            Field.require "steps" Decode.int <|
                                \steps ->
                                    Field.require "delta" Decode.float <|
                                        \delta ->
                                            Decode.succeed
                                                { source = source
                                                , density = density
                                                , steps = steps
                                                , delta = delta
                                                , lines = []
                                                }

        decodeColorRgba =
            Field.require "red" Decode.float <|
                \red ->
                    Field.require "green" Decode.float <|
                        \green ->
                            Field.require "blue" Decode.float <|
                                \blue ->
                                    Field.require "alpha" Decode.float <|
                                        \alpha ->
                                            Decode.succeed <|
                                                Color.fromRgba
                                                    { red = red
                                                    , green = green
                                                    , blue = blue
                                                    , alpha = alpha
                                                    }

        decodeSettingColors =
            Field.require "positiveCharge" decodeColorRgba <|
                \positiveCharge ->
                    Field.require "negativeCharge" decodeColorRgba <|
                        \negativeCharge ->
                            Field.require "positiveLine" decodeColorRgba <|
                                \positiveLine ->
                                    Field.require "negativeLine" decodeColorRgba <|
                                        \negativeLine ->
                                            Field.require "background" decodeColorRgba <|
                                                \background ->
                                                    Decode.succeed
                                                        { positiveCharge = positiveCharge
                                                        , negativeCharge = negativeCharge
                                                        , positiveLine = positiveLine
                                                        , negativeLine = negativeLine
                                                        , background = background
                                                        }

        decodeSettings =
            Field.require "r" Decode.float <|
                \r ->
                    Field.require "magnitude" Decode.float <|
                        \magnitude ->
                            Field.require "density" Decode.int <|
                                \density ->
                                    Field.require "steps" Decode.int <|
                                        \steps ->
                                            Field.require "delta" Decode.float <|
                                                \delta ->
                                                    Field.require "showSourceValue" Decode.bool <|
                                                        \showSourceValue ->
                                                            Field.require "colors" decodeSettingColors <|
                                                                \colors ->
                                                                    Decode.succeed
                                                                        { r = r
                                                                        , magnitude = magnitude
                                                                        , density = density
                                                                        , steps = steps
                                                                        , delta = delta
                                                                        , showSourceValue = showSourceValue
                                                                        , colors = colors
                                                                        }
    in
    Field.require "name" Decode.string <|
        \name ->
            Field.require "fields" (Decode.list decodeField) <|
                \fields ->
                    Field.attempt "activeSourceId" Decode.int <|
                        \activeSourceId ->
                            Field.require "nextId" Decode.int <|
                                \nextId ->
                                    Field.require "settings" decodeSettings <|
                                        \settings ->
                                            Field.require "width" Decode.float <|
                                                \width ->
                                                    Field.require "height" Decode.float <|
                                                        \height ->
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
                                                                , state = Resting
                                                                }


setActiveSourceId : Id -> Model -> Model
setActiveSourceId id model =
    { model
        | activeSourceId = Just id
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
                    { field
                        | source =
                            { source
                                | sign =
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


scaleSourceMagnitude : Int -> Model -> ( Model, Cmd Msg )
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
                    { field
                        | source =
                            { source
                                | magnitude =
                                    min 20 <| max 0.5 <| source.magnitude + either -0.5 0.5 (toFloat delta * -0.01)
                            }
                    }
                )
                newModel.activeSourceId
                newModel.fields
    in
    ( { model
        | fields =
            calculateFields model.width model.height newFields
        , isWheeling =
            True
        , isWheelingTimeOutCleared =
            True
      }
    , setTimeOut 200 StopWheelingTimeOut
    )


updateSourceCharge : Sign -> Model -> ( Model, Cmd Msg )
updateSourceCharge deltaDirection model =
    let
        newFields =
            updateActive
                (\field ->
                    let
                        source =
                            field.source

                        delta =
                            case deltaDirection of
                                Positive ->
                                    0.5

                                Negative ->
                                    -0.5

                        newMagnitude =
                            source.magnitude
                                + (case source.sign of
                                    Positive ->
                                        delta

                                    Negative ->
                                        -delta
                                  )
                    in
                    { field
                        | source =
                            { source
                                | magnitude = min 20 <| max 0.5 <| newMagnitude
                                , sign =
                                    if abs newMagnitude < 0.5 then
                                        flipSign source.sign

                                    else
                                        source.sign
                            }
                    }
                )
                model.activeSourceId
                model.fields
    in
    ( { model
        | fields =
            calculateFields model.width model.height newFields
      }
    , Cmd.none
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
        deoptimizeModel
            { model
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
    { model
        | contextMenu =
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
    { model
        | fields =
            calculateFields model.width model.height newFields
        , contextMenu =
            NoContextMenu
        , activeSourceId =
            Nothing
    }


addCharge : Sign -> Position -> Model -> Model
addCharge sign ( x, y ) model =
    let
        newCharge : Charge
        newCharge =
            { sign = sign
            , magnitude = model.settings.magnitude
            , position = vec2 x y
            , velocity = vec2 0 0
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


step : Float -> Model -> Model
step _ model =
    case model.state of
        Running ->
            move model

        Resting ->
            model


move : Model -> Model
move model =
    let
        fields =
            model.fields

        newFields =
            List.map
                (\field ->
                    let
                        force =
                            calculateElectricForce field (List.filter ((/=) field) fields)

                        newAcceleration =
                            force

                        newVelocity =
                            Vector2.toRecord <| Vector2.add newAcceleration field.source.velocity

                        newPosition =
                            Vector2.toRecord <| Vector2.add (Vector2.fromRecord newVelocity) field.source.position

                        r =
                            field.source.r

                        width =
                            model.width

                        height =
                            model.height

                        newX =
                            if newPosition.x < r then
                                r

                            else if newPosition.x > width - r then
                                width - r

                            else
                                newPosition.x

                        newY =
                            if newPosition.y < r then
                                r

                            else if newPosition.y > height - r then
                                height - r

                            else
                                newPosition.y

                        newXVelocity =
                            if newPosition.x < r || newPosition.x > width - r then
                                -1 * newVelocity.x

                            else
                                newVelocity.x

                        newYVelocity =
                            if newPosition.y < r || newPosition.y > height - r then
                                -1 * newVelocity.y

                            else
                                newVelocity.y

                        source =
                            field.source
                    in
                    { field
                        | source =
                            { source
                                | position =
                                    vec2 newX newY
                                , velocity =
                                    vec2 newXVelocity newYVelocity
                            }
                    }
                )
                fields
    in
    { model
        | fields =
            calculateFields model.width model.height newFields
    }


calculateElectricForce : Field -> List Field -> Vec2
calculateElectricForce self rests =
    List.foldl
        (\other netForce ->
            let
                d =
                    Vector2.distance self.source.position other.source.position / 100

                forceMagnitude =
                    if d == 0 then
                        0.5

                    else
                        min 2 <| self.source.magnitude * other.source.magnitude / d ^ 2

                forceDirection =
                    if d == 0 then
                        self.source.velocity

                    else
                        Vector2.normalize <| Vector2.sub self.source.position other.source.position

                force =
                    Vector2.scale (sign * forceMagnitude) forceDirection

                sign =
                    case ( self.source.sign, other.source.sign ) of
                        ( Positive, Negative ) ->
                            -1

                        ( Negative, Positive ) ->
                            -1

                        _ ->
                            1
            in
            Vector2.add force netForce
        )
        (Vector2.vec2 0 0)
        rests


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
                    { field
                        | source =
                            { source
                                | position =
                                    Vector2.add (vec2 (source.r * 2 + 15) 0) source.position
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
dragSource ( dx, dy ) field =
    let
        source =
            field.source
    in
    { field
        | source =
            { source
                | position =
                    Vector2.add (Vector2.vec2 dx dy) source.position
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
        ]
    <|
        E.el
            [ E.inFront <| viewContextMenu model
            , E.centerX
            , E.centerY
            , E.paddingXY 0 5
            ]
            (E.html <|
                Svg.svg
                    [ Attributes.width (px model.width)
                    , Attributes.height (px model.height)
                    , Attributes.viewBox 0 0 model.width model.height
                    , Attributes.id "modelSvg"
                    , Mouse.onContextMenu ShowGeneralContextMenu
                    ]
                <|
                    viewBackground model.width model.height model.settings.colors.background
                        :: List.map (viewFieldLines model.settings) model.fields
                        ++ List.map (viewFieldSource model.activeSourceId model.settings) model.fields
            )


viewBackground : Float -> Float -> Color -> Svg Msg
viewBackground width height color =
    Svg.rect
        [ Attributes.width <| px width
        , Attributes.height <| px height
        , Attributes.fill <| Paint color
        ]
        []


viewContextMenu : Model -> E.Element Msg
viewContextMenu model =
    case model.contextMenu of
        FieldContextMenu ->
            viewFieldContextMenu styles.button model

        GeneralContextMenu position ->
            viewGeneralContextMenu styles.button position

        NoContextMenu ->
            -- very weired. Should be `E.none` but need below for buttons to be styled correctly
            E.el [ E.htmlAttribute <| Html.Attributes.style "display" "none" ] <|
                viewFieldContextMenu styles.button model


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
        ( x, y ) =
            case List.head <| getActiveFields model of
                Just field ->
                    ( Vector2.getX field.source.position, Vector2.getY field.source.position )

                Nothing ->
                    ( 0, 0 )

        -- impossible
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
viewGeneralContextMenu menuItemstyless ( x, y ) =
    E.column
        [ E.moveRight x
        , E.moveDown y
        ]
        [ Input.button
            menuItemstyless
            { onPress = Just <| AddPositiveCharge ( x, y )
            , label = E.text "Add + charge"
            }
        , Input.button
            menuItemstyless
            { onPress = Just <| AddNegativeCharge ( x, y )
            , label = E.text "Add - charge"
            }
        ]


viewFieldSource : Maybe Id -> Settings -> Field -> Svg Msg
viewFieldSource activeSourceId settings field =
    let
        fill =
            case field.source.sign of
                Positive ->
                    settings.colors.positiveCharge

                Negative ->
                    settings.colors.negativeCharge

        gradientId =
            "gradient" ++ String.fromInt field.source.id

        x =
            Vector2.getX field.source.position

        y =
            Vector2.getY field.source.position
    in
    Svg.g []
        [ Svg.defs []
            [ Svg.radialGradient
                [ Attributes.id <| gradientId ]
                [ Svg.stop
                    [ Attributes.offset "1%"
                    , Attributes.stopColor <| Color.toCssString <| setAlpha 1 fill
                    ]
                    []
                , Svg.stop
                    [ Attributes.offset "100%"
                    , Attributes.stopColor <| Color.toCssString <| setAlpha 0.2 fill
                    ]
                    []
                ]
            ]
        , Svg.circle
            [ Attributes.cx (px x)
            , Attributes.cy (px y)
            , Attributes.r (px <| lerp 0 20 10 40 (min 20 field.source.r * field.source.magnitude / 10))
            , Attributes.fill <| Reference gradientId
            , Html.Attributes.style "pointer-events" "none"
            ]
            []
        , Svg.circle
            ([ Attributes.cx (px x)
             , Attributes.cy (px y)
             , Attributes.r (px field.source.r)
             , Attributes.fill <| Paint fill
             , Draggable.mouseTrigger field.source.id DragMsg
             , onWheel ScaleSourceMagnitude
             , Html.Events.onDoubleClick ToggleSourceSign
             , onRightClick ShowFieldContextMenu
             ]
                ++ Draggable.touchTriggers field.source.id DragMsg
                ++ (case activeSourceId of
                        Just id ->
                            if field.source.id == id then
                                [ Attributes.id "activeSource"
                                , Attributes.stroke <| Paint Color.lightGreen
                                , Attributes.strokeWidth <| px 5
                                ]

                            else
                                []

                        Nothing ->
                            []
                   )
            )
            [ Svg.animate
                [ Attributes.attributeName "stroke-opacity"
                , Attributes.animationValues [ 1, 0.3, 1 ]
                , Attributes.dur <| TypedSvg.Types.Duration "3s"
                , Attributes.repeatCount TypedSvg.Types.RepeatIndefinite
                ]
                []
            ]
        , case activeSourceId of
            Just id ->
                let
                    tooltipWidth =
                        100

                    tooltipHeight =
                        30
                in
                if field.source.id == id && settings.showSourceValue then
                    Svg.g
                        [ Attributes.transform
                            [ Translate (x - tooltipWidth / 2) (y - field.source.r - tooltipHeight - 10)
                            ]
                        ]
                        [ Svg.rect
                            [ Attributes.x <| px 0
                            , Attributes.y <| px 0
                            , Attributes.width <| px tooltipWidth
                            , Attributes.height <| px tooltipHeight
                            , Attributes.fill <| Paint Color.lightGrey
                            ]
                            []
                        , Svg.text_
                            [ Attributes.x (px <| 10)
                            , Attributes.y (px <| 20)
                            , Attributes.stroke <| Paint Color.black
                            , Attributes.cursor CursorPointer
                            , TypedSvg.Events.onClick (UpdateSourceCharge Negative)
                            ]
                            [ TypedSvg.Core.text "<"
                            ]
                        , Svg.text_
                            [ Attributes.x
                                (px <|
                                    if field.source.magnitude < 10 then
                                        30

                                    else
                                        25
                                )
                            , Attributes.y (px <| 20)
                            , Attributes.stroke <| Paint Color.black
                            , Attributes.id "sourceValueLabel"
                            , Attributes.cursor CursorDefault
                            ]
                            [ TypedSvg.Core.text (signToString field.source.sign ++ Round.round 1 field.source.magnitude)
                            ]
                        , Svg.text_
                            [ Attributes.x (px <| tooltipWidth - 20)
                            , Attributes.y (px <| 20)
                            , Attributes.stroke <| Paint Color.black
                            , Attributes.cursor CursorPointer
                            , TypedSvg.Events.onClick (UpdateSourceCharge Positive)
                            ]
                            [ TypedSvg.Core.text ">"
                            ]
                        ]

                else
                    Svg.g [] []

            Nothing ->
                Svg.g [] []
        ]


viewFieldLines : Settings -> Field -> Svg Msg
viewFieldLines settings field =
    let
        lineColor =
            case field.source.sign of
                Positive ->
                    settings.colors.positiveLine

                Negative ->
                    settings.colors.negativeLine
    in
    Svg.g [] <|
        List.map
            (\( _, line, _ ) ->
                Svg.polyline
                    [ Attributes.fill PaintNone, Attributes.stroke <| Paint lineColor, Attributes.points line ]
                    []
            )
            field.lines


onWheel : (Int -> msg) -> Html.Attribute msg
onWheel message =
    Html.Events.on "wheel" (Decode.map message (Decode.at [ "deltaY" ] Decode.int))


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    Html.Events.custom "contextmenu"
        (Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


signToString : Sign -> String
signToString sign =
    case sign of
        Positive ->
            "+"

        Negative ->
            "-"


flipSign : Sign -> Sign
flipSign sign =
    case sign of
        Positive ->
            Negative

        Negative ->
            Positive


setAlpha : Float -> Color -> Color
setAlpha alpha color =
    let
        rgba =
            Color.toRgba color
    in
    Color.fromRgba <|
        { rgba
            | alpha = alpha
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


either : Float -> Float -> Float -> Float
either minimum maximum value =
    if value < 0 then
        minimum

    else
        maximum


foldlWhile : (a -> b -> ( b, Bool )) -> b -> List a -> b
foldlWhile accumulate initial list =
    let
        foldlHelper accumulated aList =
            case aList of
                head :: tail ->
                    let
                        ( nextAccumulated, break ) =
                            accumulate head accumulated
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
    Sub.batch
        [ Draggable.subscriptions DragMsg drag
        , Browser.Events.onAnimationFrameDelta Step
        ]
