port module Main exposing (main)

import Browser
import Html exposing (Html)
import Simulation
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import List.Extra
import Utils exposing (styles, colors, centeredText)


port pageWillClose : (() -> msg) -> Sub msg
port saveProject : Encode.Value -> Cmd msg


type alias Model =
  { simulations : List Simulation.Model
  , activeSimulation : Simulation.Model
  , defaultSimulationIndex : Int
  }


type Msg
  = UpdateActiveSimulationName String
  | ChangeActiveSimulation Simulation.Model
  | SimulationMsg Simulation.Msg
  | AddSimulation
  | RemoveSimulation Simulation.Model
  | SaveProject


init : (Maybe String) -> (Model, Cmd Msg)
init savedProject =
  let
    project =
      case savedProject of
        Just projectJson ->
          Result.withDefault defaultProject <| Decode.decodeString decodeProject projectJson
        Nothing ->
          defaultProject
    
    defaultActiveSimulation =
      let
        simulation =
          Simulation.init
      in
      if simulation.name == Simulation.defaultName then
        { simulation
          | name =
            getDefaultSimulationName 1
        }
      else
        simulation
    
    defaultSimulations =
      [ defaultActiveSimulation ]
    
    defaultProject =
      { simulations =
        defaultSimulations
      , activeSimulation =
        defaultActiveSimulation
      , defaultSimulationIndex =
        1
      }
  in
  ( project
  , Cmd.none
  )


view : Model -> Html Msg
view model =
  E.layout
    [ E.width E.fill
    , E.height E.fill
    , Font.size 16
    , Font.family
      [ Font.monospace
      ]
    ] <|
    E.el
      [ E.above <| viewTabs model
      , E.centerX
      , E.centerY
      ]
      (E.html (Html.map SimulationMsg <| Simulation.view model.activeSimulation))


viewTabs : Model -> E.Element Msg
viewTabs model =
  E.row
    [ E.spacing 10
    ] <|
    List.map
      (\simulation ->
        if simulation == model.activeSimulation then
          Input.text
            ( styles.tab
              ++ [ Background.color colors.white
              , E.padding 15
              , E.inFront <| viewCloseTabButton simulation
              ]
            )
            { label = Input.labelHidden "current simulation name"
            , onChange = UpdateActiveSimulationName
            , placeholder = Nothing
            , text = simulation.name
            }
        else
          Input.button styles.tab
            { onPress = Just <| ChangeActiveSimulation simulation
            , label = E.el [ E.padding 15, E.inFront <| viewCloseTabButton simulation, E.alignLeft ] <| E.text simulation.name
            }
      )
      model.simulations
    ++ [ viewAddTab ]


viewCloseTabButton : Simulation.Model -> E.Element Msg
viewCloseTabButton target =
  Input.button
    [ E.mouseOver
      [ Background.color colors.lightGrey ]
    , E.paddingXY 3 3
    , E.centerY, E.alignRight
    , Border.rounded 10
    ]
    { onPress =
      Just <| RemoveSimulation target
    , label =
      centeredText "x"
    }


viewAddTab : E.Element Msg
viewAddTab =
  Input.button (styles.button ++ [ E.width <| E.px 20, Border.rounded 20 ])
    { onPress =
      Just AddSimulation
    , label =
      centeredText "+"
    }


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    UpdateActiveSimulationName newName ->
      (updateActiveSimulationName newName model, Cmd.none)

    ChangeActiveSimulation newSimulation ->
      (changeActiveSimulation newSimulation model, Cmd.none)

    SimulationMsg msg ->
      updateActiveSimulationWithMsg msg model

    AddSimulation ->
      (addSimulation model, Cmd.none)

    RemoveSimulation target ->
      (removeSimulation target model, Cmd.none)

    SaveProject ->
      ( model
      , saveProject <| encodeProject model
      )


updateActiveSimulationName : String -> Model -> Model
updateActiveSimulationName newName model =
  let
    oldActiveSimulation =
      model.activeSimulation
    newActiveSimulation =
      { oldActiveSimulation
        | name =
          if newName == "" then
            getDefaultSimulationName model.defaultSimulationIndex
          else
            newName
      }
  in
  updateActiveSimulation newActiveSimulation model


getDefaultSimulationName : Int -> String
getDefaultSimulationName index =
  Simulation.defaultName ++ " " ++ String.fromInt index


changeActiveSimulation : Simulation.Model -> Model -> Model
changeActiveSimulation newSimulation model =
  { model
    | activeSimulation =
      newSimulation
  }


updateActiveSimulationWithMsg : Simulation.Msg -> Model -> (Model, Cmd Msg)
updateActiveSimulationWithMsg msg model =
  let
    (newSimulation, cmd) =
      Simulation.update msg model.activeSimulation
  in
  ( updateActiveSimulation newSimulation model
  , Cmd.map SimulationMsg cmd
  )


updateActiveSimulation : Simulation.Model -> Model -> Model
updateActiveSimulation newActiveSimulation model =
 { model
  | activeSimulation =
    newActiveSimulation
  , simulations =
      List.map
        (\simulation ->
          if simulation == model.activeSimulation then
            newActiveSimulation
          else
            simulation
        )
      model.simulations
 }


addSimulation : Model -> Model
addSimulation model =
  let
    newDefaultSimulationIndex =
      model.defaultSimulationIndex + 1

    defaultSimulationName =
      getDefaultSimulationName <| newDefaultSimulationIndex
    
    newSimulation =
      let
        simulation =
          Simulation.init
      in
      { simulation
        | name =
          defaultSimulationName
      }
  in
  { model
    | simulations =
      model.simulations ++ [ newSimulation ]
    , activeSimulation =
      newSimulation
    , defaultSimulationIndex =
      newDefaultSimulationIndex
  }


removeSimulation : Simulation.Model -> Model -> Model
removeSimulation target model =
  if List.length model.simulations == 1 then
    model
  else
    let
      updatedSimulations =
        List.filter
          (\simulation ->
            simulation /= target
          )
          model.simulations
      updatedModel =
        { model
          | simulations =
            updatedSimulations
          , defaultSimulationIndex =
            if String.startsWith Simulation.defaultName target.name then
              model.defaultSimulationIndex - 1
            else
              model.defaultSimulationIndex
        }
    in
    if target == model.activeSimulation then
      { updatedModel
        | activeSimulation =
          getNextSimulation target model.simulations
      }
    else
      updatedModel


getNextSimulation : Simulation.Model -> List Simulation.Model -> Simulation.Model
getNextSimulation current simulations =
  let
    splits =
      List.Extra.splitWhen ((==) current) simulations
    next =
      case splits of
        Just (firstToPrevious, currentToLast) ->
          if List.length currentToLast == 1 then -- current is the last simulation
            -- try getting the previous simulation
            Maybe.withDefault current <| List.Extra.last <| firstToPrevious
          else
          -- try getting the next simulation
            Maybe.withDefault current <| List.head <| List.Extra.removeAt 0 currentToLast
        Nothing ->
          current -- impossible
  in
  next


encodeProject : Model -> Encode.Value
encodeProject model =
  Encode.object
    [ ("simulations", Encode.list Simulation.encodeModel model.simulations)
    , ("activeSimulation", Simulation.encodeModel model.activeSimulation)
    , ("defaultSimulationIndex", Encode.int model.defaultSimulationIndex)
    ]



decodeProject : Decoder Model
decodeProject =
  Field.require "simulations" (Decode.list Simulation.decodeModel) <| \simulations ->
  Field.require "activeSimulation" Simulation.decodeModel <| \activeSimulation ->
  Field.require "defaultSimulationIndex" Decode.int <| \defaultSimulationIndex ->

  Decode.succeed
    { simulations = simulations
    , activeSimulation = activeSimulation
    , defaultSimulationIndex = defaultSimulationIndex
    }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map SimulationMsg <| Simulation.subscriptions model.activeSimulation
    , pageWillClose (\_ -> SaveProject)
    ]

  
main : Program (Maybe String) Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }