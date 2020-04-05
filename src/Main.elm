module Main exposing (main)

import Browser
import Html exposing (Html)
import Simulation
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Color
import Utils exposing (toElmUiColor, styles)

type alias Model =
  { simulations : List Simulation.Model
  , activeSimulation : Simulation.Model
  }


type Msg
  = UpdateActiveSimulationName String
  | ChangeActiveSimulation Simulation.Model
  | SimulationMsg Simulation.Msg



init : (List String) -> (Model, Cmd Msg)
init savedSimulations =
  let
    activeSimulation =
      Tuple.first <| Simulation.init <| List.head savedSimulations
    simulations =
      case savedSimulations of
        [] ->
          [ activeSimulation ]
        _ :: tail ->
          activeSimulation :: List.map
          (\simulation ->
            Tuple.first <| Simulation.init (Just simulation)
          )
          tail
  in
  ({ simulations =
    simulations
  , activeSimulation =
    activeSimulation
  }
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
  E.row [] <|
    List.map
      (\simulation ->
        if simulation == model.activeSimulation then
          Input.text
            [ E.width <| E.px 150
            , Background.color <| toElmUiColor Color.lightGrey
            ]
            { label = Input.labelHidden "current simulation name"
            , onChange = UpdateActiveSimulationName
            , placeholder = Nothing
            , text = simulation.name
            }
        else
          Input.button styles.button
            { onPress = Just <| ChangeActiveSimulation simulation
            , label = E.text simulation.name
            }
      )
      model.simulations


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    UpdateActiveSimulationName newName ->
      (updateActiveSimulationName newName model, Cmd.none)

    ChangeActiveSimulation newSimulation ->
      (changeActiveSimulation newSimulation model, Cmd.none)

    SimulationMsg msg ->
      updateActiveSimulationWithMsg msg model


updateActiveSimulationName : String -> Model -> Model
updateActiveSimulationName newName model =
  let
    oldActiveSimulation =
      model.activeSimulation
    newActiveSimulation =
      { oldActiveSimulation
        | name =
          if newName == "" then
            Simulation.defaultName
          else
            newName
      }
  in
  updateActiveSimulation newActiveSimulation model


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


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map SimulationMsg <| Simulation.subscriptions model.activeSimulation

  
main : Program (List String) Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }