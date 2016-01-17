module FiniteAutomaton where

import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Json.Decode as JsDec
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Shorthand.Event exposing (..)
--import Html.Events.Extra exposing (..)
import StartApp.Simple as StartApp
import String exposing (..)

-- MODEL

{-
A deterministic finite automaton M is a 5-tuple, (Q, Σ, δ, q_0, F), 
consisting of 
    - a finite set of states Q
    - a finite set of input symbols called the alphabet Σ
    - a transition function δ : Q × Σ -> Q
    - an initial or start state q_0 ∈ Q
    - a set of accept states F ⊆ Q

Let w = a_1a_2...a_n be a string over the alphabet Σ. The automaton M 
accepts w if a sequence of states r_0,r_1,...,r_n exists in Q such that:
    1. r_0 = q_0
    2. r_{i+1} = δ(r_i, a_{i+1}), for i = 0,...,n-1
    3. r_n ∈ F                                                            
-}

type alias State = String

type alias Symbol = Char

type alias Model =
    { states : List State
    , alphabet : List Symbol
    , transitionFunction : Dict (State, Symbol) State  
    , startState : Maybe State
    , acceptStates : List State
    -- text boxes for adding
    , stateAdderValue : String
    , symbolAdderValue : String
    , transitionFunctionAdderValue :
        { fromState : Maybe State
        , symbol : Maybe Symbol
        , toState : Maybe State
        }
    }

initialModel : Model
initialModel =
    { states = []
    , alphabet = []
    , transitionFunction = Dict.empty
    , startState = Nothing
    , acceptStates = []
    , stateAdderValue = ""
    , symbolAdderValue = ""
    , transitionFunctionAdderValue =
        { fromState = Nothing
        , symbol = Nothing
        , toState = Nothing
        }
    }

-- View

view : Signal.Address Action -> Model -> Html
view addr model =
--   text "<~ ~ ~ Automata run ~ ~ ~ Build a finite automaton! ~ ~ ~>"
    let
        d1 = Debug.log "start state: " model.startState
        d2 = Debug.log "transition function " model.transitionFunctionAdderValue
    in
    div
        []
        [ p
            []
            [ text "<~ ~ ~ Automata run ~ ~ ~ Build a finite automaton! ~ ~ ~>"
            ]
        , p
            []
            [ text "The Alphabet"
            ]
        , text "Add symbols to the alphabet "
        , input
            [ type' "text"
            , placeholder "Enter a character"
            , onInput addr (\text -> if String.length text <= 1 then UpdateSymbolAdderValue text else NoOp)
            , value model.symbolAdderValue
            ]
            []
        , button
            [ onClick addr AddSymbol
            , disabled (not <| validSymbol model.symbolAdderValue model)
            ]
            [ text "Add" ]
        , ul
            []
            (List.map (viewSymbol addr) model.alphabet)
        , p
            []
            [ text "The States"
            ]
        , text "Add states "
        , input
            [ type' "text"
            , placeholder "Enter a name"
            , onInput addr (\text -> UpdateStateAdderValue text)
            , value model.stateAdderValue
            ]
            []
        , button
            [ onClick addr AddState
            , disabled (not <| validState model.stateAdderValue model)
            ]
            [ text "Add" ]
        , ul
            []
            (List.map ((viewState addr) model) model.states)
        , text <| "Start state: " ++ toString model.startState ++ ", Accept states: " ++ toString model.acceptStates
        , p
            []
            [ text "The Transition Function"
            ]
        , text "Add a transition from ( "
        , select
            [ onChange
                (JsDec.at ["target", "value"] JsDec.string)
                (\str -> maybeString str |> UpdateTransitionFunctionAdderFromState |> Signal.message addr)
            ]
            (option [ selected False, value "" ] [text "select a state"] :: (model.states 
                |> List.map
                (\st ->
                    option
                        [ selected False ]
                        [ text st ]

                )
            ))
        , text " , "
        , select
            [ onChange
                (JsDec.at ["target", "value"] JsDec.string)
                (\str -> maybeChar str |> UpdateTransitionFunctionAdderSymbol |> Signal.message addr)
            ]
            (option [ selected False, value "" ] [text "select a symbol"] :: (model.alphabet
                |> List.map
                (\al ->
                    option
                        [ selected False ]
                        [ text (String.fromChar al) ]
                )
            ))
        , text " ) to ( "
        , select
            [
                onChange
                    (JsDec.at ["target", "value"] JsDec.string)
                    (\str -> maybeString str |> UpdateTransitionFunctionAdderToState |> Signal.message addr)
            ]
            (option [ selected False, value "" ] [text "select a state"] :: (model.states
                |> List.map
                (\st ->
                    option
                        [ selected False ]
                        [ text st]
                )
            ))
        , text " ) "
        , button
            [ onClick addr NoOp ]
            [ text "Add" ]
        ]

maybeChar : String -> Maybe Char
maybeChar str =
    let maybe = String.uncons str
    in
       case maybe of
           Just (char, _) ->
               Just char
           Nothing ->
               Nothing

maybeString : String -> Maybe String
maybeString str =
    if (String.length str > 0) then Just str else Nothing

getMaybe : Maybe a -> a
getMaybe maybe =
    case maybe of
        Just x ->
            x
        Nothing ->
            Debug.crash "Tried to get a Maybe that was Nothing"

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
        on "input" targetValue (\str -> Signal.message address (contentToValue str))

validSymbol : String -> Model -> Bool
validSymbol text model =
    let maybeSymbol = String.uncons text
    in
       case maybeSymbol of
           Just (char, _) ->
               not <| List.member char model.alphabet
           Nothing ->
               False

viewSymbol : Signal.Address Action -> Symbol -> Html
viewSymbol addr symbol =
    li
        []
        [ text (String.fromChar symbol)
        , text " "
        , button
            [ onClick addr (RemoveSymbol symbol) ]
            [ text "remove" ]
        ]

validState : String -> Model -> Bool
validState text model =
    if String.length text > 0 then not <| List.member text model.states else False

viewState : Signal.Address Action -> Model -> State -> Html
viewState addr model state =
    li
        []
        [ text state
        , text " "
        , let
            (label, action) = 
                if ((Just state) == model.startState) then
                   ("unset start state", UpdateStartState Nothing)
                else
                    ("set start state", UpdateStartState (Just state))
            in
                button
                    [onClick addr action]
                    [text label]
        , text " "
        , let
            (label, action) =
                if List.member state model.acceptStates then
                   ("unset accept state", RemoveAcceptState state)
                else
                    ("set accept state", AddAcceptState state)
            in
                button
                    [onClick addr action]
                    [text label]
        , text " "
        , button
            [ onClick addr (RemoveState state) ]
            [ text "remove" ]
        ]

-- Update

type Action
    = NoOp
    | AddState                                  -- 1
    | RemoveState State                         -- 2
    | RenameState State String                  -- 3
    | AddSymbol                                 -- 4
    | RemoveSymbol Symbol                       -- 5
    | RenameSymbol Symbol Char                  -- 6
    | AddTransition                             -- 7
    | RemoveTransition (State, Symbol)          -- 8
    | UpdateStartState (Maybe State)            -- 9
    | AddAcceptState State                      --10
    | RemoveAcceptState State                   --11
    | UpdateSymbolAdderValue String             --12
    | UpdateStateAdderValue String              --13
    | UpdateTransitionFunctionAdderFromState (Maybe State)
    | UpdateTransitionFunctionAdderSymbol (Maybe Symbol)
    | UpdateTransitionFunctionAdderToState (Maybe State)

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model
        AddState ->
            { model 
                | states = model.states ++ [model.stateAdderValue]
                , stateAdderValue = ""
            }
        RemoveState state ->
            { model
                | states = remove state model.states
                , transitionFunction = Dict.filter (\(s1, _) s2 -> s1 /= state && s2 /= state) model.transitionFunction
                , startState =
                    case model.startState of
                        Just startState ->
                            if startState == state then Nothing else Just startState
                        Nothing ->
                            Nothing
                , acceptStates = remove state model.acceptStates 
            }
        RenameState stateOld stateNew ->
            { model | states = rename stateOld stateNew model.states
                    , transitionFunction = renameInTransitions (\((state1,symbol),state2) ->
                        ((maybeReplace stateOld stateNew state1,symbol),maybeReplace stateOld stateNew state2)) model.transitionFunction
                    , startState =
                        case model.startState of
                            Just startState ->
                                if startState == stateOld then Just stateNew else Just startState
                            Nothing ->
                                Nothing
                    , acceptStates = rename stateOld stateNew model.acceptStates
            }
        AddSymbol ->
            let
                maybeSymbol = String.uncons model.symbolAdderValue
            in
                { model 
                    | alphabet =
                        case maybeSymbol of
                            Just (char, _) ->
                                model.alphabet ++ [char]
                            Nothing ->
                                Debug.crash "Shouldn't get here"
                , symbolAdderValue = ""
            }
        RemoveSymbol symbol ->
            { model
                | alphabet = remove symbol model.alphabet
                , transitionFunction = Dict.filter (\(_, s) _ -> s /= symbol) model.transitionFunction
            }
        RenameSymbol symbolOld symbolNew ->
            { model
                | alphabet = rename symbolOld symbolNew model.alphabet
                , transitionFunction = renameInTransitions (\((state1,symbol),state2) ->
                    ((state1, maybeReplace symbolOld symbolNew symbol), state2)) model.transitionFunction
            }
        UpdateSymbolAdderValue text ->
            { model | symbolAdderValue = text }
        UpdateStateAdderValue text ->
            { model | stateAdderValue = text }
        UpdateStartState maybeState ->
            { model | startState =
                case maybeState of
                    Just state ->
                        if List.member state model.states then maybeState else Debug.crash "Must be a state"
                    Nothing ->
                        Nothing
            }
        AddAcceptState state ->
            { model | acceptStates = if List.member state model.states then model.acceptStates ++ [state] else Debug.crash "Must be a state" }
        RemoveAcceptState state ->
            { model | acceptStates = remove state model.acceptStates }
        UpdateTransitionFunctionAdderFromState state ->
            let
                adder = model.transitionFunctionAdderValue
            in
               { model | transitionFunctionAdderValue = { adder | fromState = state } }
        UpdateTransitionFunctionAdderSymbol symbol ->
            let
                adder = model.transitionFunctionAdderValue
            in
                { model | transitionFunctionAdderValue = { adder | symbol = symbol } }
        UpdateTransitionFunctionAdderToState state ->
            let
                adder = model.transitionFunctionAdderValue
            in
               { model | transitionFunctionAdderValue = { adder | toState = state } }
        _ ->
            Debug.crash "TODO" -- TODO: implement

remove : comparable -> List comparable -> List comparable
remove elem list = List.filter (\s -> s /= elem) list

rename : comparable -> comparable -> List comparable -> List comparable
rename old new list = List.map (\s -> maybeReplace old new s) list

renameInTransitions : (((State, Symbol), State) -> ((State, Symbol), State)) -> Dict (State, Symbol) State -> Dict (State, Symbol) State
renameInTransitions replaceFunction transitionFunction =
    transitionFunction
        |> Dict.toList
        |> List.map replaceFunction
        |> Dict.fromList

maybeReplace : comparable -> comparable -> comparable -> comparable
maybeReplace old new question = if question == old then new else question

-- Wiring

app : StartApp.Config Model Action
app =
    { model = initialModel
    , view = view
    , update = update
    }

main : Signal Html
main = 
    StartApp.start app

