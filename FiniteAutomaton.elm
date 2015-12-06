module FiniteAutomaton where

import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
    , symbolAdderValue : Char
    , transitionFunctionAdderValue : (State, Symbol, State)
    }

-- View

view : Signal.Address Action -> Model -> Html
view addr model =
    text "Hello world!"

-- Update

type Action
    = AddState                                  -- 1
    | RemoveState State                         -- 2
    | RenameState State String                  -- 3
    | AddSymbol                                 -- 4
    | RemoveSymbol Symbol                       -- 5
    | RenameSymbol Symbol Char                  -- 6
    | AddTransition ((State, Symbol), State)    -- 7
    | RemoveTransition (State, Symbol)          -- 8
    | UpdateStartState Maybe State              -- 9
    | AddAcceptState State                      --10
    | RemoveAcceptState State                   --11

update : Action -> Model -> Model
update action model =
    case action of
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
            { model 
                | alphabet = model.alphabet ++ [model.symbolAdderValue]
                , symbolAdderValue = '\0' -- TODO: what to actually do here?
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

