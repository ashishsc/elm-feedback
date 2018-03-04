module Feedback exposing (Config, State, Submission(..), config, init, view)

import Feedback.Flexible as Fb exposing (State(..), Submission(..))
import Html exposing (..)


type Config msg
    = Config
        { id : String
        , prompt : String
        , footnote : Maybe String
        , onStateChange : State -> msg
        , submit : Submission -> msg
        , cancel : msg
        , negativeReasons : List Reason
        }


config :
    { id : String
    , prompt : String
    , footnote : Maybe String
    , onStateChange : State -> msg
    , submit : Submission -> msg
    , cancel : msg
    , negativeReasons : List Reason
    }
    -> Config msg
config =
    Config


type alias Reason =
    Fb.Reason


type Submission
    = Positive
    | Negative Reason


type alias State =
    Fb.State


init : State
init =
    Unselected


view : Config msg -> State -> Html msg
view (Config { id, prompt, footnote, onStateChange, submit, cancel, negativeReasons }) state =
    Fb.view <|
        Fb.config
            { id = id
            , prompt = prompt
            , footnote = footnote
            , submit =
                \flexibleSubmit ->
                    submit <|
                        case flexibleSubmit of
                            Fb.Positive ->
                                Positive

                            Fb.Negative r ->
                                Negative r
            , cancel = cancel
            , negativeReasons = negativeReasons
            , state = state
            , chooseReason = onStateChange << NegativeSelected
            , selectNegative =
                let
                    nextState =
                        case state of
                            PositiveSelected ->
                                SelectingNegative

                            Unselected ->
                                SelectingNegative

                            NegativeSelected reason ->
                                NegativeSelected reason

                            SelectingNegative ->
                                SelectingNegative
                in
                onStateChange nextState
            , selectPositive = onStateChange PositiveSelected
            }
