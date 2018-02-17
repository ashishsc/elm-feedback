module Feedback exposing (Config, State, Submission(..), config, init, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E


type Config msg
    = Config
        { id : String
        , prompt : String
        , footnote : Maybe String
        , onStateChange : State -> msg
        , onSubmit : Submission -> msg
        , onCancel : msg
        , negativeReasons : List Reason
        }


config :
    { id : String
    , prompt : String
    , footnote : Maybe String
    , onStateChange : State -> msg
    , onSubmit : Submission -> msg
    , onCancel : msg
    , negativeReasons : List Reason
    }
    -> Config msg
config { id, prompt, footnote, onStateChange, onSubmit, onCancel, negativeReasons } =
    Config
        { id = id
        , prompt = prompt
        , footnote = footnote
        , onStateChange = onStateChange
        , onSubmit = onSubmit
        , onCancel = onCancel
        , negativeReasons = negativeReasons
        }


type alias Reason =
    String


type Submission
    = Positive
    | Negative Reason


type State
    = PositiveSelected
    | SelectingNegative
    | NegativeSelected Reason
    | Unselected


init : State
init =
    Unselected


buttonPositive : State -> (State -> msg) -> Html msg
buttonPositive state onStateChange =
    let
        btnClass =
            case state of
                PositiveSelected ->
                    "selected"

                _ ->
                    ""
    in
    button [ class btnClass, E.onClick (onStateChange PositiveSelected) ] [ text "Yes" ]


buttonNegative : State -> (State -> msg) -> Html msg
buttonNegative state onStateChange =
    let
        btnClass =
            case state of
                PositiveSelected ->
                    ""

                Unselected ->
                    ""

                NegativeSelected _ ->
                    "selected"

                SelectingNegative ->
                    "selected"

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
    button [ class btnClass, E.onClick (onStateChange nextState) ] [ text "No" ]


getReason : State -> Maybe Reason
getReason state =
    case state of
        NegativeSelected reason ->
            Just reason

        _ ->
            Nothing


radio : (State -> stateChangeMsg) -> Maybe Reason -> Reason -> Html stateChangeMsg
radio onStateChange maybeSelectedReason reason =
    let
        isChecked =
            case maybeSelectedReason of
                Just selectedReason ->
                    selectedReason == reason

                Nothing ->
                    False

        nextState =
            NegativeSelected reason
    in
    label [ class "feedback__reasons__label" ]
        [ input [ type_ "radio", checked isChecked, E.onClick (onStateChange nextState) ] []
        , text reason
        ]


maybeReasons : State -> (State -> stateChangeMsg) -> List Reason -> Html stateChangeMsg
maybeReasons state onStateChange negReasons =
    case state of
        PositiveSelected ->
            text ""

        Unselected ->
            text ""

        _ ->
            div [ class "feedback__reasons" ]
                (List.map
                    (\reason ->
                        radio onStateChange (getReason state) reason
                    )
                    negReasons
                )


maybeFootnote : Maybe String -> Html msg
maybeFootnote maybeNote =
    case maybeNote of
        Nothing ->
            text ""

        Just note ->
            div [ class "feedback__footnote" ] []


createSubmission : State -> Maybe Submission
createSubmission state =
    case state of
        Unselected ->
            Nothing

        PositiveSelected ->
            Just Positive

        SelectingNegative ->
            Nothing

        NegativeSelected reason ->
            Just (Negative reason)


submitCancel : State -> (State -> msg) -> (Submission -> msg) -> msg -> Html msg
submitCancel state onStateChange onSubmission onCancel =
    let
        maybeSubmission =
            createSubmission state

        submitBtnAttrs =
            case maybeSubmission of
                Just submission ->
                    [ E.onClick (onSubmission submission) ]

                Nothing ->
                    [ disabled True ]
    in
    div [ class "feedback-submit" ]
        [ button [ E.onClick onCancel ] [ text "Cancel" ]
        , button submitBtnAttrs [ text "Submit" ]
        ]


view : Config msg -> State -> Html msg
view (Config { id, prompt, footnote, onStateChange, onSubmit, onCancel, negativeReasons }) state =
    div [ class "feedback" ]
        [ div [ class "feedback__prompt" ] [ text prompt ]
        , div [ class "feedback__inclination" ]
            [ buttonPositive state onStateChange
            , buttonNegative state onStateChange
            ]
        , maybeReasons state onStateChange negativeReasons
        , maybeFootnote footnote
        , submitCancel state onStateChange onSubmit onCancel
        ]
