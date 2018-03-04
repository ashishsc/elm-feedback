module Feedback.Flexible exposing (Config, Reason, State(..), Submission(..), config, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E


type Config msg
    = Config
        { id : String
        , prompt : String
        , footnote : Maybe String
        , submit : Submission -> msg
        , cancel : msg
        , selectPositive : msg
        , selectNegative : msg
        , negativeReasons : List Reason
        , chooseReason : Reason -> msg
        , state : State
        }


config :
    { id : String
    , prompt : String
    , footnote : Maybe String
    , submit : Submission -> msg
    , cancel : msg
    , selectPositive : msg
    , selectNegative : msg
    , negativeReasons : List Reason
    , chooseReason : Reason -> msg
    , state : State
    }
    -> Config msg
config =
    Config


type alias Reason =
    String


type Submission
    = Positive
    | Negative Reason


positive : Submission
positive =
    Positive


negative : Reason -> Submission
negative =
    Negative


type State
    = PositiveSelected
    | SelectingNegative
    | NegativeSelected Reason
    | Unselected


buttonPositive : { r | selectPositive : msg } -> State -> Html msg
buttonPositive { selectPositive } state =
    let
        btnClass =
            case state of
                PositiveSelected ->
                    "selected"

                _ ->
                    ""
    in
    button [ class btnClass, E.onClick selectPositive ] [ text "Yes" ]


buttonNegative : { r | selectNegative : msg } -> State -> Html msg
buttonNegative { selectNegative } state =
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
    in
    button [ class btnClass, E.onClick selectNegative ] [ text "No" ]


getReason : State -> Maybe Reason
getReason state =
    case state of
        NegativeSelected reason ->
            Just reason

        _ ->
            Nothing


radio : (Reason -> msg) -> Maybe Reason -> Reason -> Html msg
radio chooseReason maybeSelectedReason reason =
    label [ class "feedback__reasons__label" ]
        [ input
            [ type_ "radio"
            , checked (maybeSelectedReason == Just reason)
            , E.onClick (chooseReason reason)
            ]
            []
        , text reason
        ]


maybeReasons : { r | chooseReason : Reason -> msg } -> State -> List Reason -> Html msg
maybeReasons { chooseReason } state negReasons =
    case state of
        PositiveSelected ->
            text ""

        Unselected ->
            text ""

        _ ->
            div [ class "feedback__reasons" ]
                (List.map
                    (\reason ->
                        radio chooseReason (getReason state) reason
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


submitCancel : { r | cancel : msg, submit : Submission -> msg } -> State -> Html msg
submitCancel { submit, cancel } state =
    let
        maybeSubmission =
            createSubmission state

        submitBtnAttrs =
            case maybeSubmission of
                Just submission ->
                    [ E.onClick (submit submission) ]

                Nothing ->
                    [ disabled True ]
    in
    div [ class "feedback-submit" ]
        [ button [ E.onClick cancel ] [ text "Cancel" ]
        , button submitBtnAttrs [ text "Submit" ]
        ]


view : Config msg -> Html msg
view (Config ({ id, prompt, footnote, submit, cancel, negativeReasons, state } as config)) =
    div [ class "feedback" ]
        [ div [ class "feedback__prompt" ] [ text prompt ]
        , div [ class "feedback__inclination" ]
            [ buttonPositive config state
            , buttonNegative config state
            ]
        , maybeReasons config state negativeReasons
        , maybeFootnote footnote
        , submitCancel config state
        ]
