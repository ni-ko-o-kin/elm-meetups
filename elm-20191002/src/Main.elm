module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


limit : Int
limit =
    10


type Status
    = BelowLimit Int
    | OverLimit Int
    | Critical Int


type Model
    = Idle
    | EnginesRunning Status
    | Explosion


init : Model
init =
    Idle


type Msg
    = Increment
    | Decrement
    | Shutdown


calcEngineStatus : Int -> Status
calcEngineStatus n =
    if n < limit then
        BelowLimit <| max 1 n

    else if n < limit + limit // 2 then
        OverLimit n

    else
        Critical n


getCount : Status -> Int
getCount status =
    case status of
        BelowLimit n ->
            n

        OverLimit n ->
            n

        Critical n ->
            n


update : Msg -> Model -> Model
update msg model =
    case ( model, msg ) of
        ( Explosion, _ ) ->
            Explosion

        ( Idle, Increment ) ->
            EnginesRunning <| calcEngineStatus 1

        ( Idle, _ ) ->
            Idle

        ( _, Shutdown ) ->
            Idle

        ( EnginesRunning (Critical _), Increment ) ->
            Explosion

        ( EnginesRunning status, Increment ) ->
            getCount status
                |> (+) 1
                |> calcEngineStatus
                |> EnginesRunning

        ( EnginesRunning status, Decrement ) ->
            getCount status
                |> (-) 1
                |> calcEngineStatus
                |> EnginesRunning


view : Model -> Html Msg
view model =
    case model of
        Idle ->
            button [ onClick Increment ] [ text "Start" ]

        EnginesRunning status ->
            viewControl status

        Explosion ->
            h1 [ style "background-color" "red" ] [ text "E x p l o s i o n" ]


viewControl : Status -> Html Msg
viewControl status =
    let
        ( backgroundColor, statusText ) =
            case status of
                BelowLimit _ ->
                    ( "lightgrey", "Engines running below limit." )

                OverLimit _ ->
                    ( "grey", "Engines running over limit." )

                Critical _ ->
                    ( "orange", "CRITICAL!" )
    in
    div [ style "background-color" backgroundColor ]
        [ button [ onClick Decrement ] [ text "-1" ]
        , div [] [ text <| String.fromInt <| getCount status ]
        , button [ onClick Increment ] [ text "+1" ]
        , button [ onClick Shutdown ] [ text "shutdown" ]
        , div [] [ text statusText ]
        ]
