module Main exposing (main)

import Browser
import Element exposing (Element, centerX, centerY, el, fill, height, padding, rgb255, text, width)
import Element.Border as Border
import Html exposing (Html)


type alias Model =
    {}


type Msg
    = NoOp


main =
    Browser.sandbox
        { init = init, view = view, update = update }


init : Model
init =
    {}


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill, height fill ]
        (el
            [ centerX
            , centerY
            , Border.solid
            , Border.width 3
            , Border.rounded 3
            , Border.color <| rgb255 255 192 203
            , padding 10
            ]
            (text "tic-tac-toe")
        )
