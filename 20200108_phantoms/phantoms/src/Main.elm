module Main exposing (main)

import Browser
import Cat exposing (Cat(..))
import Common exposing (Animal)
import Dog exposing (Dog(..))
import Html exposing (Html, div, text)
import Snake exposing (UnvalidatedSnake(..), ValidatedSnake(..))
import Validate exposing (validate)


view : () -> Html msg
view _ =
    case validate Cat.cat Snake.snake Dog.dog of
        Nothing ->
            text "nothing"

        Just ( _, _, UnvalidatedDog _ ) ->
            text "nothing"

        Just ( cat, snake, dog ) ->
            div []
                [ div [] [ Cat.view cat ]
                , div [] [ Snake.view snake ]
                , div [] [ Dog.view dog ]
                ]


main : Program () () msg
main =
    Browser.sandbox
        { init = ()
        , view = view
        , update = \_ _ -> ()
        }
