module Main exposing (main)

import Browser
import Cat exposing (Cat(..))
import Common exposing (Animal)
import Dog exposing (Dog(..))
import Html exposing (Html, div, text)
import Snake exposing (Snake(..))
import Validate exposing (validate)


view : () -> Html msg
view _ =
    case validate Cat.cat Snake.snake Dog.dog of
        Nothing ->
            text "nothing"

        Just ( _, _, UnvalidatedDog _ ) ->
            text "nothing"

        Just ( Cat catData, Snake.Snake snakeData, ValidatedDog dogData ) ->
            text <| String.join " - " [ catData.name, snakeData.name, dogData.name ]


main : Program () () msg
main =
    Browser.sandbox
        { init = ()
        , view = view
        , update = \_ _ -> ()
        }
