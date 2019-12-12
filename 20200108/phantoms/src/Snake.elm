module Snake exposing (Snake(..), snake, validate, view)

import Common exposing (Animal, Unvalidated, Validated)
import Html exposing (Html, div, text)


type Snake a
    = Snake Animal


validate : Snake Unvalidated -> Maybe (Snake Validated)
validate (Snake snakeData) =
    case snakeData.age > 20 of
        True ->
            Just (Snake snakeData)

        False ->
            Nothing


snake : Snake Unvalidated
snake =
    Snake { name = "Sss", age = 100 }


addSecret : Snake Validated -> Snake Validated
addSecret =
    identity


view : Snake Validated -> Html msg
view (Snake { name }) =
    text name
