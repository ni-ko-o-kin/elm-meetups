module Snake exposing (UnvalidatedSnake(..), ValidatedSnake(..), snake, validate, view)

import Common exposing (Animal, Unvalidated, Validated)
import Html exposing (Html, div, text)


type ValidatedSnake
    = ValidatedSnake Animal


type UnvalidatedSnake
    = UnvalidatedSnake Animal


validate : UnvalidatedSnake -> Maybe ValidatedSnake
validate (UnvalidatedSnake snakeData) =
    case snakeData.age > 20 of
        True ->
            Just (ValidatedSnake snakeData)

        False ->
            Nothing


snake : UnvalidatedSnake
snake =
    UnvalidatedSnake { name = "Sss", age = 100 }


addSecret : ValidatedSnake -> ValidatedSnake
addSecret =
    identity


view : ValidatedSnake -> Html msg
view (ValidatedSnake { name }) =
    text name
