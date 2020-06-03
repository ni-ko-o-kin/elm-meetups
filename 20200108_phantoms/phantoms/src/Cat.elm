module Cat exposing (Cat(..), addSecret, cat, validate, view)

import Common exposing (Animal, Unvalidated, Validated)
import Html exposing (Html, div, text)


type Cat a
    = Cat Animal


validate : Cat Unvalidated -> Maybe (Cat Validated)
validate (Cat catData) =
    case catData.age > 10 of
        True ->
            Just (Cat catData)

        False ->
            Nothing


cat : Cat Unvalidated
cat =
    Cat { name = "Kitty", age = 14 }


addSecret : Cat Validated -> Cat Validated
addSecret =
    identity


view : Cat Validated -> Html msg
view (Cat { name }) =
    text name
