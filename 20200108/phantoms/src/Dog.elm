module Dog exposing (Dog(..), dog, validate, view)

import Common exposing (Animal)
import Html exposing (Html, div, text)


type Dog
    = ValidatedDog Animal
    | UnvalidatedDog Animal


dog : Dog
dog =
    UnvalidatedDog { name = "Waldi", age = 12 }


validate : Dog -> Maybe Dog
validate d =
    case d of
        UnvalidatedDog dogData ->
            case dogData.age > 10 of
                True ->
                    Just <| ValidatedDog dogData

                False ->
                    Nothing

        ValidatedDog dogData ->
            Just (ValidatedDog dogData)


addSecret : Dog -> Maybe Dog
addSecret d =
    case d of
        ValidatedDog dogData ->
            Just <| ValidatedDog dogData

        UnvalidatedDog dogData ->
            Nothing


view : Dog -> Html msg
view d =
    case d of
        ValidatedDog { name } ->
            text name

        UnvalidatedDog _ ->
            text "unvalidated dog"
