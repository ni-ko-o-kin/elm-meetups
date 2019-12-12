module Validate exposing (validate)

import Cat exposing (Cat(..))
import Common exposing (Unvalidated, Validated)
import Dog exposing (Dog(..))
import Snake exposing (Snake(..))


validate :
    Cat Unvalidated
    -> Snake Unvalidated
    -> Dog
    -> Maybe ( Cat Validated, Snake Validated, Dog )
validate unvalidatedCat unvalidatedSnake dontKnowDog =
    Cat.validate unvalidatedCat
        |> Maybe.andThen
            (\cat ->
                case Snake.validate unvalidatedSnake of
                    Nothing ->
                        Nothing

                    Just snake ->
                        Just ( Cat.addSecret cat, snake )
            )
        |> Maybe.andThen
            (\( cat, snake ) ->
                case Dog.validate dontKnowDog of
                    Nothing ->
                        Nothing

                    Just dog ->
                        Just ( cat, snake, dog )
            )
