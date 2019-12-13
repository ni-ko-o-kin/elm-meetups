module Validate exposing (validate)

import Cat exposing (Cat(..))
import Common exposing (Unvalidated, Validated)
import Dog exposing (Dog(..))
import Snake exposing (UnvalidatedSnake(..), ValidatedSnake(..))


validate :
    Cat Unvalidated
    -> UnvalidatedSnake
    -> Dog
    -> Maybe ( Cat Validated, ValidatedSnake, Dog )
validate unvalidatedCat unvalidatedSnake dontKnowDog =
    Cat.validate unvalidatedCat
        |> Maybe.andThen
            (\cat ->
                case Snake.validate unvalidatedSnake of
                    Nothing ->
                        Nothing

                    Just snake ->
                        Just ( cat, snake )
            )
        |> Maybe.andThen
            (\( cat, snake ) ->
                case Dog.validate dontKnowDog of
                    Nothing ->
                        Nothing

                    Just dog ->
                        Just ( cat, snake, dog )
            )
