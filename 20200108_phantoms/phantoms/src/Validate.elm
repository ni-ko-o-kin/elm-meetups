module Validate exposing (validate)

import Cat exposing (Cat(..))
import Common exposing (Unvalidated, Validated)
import Dog exposing (Dog(..))
import Snake exposing (UnvalidatedSnake(..), ValidatedSnake(..))
import Utils exposing (apply, triple)


validate :
    Cat Unvalidated
    -> UnvalidatedSnake
    -> Dog
    -> Maybe ( Cat Validated, ValidatedSnake, Dog )
validate unvalidatedCat unvalidatedSnake dontKnowDog =
    Just triple
        |> apply (Cat.validate unvalidatedCat)
        |> apply (Snake.validate unvalidatedSnake)
        |> apply (Dog.validate dontKnowDog)
