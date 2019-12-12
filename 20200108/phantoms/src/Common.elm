module Common exposing (Animal, Unvalidated(..), Validated(..))


type alias Animal =
    { name : String, age : Int }


type Validated
    = Validated


type Unvalidated
    = Unvalidated
