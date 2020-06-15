module Utils exposing (apply, triple)


apply : Maybe a -> Maybe (a -> b) -> Maybe b
apply =
    Maybe.map2 (|>)


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )
