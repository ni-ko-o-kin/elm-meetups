module Main exposing (main)

import Browser
import Element exposing (Element, centerX, centerY, el, fill, height, padding, rgb255, text, width)
import Element.Border as Border
import Html exposing (Html)
import List.Extra exposing (cartesianProduct)
import Random exposing (generate)
import Random.List exposing (shuffle)



-- TODO not allowed to construct two card of the same kind


type Model
    = Preparing
    | Playing
        { remainingDeck : List Card
        , deal : List Card
        }



-- Features: Symbol, Shading, Color and Number
-- all the same or all different on a feature level


type Symbol
    = Oval
    | Diamond
    | Squiggle


symbols =
    [ Oval, Diamond, Squiggle ]


type Shading
    = Solid
    | Striped
    | Empty


shadings =
    [ Solid, Striped, Empty ]


type Color
    = Red
    | Green
    | Purple


colors =
    [ Red, Green, Purple ]


type Number
    = One
    | Two
    | Three


numbers : List Number
numbers =
    [ One, Two, Three ]


type Card
    = Card Symbol Color Number Shading


deck : List Card
deck =
    List.Extra.lift4 Card symbols colors numbers shadings



-- type alias Set =
--     List Card
-- createSet : Card -> Card -> Card -> Set
-- createSet c1 c2 c3 =


type Msg
    = DeckShuffled (List Card)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Preparing
    , Random.generate DeckShuffled <| shuffle deck
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeckShuffled deckShuffled ->
            ( Debug.log "" <| Playing { remainingDeck = deckShuffled, deal = [] }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill, height fill ]
        (el
            [ centerX
            , centerY
            , Border.solid
            , Border.width 3
            , Border.rounded 3
            , Border.color <| rgb255 255 192 203
            , padding 10
            ]
            (text "SET")
        )
