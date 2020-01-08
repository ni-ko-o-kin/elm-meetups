module Main exposing (..)

import Browser
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, rgb255, text, width)
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
    = Card CardData


type alias CardData =
    { symbol : Symbol
    , color : Color
    , number : Number
    , shading : Shading
    }


initCard : Symbol -> Color -> Number -> Shading -> Card
initCard symbol color number shading =
    Card
        { symbol = symbol
        , color = color
        , number = number
        , shading = shading
        }


deck : List Card
deck =
    List.Extra.lift4 initCard symbols colors numbers shadings


type Set
    = Set Card Card Card


allSame : (CardData -> b) -> Card -> Card -> Card -> Bool
allSame getter (Card c1) (Card c2) (Card c3) =
    getter c1 == getter c2 && getter c1 == getter c3


allDiff : (CardData -> b) -> Card -> Card -> Card -> Bool
allDiff getter (Card c1) (Card c2) (Card c3) =
    getter c1 /= getter c2 && getter c1 /= getter c3 && getter c2 /= getter c3


isFeatureSet : (CardData -> b) -> Card -> Card -> Card -> Bool
isFeatureSet getter a b c =
    allSame getter a b c || allDiff getter a b c


isValidSet : Set -> Bool
isValidSet (Set a b c) =
    List.all
        identity
        [ isFeatureSet .symbol a b c
        , isFeatureSet .number a b c
        , isFeatureSet .color a b c
        , isFeatureSet .shading a b c
        ]


symbolsAllSame : Card -> Card -> Card -> Bool
symbolsAllSame =
    allSame .symbol


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
            ( Playing { remainingDeck = deckShuffled, deal = [] }, Cmd.none )


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
            (viewCard (initCard Squiggle Red Two Striped))
        )


viewCard : Card -> Element Msg
viewCard (Card { symbol, color, number, shading }) =
    column []
        [ text (symbolToString symbol)
        , text (colorToString color)
        , text (numberToString number)
        , text (shadingToString shading)
        ]



-- type alias CardData =
--     { symbol : Symbol
--     , color : Color
--     , number : Number
--     , shading : Shading
--     }


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        Oval ->
            "Oval"

        Diamond ->
            "Diamond"

        Squiggle ->
            "Squiggle"


numberToString : Number -> String
numberToString number =
    case number of
        One ->
            "One"

        Two ->
            "Two"

        Three ->
            "Three"


shadingToString : Shading -> String
shadingToString shading =
    case shading of
        Solid ->
            "Solid"

        Striped ->
            "Striped"

        Empty ->
            "Empty"


colorToString : Color -> String
colorToString color =
    case color of
        Red ->
            "Red"

        Green ->
            "Green"

        Purple ->
            "Purple"
