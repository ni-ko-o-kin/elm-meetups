module Frontend exposing (..)

import Browser
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, pointer, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)
import Lamdera
import List.Extra exposing (cartesianProduct)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Set


app =
    Lamdera.frontend
        { init = \_ _ -> init
        , onUrlRequest = \_ -> Noop --UrlClicked
        , onUrlChange = \_ -> Noop --UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


updateFromBackend : toFrontend -> Model -> ( Model, Cmd Msg )
updateFromBackend fromBackend model =
    ( model, Cmd.none )


type Model
    = Preparing
    | Playing PlayingModel


type Picked
    = PickedNone
    | PickedOne Card
    | PickedTwo Card Card
    | Valid Set
    | Invalid Card Card Card


type alias PlayingModel =
    { remainingDeck : List Card
    , deal : List Card
    , picked : Picked
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
    | CardClicked Card
    | Noop


init : ( Model, Cmd Msg )
init =
    ( Preparing
    , Random.generate DeckShuffled <| shuffle deck
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( DeckShuffled deckShuffled, _ ) ->
            ( Playing
                { remainingDeck = List.drop 12 deckShuffled
                , deal = List.take 12 deckShuffled
                , picked = PickedNone
                }
            , Cmd.none
            )

        ( CardClicked card, Playing playingModel ) ->
            ( Playing { playingModel | picked = addCard card playingModel.picked }
            , Cmd.none
            )

        ( CardClicked _, _ ) ->
            ( model, Cmd.none )

        ( Noop, _ ) ->
            ( model, Cmd.none )


pickedToList : Picked -> List Card
pickedToList picked =
    case picked of
        PickedNone ->
            []

        PickedOne card1 ->
            [ card1 ]

        PickedTwo card1 card2 ->
            [ card1, card2 ]

        Invalid card1 card2 card3 ->
            [ card1, card2, card3 ]

        Valid (Set card1 card2 card3) ->
            [ card1, card2, card3 ]


addCard : Card -> Picked -> Picked
addCard card picked =
    let
        isMember_ =
            List.member card (pickedToList picked)
    in
    if isMember_ then
        picked

    else
        case picked of
            PickedNone ->
                PickedOne card

            PickedOne card1 ->
                PickedTwo card1 card

            PickedTwo card1 card2 ->
                if isValidSet (Set card1 card2 card) then
                    Valid (Set card1 card2 card)

                else
                    Invalid card1 card2 card

            Invalid _ _ _ ->
                PickedNone

            Valid _ ->
                PickedNone



-- toggleMember : a -> List a -> List a
-- toggleMember x xs =
--     if List.member x xs then
--         List.filter ((/=) x) xs
--     else
--         x :: xs


normalColor =
    rgb255 255 192 203


sadColor =
    rgb255 100 10 10


happyColor =
    rgb255 50 100 50


view : Model -> Browser.Document Msg
view model =
    { title = "Set Game"
    , body = [ bodyView model ]
    }


bodyView : Model -> Html Msg
bodyView model =
    let
        content =
            case model of
                Preparing ->
                    text "loading..."

                Playing { deal, picked } ->
                    viewCards picked deal

        bg =
            case model of
                Playing playingModel ->
                    case playingModel.picked of
                        Invalid _ _ _ ->
                            sadColor

                        Valid _ ->
                            happyColor

                        _ ->
                            normalColor

                _ ->
                    normalColor
    in
    Element.layout
        [ width fill, height fill ]
        (el
            [ centerX
            , centerY
            , Border.solid
            , Border.width 3
            , Border.rounded 3
            , Border.color <| normalColor
            , padding 10
            , Background.color bg
            ]
            content
        )


viewCard : List Card -> Card -> Element Msg
viewCard pickedCards ((Card { symbol, color, number, shading }) as card) =
    let
        isPicked =
            List.member card pickedCards

        color_ =
            if isPicked then
                rgb255 50 150 50

            else
                normalColor
    in
    column
        [ Border.width 3
        , padding 10
        , width (Element.fillPortion 1)
        , Events.onClick (CardClicked card)
        , Border.color color_
        , pointer
        ]
        [ text (symbolToString symbol)
        , text (colorToString color)
        , text (numberToString number)
        , text (shadingToString shading)
        ]


viewCards : Picked -> List Card -> Element Msg
viewCards pickedCards cards =
    column []
        (List.Extra.groupsOf 4 cards
            |> List.map (viewCardsRow (pickedToList pickedCards))
        )


viewCardsRow : List Card -> List Card -> Element Msg
viewCardsRow pickedCards cards =
    Element.row [ width fill ]
        (List.map (viewCard pickedCards) cards)


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


type Set
    = Set Card Card Card
