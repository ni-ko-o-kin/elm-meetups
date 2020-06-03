module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "isSet"
        (List.concat
            [ List.indexedMap
                (\index example -> validExampleTest example ("valid example " ++ String.fromInt index))
                validExamples
            , List.indexedMap
                (\index example -> invalidExampleTest example ("invalid example " ++ String.fromInt index))
                invalidExamples
            ]
        )


validExampleTest : Set -> String -> Test
validExampleTest set testTitle =
    test testTitle <|
        \_ ->
            Expect.equal (isValidSet set) True


invalidExampleTest : Set -> String -> Test
invalidExampleTest set testTitle =
    test testTitle <|
        \_ ->
            Expect.equal (isValidSet set) False


validExamples : List Set
validExamples =
    [ Set
        (initCard Oval Red One Striped)
        (initCard Oval Red One Empty)
        (initCard Oval Red One Solid)
    , Set
        (initCard Squiggle Green One Striped)
        (initCard Oval Purple Two Striped)
        (initCard Diamond Red Three Striped)
    , Set
        (initCard Oval Purple One Striped)
        (initCard Diamond Green Two Solid)
        (initCard Squiggle Red Three Empty)
    ]


invalidExamples : List Set
invalidExamples =
    [ Set
        (initCard Diamond Red One Empty)
        (initCard Diamond Purple One Empty)
        (initCard Diamond Green One Solid)
    , Set
        (initCard Squiggle Green Two Empty)
        (initCard Squiggle Red Two Striped)
        (initCard Squiggle Red Two Solid)
    ]
