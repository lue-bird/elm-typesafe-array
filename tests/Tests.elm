module Tests exposing (suite)

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable, fillElseOnEmpty, fillMap, filled)
import Expect exposing (Expectation)
import Linear exposing (Direction(..))
import N exposing (Add1, Exactly, In, N8, To, Up, n2, n4, n8)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typesafe-array"
        [ maximumConstrainedTest
        , arraySizedTests
        ]


arraySizedTests : Test
arraySizedTests =
    describe "ArraySized"
        [ describe "all"
            [ test "True"
                (\() ->
                    ArraySized.all isEven (ArraySized.l2 2 4)
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    ArraySized.all isEven (ArraySized.l2 2 3)
                        |> Expect.equal False
                )
            ]
        , describe "any"
            [ test "True"
                (\() ->
                    ArraySized.any isEven (ArraySized.l2 1 2)
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    ArraySized.any isEven (ArraySized.l2 1 3)
                        |> Expect.equal False
                )
            ]
        ]


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0


maximumConstrainedTest : Test
maximumConstrainedTest =
    describe "maximum constrained"
        [ test "append"
            (\() ->
                ArraySized.l3 1 1 1
                    |> ArraySized.glue Up (ArraySized.l3 0 0 0)
                    |> expectEqualArraySized
                        (ArraySized.l6 1 1 1 0 0 0)
            )
        , test "prepend"
            (\() ->
                ArraySized.l3 1 1 1
                    |> ArraySized.glue Down (ArraySized.l3 0 0 0)
                    |> expectEqualArraySized
                        (ArraySized.l6 0 0 0 1 1 1)
            )
        , describe "allFill"
            [ test "all are Filled"
                (\() ->
                    ArraySized.l3 (filled 1) (filled 2) (filled 3)
                        |> ArraySized.allFill
                        |> fillMap
                            (ArraySized.toList >> Expect.equalLists [ 1, 2, 3 ])
                        |> fillElseOnEmpty
                            (\_ -> Expect.fail "was Nothing, expected Just")
                )
            , test "one is Nothing"
                (\() ->
                    ArraySized.l3 (filled 1) Emptiable.empty (filled 3)
                        |> ArraySized.allFill
                        |> Expect.equal Emptiable.empty
                )
            ]
        , test "intersperse"
            (\() ->
                ArraySized.l3 "turtles" "turtles" "turtles"
                    |> ArraySized.interweave (ArraySized.repeat "on" n2)
                    |> expectEqualArraySized
                        (ArraySized.l5 "turtles" "on" "turtles" "on" "turtles")
            )
        ]


expectEqualArraySized : ArraySized range a -> (ArraySized range a -> Expectation)
expectEqualArraySized expected actual =
    Expect.equalLists
        (expected |> ArraySized.toList)
        (actual |> ArraySized.toList)


maybePush :
    Emptiable element possiblyOrNever_
    ->
        (ArraySized
            (In
                (Up minX To minSumPlusX)
                (Up maxX To maxPlusX)
            )
            element
         ->
            ArraySized
                (In (Up minX To minSumPlusX) (Up maxX To (Add1 maxPlusX)))
                element
        )
maybePush maybePushedElement =
    ArraySized.glue Up
        (ArraySized.l1 maybePushedElement
            |> ArraySized.fills
        )


startBoard : ArraySized (Exactly N8) (ArraySized (Exactly N8) Field)
startBoard =
    let
        pawnRow color =
            ArraySized.repeat (Piece Pawn color) n8

        firstRow color =
            ArraySized.repeat (Piece Other color) n8
    in
    ArraySized.empty
        |> ArraySized.push (firstRow White)
        |> ArraySized.push (pawnRow White)
        |> ArraySized.glue Up
            (ArraySized.repeat
                (ArraySized.repeat Empty n8)
                n4
            )
        |> ArraySized.push (pawnRow Black)
        |> ArraySized.push (firstRow Black)


type Color
    = Black
    | White


type Field
    = Empty
    | Piece Piece Color


type Piece
    = Pawn
    | Other
