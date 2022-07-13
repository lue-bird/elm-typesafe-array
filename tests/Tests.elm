module Tests exposing (startBoard, suite)

import ArraySized exposing (ArraySized, In)
import Emptiable exposing (Emptiable, fillElseOnEmpty, fillMap, filled)
import Expect exposing (Expectation)
import Linear exposing (DirectionLinear(..))
import N exposing (Add1, Add4, Exactly, Min, N4, N8, n0, n1, n3, n4, n6, n8)
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
                    ArraySized.areAll isEven (ArraySized.l2 2 4)
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    ArraySized.areAll isEven (ArraySized.l2 2 3)
                        |> Expect.equal False
                )
            ]
        , describe "any"
            [ test "True"
                (\() ->
                    ArraySized.isAny isEven (ArraySized.l2 1 2)
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    ArraySized.isAny isEven (ArraySized.l2 1 3)
                        |> Expect.equal False
                )
            ]
        ]


maximumConstrainedTest : Test
maximumConstrainedTest =
    describe "maximum constrained"
        [ test "append"
            (\() ->
                ArraySized.l3 1 1 1
                    |> ArraySized.glue Up n3 (ArraySized.l3 0 0 0)
                    |> expectEqualArraySized
                        (ArraySized.l6 1 1 1 0 0 0)
            )
        , test "prepend"
            (\() ->
                ArraySized.l3 1 1 1
                    |> ArraySized.glue Down n3 (ArraySized.l3 0 0 0)
                    |> expectEqualArraySized
                        (ArraySized.l6 0 0 0 1 1 1)
            )
        , describe "areAllFilled"
            [ test "all are Filled"
                (\() ->
                    ArraySized.l3 (filled 1) (filled 2) (filled 3)
                        |> ArraySized.areAllFilled
                        |> fillMap
                            (ArraySized.toList >> Expect.equalLists [ 1, 2, 3 ])
                        |> fillElseOnEmpty
                            (\_ -> Expect.fail "was Nothing, expected Just")
                )
            , test "one is Nothing"
                (\() ->
                    ArraySized.l3 (filled 1) Emptiable.empty (filled 3)
                        |> ArraySized.areAllFilled
                        |> Expect.equal Emptiable.empty
                )
            ]
        , test "intersperse"
            (\() ->
                ArraySized.l3 "turtles" "turtles" "turtles"
                    |> ArraySized.intersperseIn ( n3, n3 ) "on"
                    |> expectEqualArraySized
                        (ArraySized.l5 "turtles" "on" "turtles" "on" "turtles")
            )
        ]


num1234 : ArraySized (In N4 (Add4 a_)) number_
num1234 =
    ArraySized.l4 1 2 3 4


maybePush :
    Emptiable a possiblyOrNever
    -> ArraySized (In min max) a
    -> ArraySized (In min (Add1 max)) a
maybePush maybePushedElement =
    ArraySized.glueIn Up
        ( n0, n1 )
        (ArraySized.l1 maybePushedElement
            |> ArraySized.fills
        )


minCons :
    element
    -> ArraySized (In min maxLength_) element
    -> ArraySized (Min (Add1 min)) element
minCons =
    ArraySized.minInsert ( Up, n0 )


cons :
    element
    -> ArraySized (In min max) element
    -> ArraySized (In (Add1 min) (Add1 max)) element
cons =
    ArraySized.insert ( Up, n0 )


startBoard : ArraySized (Exactly N8) (ArraySized (Exactly N8) Field)
startBoard =
    let
        pawnRow color =
            ArraySized.repeat n8 (Piece Pawn color)

        firstRow color =
            ArraySized.repeat n8 (Piece Other color)
    in
    ArraySized.empty
        |> ArraySized.push (firstRow White)
        |> ArraySized.push (pawnRow White)
        |> ArraySized.glue Up
            n4
            (ArraySized.repeat n4
                (ArraySized.repeat n8 Empty)
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



-- ↓ not important


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0


expectEqualArraySized : ArraySized range a -> (ArraySized range a -> Expectation)
expectEqualArraySized expected actual =
    Expect.equalLists
        (expected |> ArraySized.toList)
        (actual |> ArraySized.toList)
