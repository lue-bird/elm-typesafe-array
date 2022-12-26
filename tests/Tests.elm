module Tests exposing (suite)

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable, filled)
import Expect exposing (Expectation)
import Linear exposing (Direction(..))
import N exposing (Add1, Exactly, In, N8, On, To, Up, n1, n2, n4, n8)
import Possibly exposing (Possibly)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typesafe-array"
        [ maximumConstrainedTest
        , anyMaximumTests
        ]


anyMaximumTests : Test
anyMaximumTests =
    describe "any maximum"
        [ describe "allFilled"
            [ test "all Filled → Filled"
                (\() ->
                    case
                        ArraySized.l3 (filled 1) (filled 2) (filled 3)
                            |> ArraySized.allFill
                    of
                        Emptiable.Filled arraySized ->
                            arraySized
                                |> ArraySized.toList
                                |> Expect.equalLists [ 1, 2, 3 ]

                        Emptiable.Empty _ ->
                            Expect.fail "Emptiable.Empty, expected Emptiable.Filled"
                )
            , test "one empty → empty"
                (\() ->
                    ArraySized.l3 (filled 1) Emptiable.empty (filled 3)
                        |> ArraySized.allFill
                        |> Expect.equal Emptiable.empty
                )
            , test "all toEven → filled"
                (\() ->
                    ArraySized.l2 2 4
                        |> ArraySized.map toEven
                        |> ArraySized.allFill
                        |> Emptiable.map ArraySized.toList
                        |> Expect.equal (filled [ 2, 4 ])
                )
            , test "one not toEven → empty"
                (\() ->
                    ArraySized.l2 2 3
                        |> ArraySized.map toEven
                        |> ArraySized.allFill
                        |> Expect.equal Emptiable.empty
                )
            ]
        ]


toEven : Int -> Emptiable Int Possibly
toEven =
    \n ->
        if (n |> modBy 2) == 0 then
            n |> filled

        else
            Emptiable.empty


maximumConstrainedTest : Test
maximumConstrainedTest =
    describe "maximum constrained"
        [ describe "attach"
            [ test "Up → append"
                (\() ->
                    ArraySized.l3 1 1 1
                        |> ArraySized.attach Up (ArraySized.l3 0 0 0)
                        |> expectEqualArraySized
                            (ArraySized.l6 1 1 1 0 0 0)
                )
            , test "Down → prepend"
                (\() ->
                    ArraySized.l3 1 1 1
                        |> ArraySized.attach Down (ArraySized.l3 0 0 0)
                        |> expectEqualArraySized
                            (ArraySized.l6 0 0 0 1 1 1)
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


pushEmptiable :
    Emptiable element possiblyOrNever_
    ->
        (ArraySized
            element
            (In
                (Up minX To minPlusX)
                (Up maxX To maxPlusX)
            )
         ->
            ArraySized
                element
                (In (Up minX To minPlusX) (Up maxX To (Add1 maxPlusX)))
        )
pushEmptiable emptiableElementToPush =
    case emptiableElementToPush of
        Emptiable.Empty _ ->
            \arraySized -> arraySized |> ArraySized.maxAdd n1

        Emptiable.Filled elementToPush ->
            \arraySized ->
                arraySized
                    |> ArraySized.push elementToPush
                    |> ArraySized.minSubtract n1


startBoard : ArraySized (ArraySized Field (Exactly (On N8))) (Exactly (On N8))
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
        |> ArraySized.attach Up
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
