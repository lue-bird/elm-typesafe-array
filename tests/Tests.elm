module Tests exposing (startBoard, suite)

import Arr exposing (Arr)
import Array
import Expect
import InArr
import LinearDirection exposing (LinearDirection(..))
import NNats exposing (..)
import Nat exposing (In, Only)
import Test exposing (Test, describe, test)
import TypeNats exposing (..)


suite : Test
suite =
    describe "typesafe-array"
        [ inArrTests
        , arrTests
        ]


arrTests : Test
arrTests =
    describe "Arr"
        [ test "when"
            (\() ->
                Arr.when (\n -> n >= 3)
                    (Arr.from5 1 2 3 4 5)
                    |> Arr.toList
                    |> Expect.equal [ 3, 4, 5 ]
            )
        , test "dropWhen"
            (\() ->
                Arr.dropWhen (\n -> n < 3)
                    (Arr.from5 1 2 3 4 5)
                    |> Arr.toList
                    |> Expect.equal [ 3, 4, 5 ]
            )
        ]


inArrTests : Test
inArrTests =
    describe "InArr"
        [ test "append"
            (\() ->
                Arr.from3 1 1 1
                    |> InArr.append nat3 (Arr.from3 0 0 0)
                    |> Arr.toList
                    |> Expect.equal
                        [ 1, 1, 1, 0, 0, 0 ]
            )
        , describe "resize"
            [ describe "FirstToLast"
                [ test "length less than current"
                    (\() ->
                        Arr.resize FirstToLast nat3 0 num1234
                            |> Expect.equal
                                (Arr.from3 1 2 3)
                    )
                , test "length greater than current"
                    (\() ->
                        Arr.resize FirstToLast nat6 0 num1234
                            |> Expect.equal
                                (Arr.from6 1 2 3 4 0 0)
                    )
                ]
            , describe "LastToFirst"
                [ test "length less than current"
                    (\() ->
                        Arr.resize LastToFirst nat3 0 num1234
                            |> Expect.equal
                                (Arr.from3 2 3 4)
                    )
                , test "length greater than current"
                    (\() ->
                        Arr.resize LastToFirst nat6 0 num1234
                            |> Expect.equal
                                (Arr.from6 0 0 1 2 3 4)
                    )
                ]
            ]
        ]


num1234 : Arr (In Nat4 (Nat4Plus a_)) number_
num1234 =
    Arr.from4 1 2 3 4


maybePush :
    Maybe a
    -> Arr (In min max) a
    -> Arr (In min (Nat1Plus max)) a
maybePush maybePushedElement =
    InArr.appendIn nat0
        nat1
        (Arr.from1 maybePushedElement
            |> Arr.whenJust
        )


startBoard : Arr (Only Nat8) (Arr (Only Nat8) Field)
startBoard =
    let
        pawnRow color =
            Arr.repeat nat8 (Piece Pawn color)

        firstRow color =
            Arr.repeat nat8 (Piece Other color)
    in
    Arr.empty
        |> InArr.push (firstRow White)
        |> InArr.push (pawnRow White)
        |> InArr.append nat4
            (Arr.repeat nat4 (Arr.repeat nat8 Empty))
        |> InArr.push (pawnRow Black)
        |> InArr.push (firstRow Black)


type Color
    = Black
    | White


type Field
    = Empty
    | Piece Piece Color


type Piece
    = Pawn
    | Other
