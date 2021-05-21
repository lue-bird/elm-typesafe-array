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
        [ test "takeWhen"
            (\() ->
                Arr.takeWhen (\n -> n >= 3)
                    (Arr.from5 1 2 3 4 5)
                    |> Arr.toArray
                    |> Array.toList
                    |> Expect.equal [ 3, 4, 5 ]
            )
        , test "dropWhen"
            (\() ->
                Arr.dropWhen (\n -> n < 3)
                    (Arr.from5 1 2 3 4 5)
                    |> Arr.toArray
                    |> Array.toList
                    |> Expect.equal [ 3, 4, 5 ]
            )
        ]


inArrTests : Test
inArrTests =
    describe "InArr"
        [ test "extend"
            (\() ->
                Arr.from3 1 1 1
                    |> InArr.extend nat3 (Arr.from3 0 0 0)
                    |> Arr.toArray
                    |> Expect.equal
                        ([ 1, 1, 1, 0, 0, 0 ]
                            |> Array.fromList
                        )
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


num1234 : Arr (In Nat4 (Nat4Plus a)) number
num1234 =
    Arr.from4 1 2 3 4


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
        |> InArr.extend nat4
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


type alias TicTacToeBoard =
    Arr
        (Only Nat3)
        (Arr (Only Nat3) TicTacToeField)


initialTicTacToeBoard : TicTacToeBoard
initialTicTacToeBoard =
    Arr.repeat nat3
        (Arr.repeat nat3 FieldEmpty)


type TicTacToeField
    = FieldEmpty
    | X
    | O
