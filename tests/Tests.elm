module Tests exposing (startBoard, suite)

import Arr exposing (Arr)
import Array
import Expect
import InArr
import NNats exposing (..)
import Nat exposing (Only)
import Test exposing (Test, describe, test)
import TypeNats exposing (..)


suite : Test
suite =
    describe "InArr"
        [ test "extendOnly"
            (\() ->
                Arr.from3 1 1 1
                    |> InArr.extendOnly nat3 (Arr.from3 0 0 0)
                    |> Arr.toArray
                    |> Expect.equal
                        ([ 1, 1, 1, 0, 0, 0 ]
                            |> Array.fromList
                        )
            )
        ]


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
        |> InArr.extendOnly nat4
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
