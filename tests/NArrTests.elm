module NArrTests exposing (startBoard)

import Arr exposing (Arr)
import Expect
import NArr
import NNats exposing (..)
import Nat exposing (Only, ValueOnly)
import Test exposing (Test, describe, test)
import TypeNats exposing (..)


suite : Test
suite =
    describe "NArr"
        []


startBoard : Arr (ValueOnly Nat8) (Arr (ValueOnly Nat8) Field)
startBoard =
    let
        emptyRow =
            Arr.repeat nat8 Empty

        pawnRow color =
            Arr.repeat nat8 (Piece Pawn color)

        firstRow color =
            Arr.repeat nat8 (Piece Other color)
    in
    Arr.empty
        |> NArr.push (firstRow White)
        |> NArr.push (pawnRow White)
        |> NArr.extend nat4 (Arr.repeat nat4 emptyRow)
        |> NArr.push (pawnRow Black)
        |> NArr.push (firstRow Black)
        |> NArr.toIn
        |> Arr.map NArr.toIn


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
        (ValueOnly Nat3)
        (Arr (ValueOnly Nat3) TicTacToeField)


initialTicTacToeBoard : TicTacToeBoard
initialTicTacToeBoard =
    Arr.repeat nat3
        (Arr.repeat nat3 FieldEmpty |> NArr.toIn)
        |> NArr.toIn


type TicTacToeField
    = FieldEmpty
    | X
    | O
