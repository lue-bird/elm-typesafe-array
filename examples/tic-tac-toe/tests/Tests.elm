module Tests exposing (..)

import Arr
import Expect
import Main exposing (Field(..), GameOver(..), Player(..), isGameOver)
import Nats exposing (nat3)
import Test exposing (..)


suite : Test
suite =
    describe "isGameOver"
        [ test "same in a row"
            (\() ->
                isGameOver
                    (Arr.from3
                        (Arr.repeat nat3 FieldNotSet)
                        (Arr.repeat nat3 (FieldSet O))
                        (Arr.repeat nat3 FieldNotSet)
                    )
                    |> Expect.equal (Just (PlayerWon O))
            )
        , test "same in a column"
            (\() ->
                isGameOver
                    (Arr.repeat nat3
                        (Arr.from3 FieldNotSet (FieldSet O) FieldNotSet)
                    )
                    |> Expect.equal (Just (PlayerWon O))
            )
        , describe "same diagonally"
            [ test "top-left to bottom-right"
                (\() ->
                    isGameOver
                        (Arr.from3
                            (Arr.from3 (FieldSet O) FieldNotSet FieldNotSet)
                            (Arr.from3 FieldNotSet (FieldSet O) FieldNotSet)
                            (Arr.from3 FieldNotSet FieldNotSet (FieldSet O))
                        )
                        |> Expect.equal (Just (PlayerWon O))
                )
            , test "top-right to bottom-left"
                (\() ->
                    isGameOver
                        (Arr.from3
                            (Arr.from3 FieldNotSet FieldNotSet (FieldSet O))
                            (Arr.from3 FieldNotSet (FieldSet O) FieldNotSet)
                            (Arr.from3 (FieldSet O) FieldNotSet FieldNotSet)
                        )
                        |> Expect.equal (Just (PlayerWon O))
                )
            ]
        , test "draw"
            (\() ->
                isGameOver
                    (Arr.from3
                        (Arr.from3 (FieldSet O) (FieldSet X) (FieldSet O))
                        (Arr.from3 (FieldSet X) (FieldSet X) (FieldSet O))
                        (Arr.from3 (FieldSet O) (FieldSet O) (FieldSet X))
                    )
                    |> Expect.equal (Just Draw)
            )
        ]
