module Tests exposing (..)

import App exposing (Field(..), GameOver(..), Player(..), isGameOver)
import ArraySized
import Expect
import N exposing (n3)
import Test exposing (..)


suite : Test
suite =
    describe "isGameOver"
        [ test "same in a row"
            (\() ->
                isGameOver
                    (ArraySized.l3
                        (ArraySized.repeat FieldNotSet n3)
                        (ArraySized.repeat (FieldSet O) n3)
                        (ArraySized.repeat FieldNotSet n3)
                    )
                    |> Expect.equal (Just (PlayerWon O))
            )
        , test "same in a column"
            (\() ->
                isGameOver
                    (ArraySized.repeat
                        (ArraySized.l3 FieldNotSet (FieldSet O) FieldNotSet)
                        n3
                    )
                    |> Expect.equal (Just (PlayerWon O))
            )
        , describe "same diagonally"
            [ test "top-left to bottom-right"
                (\() ->
                    isGameOver
                        (ArraySized.l3
                            (ArraySized.l3 (FieldSet O) FieldNotSet FieldNotSet)
                            (ArraySized.l3 FieldNotSet (FieldSet O) FieldNotSet)
                            (ArraySized.l3 FieldNotSet FieldNotSet (FieldSet O))
                        )
                        |> Expect.equal (Just (PlayerWon O))
                )
            , test "top-right to bottom-left"
                (\() ->
                    isGameOver
                        (ArraySized.l3
                            (ArraySized.l3 FieldNotSet FieldNotSet (FieldSet O))
                            (ArraySized.l3 FieldNotSet (FieldSet O) FieldNotSet)
                            (ArraySized.l3 (FieldSet O) FieldNotSet FieldNotSet)
                        )
                        |> Expect.equal (Just (PlayerWon O))
                )
            ]
        , test "draw"
            (\() ->
                isGameOver
                    (ArraySized.l3
                        (ArraySized.l3 (FieldSet O) (FieldSet X) (FieldSet O))
                        (ArraySized.l3 (FieldSet X) (FieldSet X) (FieldSet O))
                        (ArraySized.l3 (FieldSet O) (FieldSet O) (FieldSet X))
                    )
                    |> Expect.equal (Just Draw)
            )
        ]
