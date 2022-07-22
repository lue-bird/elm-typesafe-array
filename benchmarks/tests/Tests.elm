module Tests exposing (..)

import Array
import Candidates exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "candidates"
        [ describe "allFill"
            [ test "areAllFilledWithListCons"
                (\() ->
                    areAllFilledWithListCons
                        (Array.fromList [ Just 1, Just 2, Just 3 ])
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3 ] |> Just)
                )
            , test "areAllFilledWithArrayPush"
                (\() ->
                    areAllFilledWithArrayPush
                        (Array.fromList [ Just 1, Just 2, Just 3 ])
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3 ] |> Just)
                )
            ]
        ]
