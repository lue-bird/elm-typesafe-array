module Tests exposing (..)

import Array
import Candidates exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)

import Candidates exposing (..)


suite : Test
suite =
    describe "candidates"
        [ describe "whenAllJust"
            [ test "whenAllJustWithListCons"
                (\() ->
                    whenAllJustWithListCons
                        (Array.fromList [ Just 1, Just 2, Just 3 ])
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3 ] |> Just)
                )
            , test "whenAllJustWithArrayPush"
                (\() ->
                    whenAllJustWithArrayPush
                        (Array.fromList [ Just 1, Just 2, Just 3 ])
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3 ] |> Just)
                )
            ]
        ]
