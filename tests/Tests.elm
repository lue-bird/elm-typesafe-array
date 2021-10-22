module Tests exposing (startBoard, suite)

import Arr exposing (Arr)
import Expect exposing (Expectation)
import InArr
import LinearDirection exposing (LinearDirection(..))
import MinArr
import Nat exposing (In, Min, Only)
import Nats exposing (..)
import Test exposing (Test, describe, test)


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
        , describe "all"
            [ test "True"
                (\() ->
                    Arr.all isEven (Arr.from2 2 4)
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    Arr.all isEven (Arr.from2 2 3)
                        |> Expect.equal False
                )
            ]
        , describe "any"
            [ test "True"
                (\() ->
                    Arr.any isEven (Arr.from2 1 2)
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    Arr.any isEven (Arr.from2 1 3)
                        |> Expect.equal False
                )
            ]
        , test "errorToString"
            (\() ->
                { expected = Arr.ExpectLength (Nat.intAtLeast nat0 11)
                , actual = { length = Nat.intAtLeast nat0 10 }
                }
                    |> Arr.errorToString
                    |> Expect.equal
                        "Expected an array of length 11, but the actual length was 10."
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
                    |> Expect.equalLists
                        [ 1, 1, 1, 0, 0, 0 ]
            )
        , test "prepend"
            (\() ->
                Arr.from3 1 1 1
                    |> InArr.prepend nat3 (Arr.from3 0 0 0)
                    |> Arr.toList
                    |> Expect.equalLists
                        [ 0, 0, 0, 1, 1, 1 ]
            )
        , describe "resize"
            [ describe "FirstToLast"
                [ test "length less than current"
                    (\() ->
                        Arr.resize FirstToLast nat3 0 num1234
                            |> expectEqualArr
                                (Arr.from3 1 2 3)
                    )
                , test "length greater than current"
                    (\() ->
                        Arr.resize FirstToLast nat6 0 num1234
                            |> expectEqualArr
                                (Arr.from6 1 2 3 4 0 0)
                    )
                ]
            , describe "LastToFirst"
                [ test "length less than current"
                    (\() ->
                        Arr.resize LastToFirst nat3 0 num1234
                            |> expectEqualArr
                                (Arr.from3 2 3 4)
                    )
                , test "length greater than current"
                    (\() ->
                        Arr.resize LastToFirst nat6 0 num1234
                            |> expectEqualArr
                                (Arr.from6 0 0 1 2 3 4)
                    )
                ]
            ]
        , describe "whenAllJust"
            [ test "all are Just"
                (\() ->
                    Arr.from3 (Just 1) (Just 2) (Just 3)
                        |> Arr.whenAllJust
                        |> Maybe.map
                            (Arr.toList >> Expect.equalLists [ 1, 2, 3 ])
                        |> Maybe.withDefault
                            (Expect.fail "was Nothing, expected Just")
                )
            , test "one is Nothing"
                (\() ->
                    Arr.from3 (Just 1) Nothing (Just 3)
                        |> Arr.whenAllJust
                        |> Expect.equal Nothing
                )
            ]
        , test "intersperse"
            (\() ->
                Arr.from3 "turtles" "turtles" "turtles"
                    |> InArr.intersperse "on" nat3 nat3
                    |> expectEqualArr
                        (Arr.from5 "turtles" "on" "turtles" "on" "turtles")
            )
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


minCons :
    element
    -> Arr (In minLength maxLength_) element
    -> Arr (Min (Nat1Plus minLength)) element
minCons =
    MinArr.insertAt nat0 FirstToLast


inCons :
    element
    -> Arr (In minLength maxLength) element
    -> Arr (In (Nat1Plus minLength) (Nat1Plus maxLength)) element
inCons =
    InArr.insertAt nat0 FirstToLast


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



-- ↓ not important


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0


expectEqualArr : Arr l a -> Arr l a -> Expectation
expectEqualArr expected actual =
    Expect.equalLists
        (expected |> Arr.toList)
        (actual |> Arr.toList)
