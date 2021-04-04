module OnlyArr exposing (serialize)

{-|


## extra

@docs serialize

-}

import Arr exposing (Arr, SerializeError(..), toArray)
import MinArr
import NNats exposing (nat0)
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import Serialize
import TypeNats exposing (..)


{-| A [Codec](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/Serialize#Codec)
to serialize `Arr (Min ...)`s.
-}
serialize :
    Nat
        (N
            (Nat1Plus minMinus1)
            (Is (Nat1Plus aMinus1) To)
            (Nat1Plus minMinus1PlusA)
            x
        )
    -> Serialize.Codec elementError element
    ->
        Serialize.Codec
            (SerializeError elementError)
            (Arr
                (N
                    (Nat1Plus minMinus1)
                    (Is (Nat1Plus aMinus1) To)
                    (Nat1Plus minMinus1PlusA)
                    x
                )
                element
            )
serialize amount serializeElement =
    Serialize.array
        (serializeElement
            |> Serialize.mapError ElementSerializeError
        )
        |> Serialize.mapValid
            (Arr.fromArray
                >> MinArr.isLength amount
                    { min = nat0 }
                    { less = \_ -> Nothing
                    , greater = \_ -> Nothing
                    , equal = Just
                    }
                >> Result.fromMaybe WrongAmountSerializeError
            )
            toArray
