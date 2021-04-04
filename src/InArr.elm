module InArr exposing
    ( push, extend, extendN, removeAt
    , dropN
    , toMin
    , insertAt
    )

{-| A `Array (In ...)` describes an array where you know a minimum & maximum amount of elements.

    Array.empty |> Array.get 0
    --> Nothing

    InArr.empty |> InArr.at (nat0 |> NNat.toIn) FirstToLast
    --> compile time error

Is this any useful? Let's look at an example:

> 0 to 100 joined by a space

    joinBy between =
        \before after-> before ++ between ++ after

    let
        intStringsAfter =
            Array.fromList (List.range 1 100)
                |> Array.map String.fromInt
    in
    Array.foldl (joinBy " ") "0" intStringsAfter
    --> "0 1 2 3 4 5 ..."

    InArr.range ( nat0, nat0 ) (nat100 |> NNat.toIn)
        |> InArr.map (InNat.toInt >> String.fromInt)
        |> InArr.foldWithFirst (joinBy " ")
    --> "0 1 2 3 4 5 ..."


## modify

@docs push, extend, extendN, removeAt, replaceAt, take


## part

@docs takeIn, takeN, dropN


## drop information

@docs toMin


## create

@docs range

-}

import Arr exposing (Arr)
import Internal.InArr as Internal
import LinearDirection exposing (LinearDirection(..))
import NNats exposing (..)
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import TypeNats exposing (..)



-- ## modify


{-| Use `MinArr.push`, if the `max` isn't known to be a certain value.
-}
push :
    element
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.push element


{-| Use `MinArr.push`, if the `max` isn't known to be a certain value.
-}
insertAt :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max maybeN) element
    -> Arr (ValueIn (Nat2Plus minMinus1) (Nat1Plus max)) element
insertAt index direction element =
    Internal.insertAt index direction element


{-| Append the elements in another `Arr (In ...)`.
-}
extend :
    Arr (In extensionMin extensionMax extensionMaybeN) element
    -> Nat (N extensionMin (Is min To extendedMin) x)
    -> Nat (N extensionMax (Is max To extendedMax) y)
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn extendedMin extendedMax) element
extend extension extensionMin extensionMax =
    Internal.extend extension extensionMin extensionMax


extendN :
    Arr (N added (Is min To sumMin) (Is max To sumMax)) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn sumMin sumMax) element
extendN nArrayExtension =
    Internal.extendN nArrayExtension


removeAt :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1) maybeN) element
    -> Arr (ValueIn minMinus1 maxMinus1) element
removeAt index direction =
    Internal.removeAt index direction



-- ## part


dropN :
    Nat (N dropped (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn minTaken maxTaken) element
dropN droppedAmount direction =
    Internal.dropN droppedAmount direction



-- ## drop information


toMin : Arr (In min max maybeN) element -> Arr (ValueMin min) element
toMin =
    Internal.toMin
